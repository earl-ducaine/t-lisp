(herald simplify
        (syntax-table *orbit-syntax-table*))

;;;  Optimization of CPS code tree

;;; Post-CPS code has these properties:
;;;   For every LAMBDA node L:
;;;     - L's body is a call.
;;;     - L's parent is a call, or else L is the top of the tree.
;;;   For every call node N:
;;;     - N's procedure and arguments are all non-calls.
;;;     - N's parent is a LAMBDA.

;;; (SIMPLIFY node)
;;;============================================================================
;;;   Post-CPS optimizer.  All simplifications are done by changing the
;;; structure of the node tree.
;;;
;;; Simplification of a node is done in the following order:
;;;   Presimplification - Certain simplifications are done before any of the
;;;                       child nodes are simplified.  See PRESIMPLIFY.T
;;;   Non-exit children - The children that are not exits are simplified.
;;;   Node              - The node itself is simplified.
;;;   Exit children     - If the node is call node any exit children are
;;;                       simplified.
;;; If any of these result in a change to the tree, the process begins again
;;; with the (possibly new) node occupying the position being simplified.
;;;
;;; There are three requirements for the simplification procedures:
;;;    1) They must return T if the tree has been changed and NIL otherwise.
;;;    2) The ancestors of the node being simplified  must not be changed.
;;;    3) If a node is changed the NODE-SIMPLIFIED? flag of that node and all
;;;       its ancestors must be set to NIL.

(define (simplify node)
  (cond ((node-simplified? node)
         node)
        (else
         (iterate loop ((node node))
           (let ((parent (node-parent node))
                 (role (node-role node)))
             (cond ((or (presimplify node)
                        (simplify-non-exit-children node)
                        (really-simplify node)
                        (and (call-node? node)
                             (or (simplify-exit-children node)
                                 (really-simplify node))))
                    (loop (role parent)))
                   (else
                    (set (node-simplified? node) t)
                    node)))))))

;;; (REALLY-SIMPLIFY node)
;;;===========================================================================
;;;   Dispatch on the type of the node.

(define (really-simplify node)
  (xselect (node-variant node)
    ((leaf-node?)
     (simplify-leaf node))
    ((lambda-node?)
     (simplify-lambda node))
    ((call-node?)
     (simplify-call node))))

;;; (SIMPLIFY-NON-EXIT-CHILDREN node)
;;; (SIMPLIFY-EXIT-CHILDREN node)
;;;=========================================================================
;;;  Simplify the specified children.  These use the NODE-SIMPLIFIED? flag
;;; to determine if a change has been made.

(define (simplify-non-exit-children node)
  (set (node-simplified? node) t)
  (xselect (node-variant node)
    ((leaf-node?)
     nil)
    ((lambda-node?)
     (simplify (lambda-body node)))
    ((call-node?)
     (cond ((lambda-node? (call-proc node))
            (walk simplify (call-args node)))
           (else
            (walk simplify (nthcdr (call-args node) (call-exits node)))
            (simplify (call-proc node))))))
  (not (node-simplified? node)))

(define (simplify-exit-children node)
  (set (node-simplified? node) t)
  (cond ((lambda-node? (call-proc node))
         (simplify (call-proc node)))
        (else
         (do ((i (call-exits node) (fx- i 1))
              (args (call-args node) (cdr args)))
             ((fx<= i 0))
           (simplify (car args)))))
  (not (node-simplified? node)))

;;; (SIMPLIFY-LEAF node)
;;;==========================================================================
;;;   Leaf nodes are simplified only if they are variables with early binding
;;; information.

(define (simplify-leaf node)
  (cond ((and (reference-node? node)
              (supported? (reference-variable node)))
         (simplify-using-support node))
        (else
         nil)))

;;; (SIMPLIFY-USING-SUPPORT node)
;;;============================================================================
;;;    NODE is a reference to a variable with early binding.  Integegrate if
;;; possible, otherwise nothing.

(define (simplify-using-support node)
  (let* ((variable (reference-variable node))
         (support (variable-support variable)))
    (cond ((nonvalue-reference? node)
           nil)
          ((integrable-support node variable support)
           =>(lambda (original)
               (integrate-support node original variable)
               t))
          (else   ; Will eventually do type checking here
           nil))))

;;; (INTEGRABLE-SUPPORT node variable support)
;;;============================================================================
;;;    NODE is a reference to VARIABLE which has early binding information.
;;; Calls INTEGRABLE-VALUE to determine if there is a value to integrate.
;;; Returns the value if it is appropriate to integrate it.

(define (integrable-support node variable support)
  (let ((value (integrable-value node variable support)))
    (cond ((not value)
           nil)
          ((or (literal-node? value)
               (and (reference-node? value)
                    (not (variable-binder (reference-variable value)))))
           value)
          ((primop-node? value)
           (if (primop.integrate? (primop-value value) node)
               value
               nil))
          ((lambda-node? value)
           (if (eq? (node-role node) call-proc)
               value
               nil))
          (else
           nil))))

;;; (INTEGRABLE-VALUE node variable support)
;;;============================================================================
;;;    Supposedly constant values are returned only if they actually are
;;; constant.

(define (integrable-value node variable support)
  (let ((variant (support.variant support)))
    (cond ((eq? variant support/constant)
           (let ((value (support.value support)))
             (if (suitable-constant-support? value)
                 value
                 nil)))
          ((or (eq? variant support/integrable)
               (eq? variant support/wired))
           (support.value support))
          (else      ; Can this happen?
           nil))))


;;; (INTEGRATE-SUPPORT node original variable)
;;;============================================================================
;;;    Replace NODE with a copy of ORIGINAL.

(define (integrate-support node original variable)
  (orbit-debug "integrating: ~A~%" (variable-name variable))
  (replace node (copy-node-tree original '())))


;;; (SIMPLIFY-LAMBDA node)
;;;============================================================================
;;;     Simplify a lambda node.
;;; (lambda () (x)) => x if the node is an exit.

(define (simplify-lambda node)
  (cond ((and (call-exit? node)
              (null? (lambda-rest-var node))
              (null? (lambda-variables node))
              (null? (call-args (lambda-body node))))
         (replace node (detach (call-proc (lambda-body node))))
         t)
        (else
         nil)))

;;; (SIMPLIFY-CALL node)
;;;============================================================================
;;;     Simplify a call node.
;;; Calls to literals are flushed.  Primops are simplified using there own
;;; methods.  Calls to objects are simplified (the handler is flushed).
;;; If the second argument is a reference to a known object operation dispatch
;;; will be attempted.

(define (simplify-call node)
  (let ((proc (call-proc node)))
    (xselect (node-variant proc)
      ((lambda-node?)
       nil)
      ((leaf-node?)
       (cond ((literal-node? proc)
              (warning "literal in procedure position - ~S~%" node)
              (replace proc (create-primop-node primop/undefined-effect))
              t)
             ((known-primop proc)
              => (lambda (primop)
                   (if (and (primop.formals primop)
                            (not (primop.constructed? primop)))
                       (simplify-parameterized-primop primop node)
                       (primop.simplify primop node))))
             ((known-object-lambda proc)
              => (lambda (obj-lambda)
                   (simplify-called-object proc obj-lambda)))
             ((and (cdr (call-args node))
                   (reference-node? ((call-arg 2) node))
                   (known-object-lambda ((call-arg 2) node)))
              => (lambda (obj-lambda)
                   (simplify-operation-dispatch node obj-lambda)))
             (else
              nil))))))

;;; (SIMPLIFY-PARAMETERIZED-PRIMOP node primop)
;;;============================================================================
;;;  CALL is a call node whose procedure is PRIMOP.  Checks to see that there
;;; are the right number of arguments and that they are all literals.  If so,
;;; the values are attached the primop and declared as support for any
;;; appropriate variables.  This works for the one necessary (simple) case.
;;; Anything fancier will probably lose.  The problem is to simplify the
;;; support for a variable without simplifying the original definition.

(define (simplify-parameterized-primop primop call)
  (cond ((and (fx= (length (primop.formals primop)) ; Could be improper?
                   (fx+ -1 (length (call-args call))))
              (every? literal-node? (cdr (call-args call)))
              (lambda-node? ((call-arg 1) call))
              (unbound-support-refs (lambda-cont-var ((call-arg 1) call))))
         =>(lambda (refs)
             (let* ((arglist (map literal-value (cdr (call-args call))))
                    (new-primop (join (object nil
                                        ((primop.constructed? self) t)
                                        ((primop.arglist self) arglist))
                                      primop))
                    (new (create-primop-node new-primop)))
               (walk (lambda (ref)      ; Multiple definition problems?
                       (change-support-value (node-parent ref) new))
                     refs)))))
  nil)

;;; (OBJECT-LAMBDA? node)
;;;=========================================================================
;;;  Is the lambda node really an object?

(define (object-lambda? node)
  (and (lambda-node? node)
       (or (primop-ref? (call-proc (lambda-body node)) primop/handler)
           (primop-ref? (call-proc (lambda-body node)) primop/proc+handler))))

;;; (SIMPLIFY-CALLED-OBJECT? node value)
;;;=========================================================================
;;;  Replace the object with its procedure.

(define (simplify-called-object node value)
  (cond ((primop-ref? (call-proc (lambda-body value)) primop/handler)
         (warning "calling an object with no procedure definition ~S" value)
         nil)
        (else
         (let ((proc ((call-arg 2) (lambda-body value))))
           (replace node (if (reference-node? node)
                             (copy-node-tree proc '())
                             (detach proc)))
           t))))

;;; (SIMPLIFY-OPERATION-DISPATCH call obj)
;;;=========================================================================
;;;  OBJ is an object-lambda.  The methods are searched to see if there is
;;; one corresponding to the procedure being called.  If so, the method is
;;; integrated.

(define (simplify-operation-dispatch call obj)
  (let ((var (reference-variable (call-proc call)))
        (obj-body (lambda-body obj)))
    (iterate loop ((ops (call-args (lambda-body ((call-arg 3) obj-body))))
                   (methods (cdddr (call-args obj-body))))
      (cond ((null? ops)
             nil)
            ((eq? var (reference-variable (car ops)))
             (replace-operation-with-method call (car methods)))
            (else
             (loop (cdr ops) (cdr methods)))))))

;;; (REPLACE-OPERATION-WITH-METHOD call method)
;;;=========================================================================
;;;  (<op> <cont> <object> . <args>) =>
;;;  (<method> <cont> <object> <op> *the-buck-stops-here* <object> . <args>)
;;; where <method> is <object>'s method for <op>.  <object> is known to be a
;;; reference node (so copying it isn't a problem).

(define (replace-operation-with-method call method)
  (let ((new (create-call-node (fx+ 4 (length (call-args call))) 1)))
    (relate call-proc new (copy-node-tree method '()))
    (relate-call-args new `(,(detach ((call-arg 1) call))
                            ,(copy-node-tree ((call-arg 2) call) '())
                            ,(detach (call-proc call))
                            ,(create-nonlocal-reference '*the-buck-stops-here*
                                                        *standard-support-env*)
                            . ,(map detach (cdr (call-args call)))))
    (replace call new)
    t))

;;; (UNBOUND-SUPPORT-REFS var)
;;;=========================================================================
;;;  Returns a list of the references to VAR that support a definition where
;;; the variable being defined does not yet have a support value.

(define (unbound-support-refs var)
  (do ((refs (variable-refs var) (cdr refs))
       (res '() (if (and (supports-definition? (car refs))
                         (not (primop? (support.value
                                        (variable-support
                                         (reference-variable
                                          ((call-arg 2)
                                           (node-parent (car refs)))))))))
                    (cons (car refs) res)
                    res)))
      ((null? refs)
       res)))

;;; (CHANGE-SUPPORT-VALUE call value)
;;;=========================================================================
;;;   Change the support generated by CALL to have value VALUE.  Resimplify
;;; any references to the supported variable.

(define (change-support-value call value)
  (let ((var (reference-variable ((call-arg 2) call)))
        (variant (primop.support-variant (primop-value (call-proc call)))))
    (add-support-value var variant value)
    (walk simplify-using-support
          (variable-refs var))))

;;; (REPLACE-CALL-WITH-VALUE call value)
;;;============================================================================
;;;   Replaces the call node CALL with VALUE.
;;; (<proc> <exit> . <args>) => (<exit> <value>)

(define (replace-call-with-value call value)
  (cond ((fxn= 1 (call-exits call))
         (bug "can only substitute for call with one exit ~S" call))
        (else
         (let ((cont (detach ((call-arg 1) call))))
           (walk (compose erase-all detach)
                 (cdr (call-args call)))
           (replace (call-proc call) cont)
           (relate-shorter-call-args call (if value `(,value) '()))))))

;;; (KNOWN-PRIMOP node)
;;;============================================================================
;;;   If NODE is a primop node or a variable node for a variable whose value
;;; is a primop, the primop is returned.

(define (known-primop node)
  (cond ((primop-node? node)
         (primop-value node))
        ((and (reference-node? node)
              (supported? (reference-variable node)))
         (let ((value
                (support.value (variable-support (reference-variable node)))))
           (if (and value (primop-node? value))
               (primop-value value)
               nil)))
        (else
         nil)))

;;; (KNOWN-OBJECT-LAMBDA node)
;;;============================================================================
;;;   If NODE is an object-lambda node or a variable node for a variable whose
;;; value is an object-lambda, the object-lambda is returned.

(define (known-object-lambda node)
  (cond ((object-lambda? node)
         node)
        ((and (reference-node? node)
              (supported? (reference-variable node)))
         (let ((value
                (support.value (variable-support (reference-variable node)))))
           (if (and value (object-lambda? value))
               value
               nil)))
        (else
         nil)))

;;; (VARIABLE-KNOWN var)
;;;============================================================================
;;; Returns label procedure for this variable.  This depend on LET lambdas
;;; setting the type fields of their arguments.

(define (variable-known var)
  (let ((type (variable-type var)))
    (cond ((and (node? type)
                (lambda-node? type))
                ;; ((lambda (var) ...) type)  - fix later for labels
                ;(eq? (node-parent (variable-binder var))
                ;     (node-parent type))
           type)
          (else nil))))

;;; (ALL-REFS-ARE-CALLS? var)
;;;============================================================================
;;; If true, then the LAMBDA to which this variable is being bound
;;; can always be jumped to (although an environment adjustment may
;;; be needed).

(define (all-refs-are-calls? var)               
  (every? (lambda (ref)
            (eq? (node-role ref) call-proc))
          (variable-refs var)))

;;; (ERASE-UNUSED-ARGUMENTS call-node in-vars)
;;;============================================================================
;;; Flush ignored arguments from calls to a known procedure.

(define (erase-unused-arguments call-node in-vars)
  (iterate loop ((vars in-vars)
                 (args (call-args call-node))
                 (new-args '()))
    (cond ((null? vars)
           (cond ((fx= (length in-vars) (length new-args))
                  nil)
                 (else
                  (relate-shorter-call-args call-node
                                            (reverse! (map! detach new-args)))
                  t)))
          ((not (used? (car vars)))
           (if (not (empty? (car args)))
               (erase-all (detach (car args))))
           (loop (cdr vars)
                 (cdr args)
                 new-args))
          (else
           (loop (cdr vars)
                 (cdr args)
                 (cons (car args) new-args))))))

;;; (REMOVE-UNREFERENCED-VARIABLES node)
;;;============================================================================
;;;   Remove any unreferenced variables from a lambda node.  The above
;;; procedure has already been called to remove the arguments the variables
;;; were bound to.

(define (remove-unreferenced-variables node)
  (iterate loop ((vars (lambda-rest+variables node)) (n 2) (changed? nil))
    (cond ((null? (cdr vars))
           changed?)
          ((used? (cadr vars))
           (set (variable-number (cadr vars)) n)
           (loop (cdr vars) (fx+ n 1) nil))
          (else
           (set (cdr vars) (cddr vars))
           (loop vars n t)))))

;;; (SUBSTITUTE var val detach?)
;;;============================================================================
;;;   Substitute VAL for VAR.  If DETACH? is true then VAL should be detached
;;; and so can be used instead of a copy for the first substitution.

(define (substitute var val detach?)
  (let ((refs (copy-list (variable-refs var))))
    (orbit-debug "substituting: ~A := ~A~%" var (pp-cps-2 val))
    (if (and (reference-node? val)               ;Keep LET variable names
             (eq? 'k (variable-name (reference-variable val))))
        (set (variable-name (reference-variable val))
             (variable-name var)))
    (cond (refs
           (walk (lambda (ref)
                   (replace ref (copy-node-tree val '())))
                 (if detach? (cdr refs) refs))
           (if detach? (replace (car refs) (detach val))))
          (detach?
           (erase-all (detach val))))))

;;; (REPLACE-VAR var val body detach?)
;;;============================================================================
;;;   This is SUBSTITUTE with the guarentee that VAL will not be copied.
;;; <body> =>
;;; ((lambda (v)
;;;    <body>[VAR replaced by V])
;;;  VAL)
;;; If VAL is a reference node or VAR has only one reference then SUBSTITUTE is
;;; called instead.

(define (replace-var var val body detach?)
  (cond ((or (reference-node? val)
             (null? (cdr (variable-refs var))))
         (substitute var val detach?))
        (else
         (let* ((new-var (create-variable 'v))
                (ref (create-reference-node new-var))
                (l-node (create-lambda-node 'p `(,new-var)))
                (new-body (create-call-node 2 0)))
           (substitute var ref nil)
           (erase ref)
           (relate call-proc new-body l-node)
           (relate (call-arg 1) new-body (if detach?
                                             (detach val)
                                             (copy-node-tree val '())))
           (move body
                 (lambda (node)
                   (relate lambda-body l-node node)
                   new-body))))))

;;; (NONVALUE-REFERENCE? ref)
;;;============================================================================
;;;  Returns T if REF is being referred to as an L-value.

(define (nonvalue-reference? ref)
  (and (eq? (node-role ref) (call-arg 2))
       (primop-node? (call-proc (node-parent ref)))
       (primop.uses-L-value? (primop-value (call-proc (node-parent ref))))))

;;; (CREATE-NONLOCAL-REFERENCE name env)
;;;============================================================================
;;;  Return a reference node to the variable bound to NAME in support
;;; environment ENV.

(define (create-nonlocal-reference name env)
  (let ((support (env name)))
    (cond (support
;;; Need to define the following...sometime.  Currently this reference is
;;;   removed by simplify.
;;;        (add-to-early-bound-vars (support.variable support))
           (create-reference-node (support.variable support)))
          (else
           (bug "need reference to ~S in ~S and can't find it" name env)))))

;;; (PRIMOP-LOOKUP name env)
;;;============================================================================
;;;   Find the primop with id NAME in support environment ENV.  Either this
;;; or the previous procedure should reverse its argument order.

(define (primop-lookup name env)
  (let ((primop (primop-support env name)))
    (cond (primop
           (create-primop-node primop))
          (else
           (bug "need primop ~S in ~S and can't find it" name env)))))
