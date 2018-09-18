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
;;;   Post-CPS optimizer.
;;; Walks the tree bottom up.  Each node is marked when it is simplified.  If
;;; a node is changed all of it's ancestors are unmarked.  Thus they all
;;; for possible resimplification.  Not all of the procedures defined here are
;;; explicitly called from this file.  Some are called by primops in response
;;; to PRIMOP.SIMPLIFY.

(define (simplify node)
  (iterate loop ((node node))
    (cond ((node-simplified? node) node)
          (else
           (walk simplify (node-children node))      ; bottom-up
           (set (node-simplified? node) t)
           (let ((parent (node-parent node))
                 (role (node-role node)))
             (xselect (node-variant node)
               ((leaf-node?)
                (simplify-leaf node))
               ((lambda-node?)
                (simplify-lambda node))
               ((call-node?)
                (simplify-call node)))
             (if parent (loop (role parent)) node))))))

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
;;;    NODE is a reference to a variable with early binding.

(define (simplify-using-support node)
  (let* ((variable (reference-variable node))
         (support (variable-support variable)))
    (cond ((nonvalue-reference? node)
           nil)
          ((integrable-support? node variable support)
           =>(lambda (original)
               (integrate-support node original variable)))
          (else   ; Will eventually do type checking here
           nil))))

;;; (INTEGRABLE-SUPPORT node variable support)
;;;============================================================================
;;;    NODE is a reference to VARIABLE which has early binding.  Integration
;;; is not done if this is a reference to VARIABLE as an L-value, or if it
;;; would cause a recursive integration.  Variables defined as constant are
;;; integrated only if the definition is actually a constant.

(define (integrable-support? node variable support)
  (let ((value (integrable-value node variable support)))
    (if (or (not value)
            (and (lambda-node? value)
                 (neq? (node-role node) call-proc))
            (and (primop-node? value)
                 (not (primop.integrate? (primop-value value) node))))
        nil
        value)))

(define (integrable-value node variable support)
  (let ((variant (support.variant support)))
    (cond ((memq? variable (reference-copy-sources node))
           (warning "recursive integrable call to ~S" (variable-name variable))
           nil)
          ((eq? variant support/constant)
           (let ((value (support.value support)))
             (if (suitable-constant-support? value)
                 value
                 nil)))
          ((or (eq? variant support/integrable)
               (eq? variant support/wired))
           (support.value support))
          (else
           nil))))

;;; (INTEGRATE-SUPPORT node original variable)
;;;============================================================================
;;;    Replace NODE with a copy of ORIGINAL.

(define (integrate-support node original variable)
  (noise "integrating: ~A~%" (variable-name variable))
  (replace node
           (lambda (old)
             (erase old)
             (copy-node-tree original '() variable))))


;;; (SIMPLIFY-LAMBDA node)
;;;============================================================================
;;;     Simplify a lambda node.  This just flushes any unused variables.

(define (simplify-lambda node)
  (cond ((and (call-exit? node)
              (null? (cdr (lambda-variables node)))
              (null? (call-args (lambda-body node))))
         (replace node
                  (lambda (old)
                    (let ((new (detach (call-proc (lambda-body old)))))
                      (erase old)
                      new))))
        (else
         (map! (lambda (var)
                 (cond ((and (used? var)
                             (null? (variable-refs var)))
                        nil)
                       (else
                        var)))
               (cdr (lambda-variables node))))))

;;; (SIMPLIFY-CALL node)
;;;============================================================================
;;;     Simplify a call node.

(define (simplify-call node)
  (let ((proc (call-proc node)))
    (select (node-variant proc)
      ((lambda-node?)
       (simplify-let node))
      ((leaf-node?)
       (cond ((literal-node? proc)
              (warning "literal in procedure position - ~S~%" node)
              (replace proc
                       (lambda (old)
                         (ignore old)
                         (create-primop-node primop/undefined-effect))))
             ((known-primop-value proc)
              => (lambda (primop)
                   (if (and (primop.formals primop)
                            (not (primop.constructed? primop)))
                       (simplify-parameterized-primop proc primop)
                       (primop.simplify primop node))))
             (else
              nil))))))

(define (simplify-test node)
  (destructure (((exit-1 exit-2 #f test val) (call-args node)))
    (cond ((not (and (primop-ref? test primop/true?)
                     (literal-node? val)))
           nil)
          ((eq? '#f (primop-value val))
           (replace-test node exit-2))
          (else
           (replace-test node exit-1)))))

(define (replace-test call-node new-node)
  (let ((new-call (create-call-node 1 0)))
    (detach new-node)
    (relate call-proc new-call new-node)
    (replace call-node
             (lambda (node)
               (walk erase-all
                     (node-children node))
               new-call))))

(define (simplify-define-constant node)
  (let ((original (get-support-from-ref support/constant ((call-arg 2) node))))
    (cond ((not original)
           nil)
          ((suitable-constant-support? original)
           (change-support-value node original))
          ((lambda-node? original)
           (set (primop-value (call-proc node)) primop/*define-wired)
           (change-support-type (variable-reference ((call-arg 2) node))
                                support/wired)
           (change-support-value node original))
          (else
           nil))))

(define (simplify-parameterized-primop node primop)
  (let ((call (node-parent node)))
    (cond ((and (eq? (node-role node) call-proc)
                (= (length (primop.formals primop)) ; Could be improper?
                   (-1+ (length (call-args call))))
                (every? literal-node? (cdr (call-args call)))
                (lambda-node? ((call-arg 1) call)))
           (let* ((arglist (map literal-value (cdr (call-args call))))
                  (new-primop (join (object nil
                                      ((primop.constructed? self) t)
                                      ((primop.arglist self) arglist))
                                    primop))
                  (new (create-primop-node new-primop)))
             (walk (lambda (ref)
                     (if (supports-definition? ref)
                         (change-support-value (node-parent ref) new)))
                   (variable-refs
                     (cadr (lambda-variables ((call-arg 1) call)))))))
          (else
           nil))))

(define (change-support-value call value)
  (let ((var (reference-variable ((call-arg 2) call)))
        (variant (primop.support-variant (primop-value (call-proc call)))))
    (add-support-value var variant value)
    (walk simplify-using-support
          (variable-refs var))))

(define (replace-call-with-value call value)
  (cond ((fxn= 1 (call-exits call))
         (bug "can only substitute for call with one exit ~S" call))
        (else
         (let ((cont (detach ((call-arg 1) call))))
           (walk (compose erase-all detach)
                 (cdr (call-args call)))
           (replace (call-proc call)
                    (lambda (old)
                      (erase-all old)
                      cont))
           (relate-shorter-call-args call (if value `(,value) '()))
           call))))

;;; (values cont a b c)
;;;  ==>  (cont a b c)

(define (simplify-values node)
  (let ((cont (car (call-args node))))
    (set (call-exits node) 0)
    (replace-call-args node (cdr (copy-list (call-args node))))
    (replace (call-proc node)
             (lambda (proc) (erase proc) cont))))

;;; (block cont x) => (cont) if x isn't a lambda

(define (simplify-block node)
  (cond ((not (lambda-node? ((call-arg 2) node)))
         (replace-call-with-value node nil))))

;;; (Y (lambda (B ...) (B K0))
;;;    (lambda (K1 ...)
;;;      (K1 (lambda (K2) . <body>) ...)))
;;; Substitute K0 for K2, since it is not necessary to actually call the body.

(define (simplify-Y node)
  (destructure (((#f l1 l2) (call-proc+args node)))
    (let ((var (cadr (lambda-variables ((call-arg 1) (lambda-body l2))))))
      (cond ((used? var)
             (substitute var ((call-arg 1) (lambda-body l1)) nil))
            (else
             nil)))))

;;; (<type>? cont x) => (test cont <type>? x)

(define (simplify-predicate node)
  (destructure (((pred cont arg) (call-proc+args node)))
    (replace node
             (lambda (old)
               (walk detach (call-proc+args old))
               (let ((new (create-call-node 4 1)))
                 (relate call-proc new (create-primop-node primop/test))
                 (relate-call-args new `(,cont ,pred ,arg ))
                 new)))))

;;; (<cond> cont arg1 arg2) =>
;;; (primop/conditional (lambda () (cont #t))
;;;                     (lambda () (cont #f))
;;;                      <cond> arg1 arg2)
;;; where <cond> is one of test, eq?, fx<, etc.

(define (simplify-to-conditional node)
  (destructure (((pred cont arg1 arg2) (call-proc+args node)))
    (replace node
             (lambda (old)
               (walk detach (call-proc+args old))
               (s-exp->node-tree `(exits 1
                                         (clambda (c)
                                           (exits 2
                                                  ,primop/conditional
                                                  (clambda () (exits 0 c '#t))
                                                  (clambda () (exits 0 c '#f))
                                                  ,pred
                                                  ,arg1
                                                  ,arg2))
                                         ,cont))))))


;;; ((setter foo) x . y) => (set-value (primop/foo-locative x) . y) if foo
;;; has a locative primop.  Simplify-setter is returned by the setter primop
;;; when called by primop.simplify.

(define (simplify-setter node)
  (cond ((simplify-setter? node)
         => (lambda (info)
              (let ((cont ((call-arg 1) node))
                    (arg (cond ((list? info)     ; alpha-integrable is gone
                                (let ((new (alpha-integrable info)))
                                  (convert new)
                                  new))
                               ((primop? info)
                                (create-primop-node info))
                               (else
                                (let ((ref (create-primop-node
                                              %set-accessor-primop)))
                                  (set (leaf-via ref) info)
                                  ref))))
                    (call (create-call-node 2 1)))
                (replace node
                         (lambda (old)
                           (detach cont)
                           (walk erase
                                 (node-children old))
                           (relate call-proc call cont)
                           (relate (call-arg 1) call arg)
                           call)))))))


(define (simplify-setter? node)
  (let ((arg  ((call-arg 2) node))
        (cont ((call-arg 1) node)))
    (and (known-primop-value arg)
         (lambda-node? cont)
         (eq? (node-role (car (variable-refs (cadr (lambda-variables cont)))))
              call-proc)
         (primop.setter (known-primop-value arg)))))

(define (known-primop-value node)
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

;;; --- Simplify a LET.

;;; Must substitute for the lambda's variables, if possible.  When
;;; we're done with this, all remaining arg nodes will be lambdas
;;; to which there are at least two calls.

;;; (let ((x 1) (y a) (f (lambda ...)))
;;;   ... x ... y ... f ...)

(define (simplify-let let-node)
  (let ((node (call-proc let-node)))
    (cond ((null? (cdr (lambda-variables node)))
           ;; ((lambda () x)) => x
           (replace let-node
                    (lambda (let-node)
                      (erase let-node)
                      (detach (lambda-body node)))))
          ((and (fx= 2 (call-exits (lambda-body node)))
                (null? (cddr (lambda-variables node)))
                (lambda-node? ((call-arg 1) let-node))
                (cdr (variable-refs (cadr (lambda-variables node)))))
           (simplify-join let-node))
          (else
           (walk (lambda (var val)
                   (cond ((not (used? var))
                          nil)
                         ((or (not (lambda-node? val))
                              (null? (cdr (variable-refs var))))
                          (substitute var val t))
                         (else
                          (set (variable-type var) val))))
                 (cdr (lambda-variables node))
                 (call-args let-node))
           (erase-unused-arguments let-node
                                   (cdr (lambda-variables node)))
           (remove-unreferenced-variables node)))))

(define (simplify-join join-node)
  (let ((proc (call-proc join-node))
        (cont ((call-arg 1) join-node)))
    (destructure (((cond? exit1 exit2 test? #f ref)
                   (call-proc+args (lambda-body cont))))
      (cond ((and (fx= 2 (length (lambda-variables cont)))
                  (primop-ref? cond? primop/conditional)
                  (primop-ref? test? primop/test)
                  (reference-node? ref)
                  (eq? (reference-variable ref)
                       (cadr (lambda-variables cont)))
                  (null? (cdr (variable-refs (cadr (lambda-variables cont))))))
             (let* ((c1 (create-variable 'c))
                    (c2 (create-variable 'c))
                    (new-proc (create-lambda-node (list nil c1 c2)))
                    (new-call (create-call-node 3 2)))
               (relate lambda-body new-proc (detach (lambda-body proc)))
               (detach exit1)
               (detach exit2)
               (relate call-proc new-call new-proc)
               (relate-call-args new-call (list exit1 exit2))
               (relate (call-arg 1) (lambda-body cont)
                                    (create-reference-node c1))
               (relate (call-arg 2) (lambda-body cont)
                                    (create-reference-node c2))
               (walk (lambda (ref)
                       (replace ref
                                (lambda (ref)
                                  (erase ref)
                                  (copy-node-tree cont '() nil))))
                     (variable-refs (cadr (lambda-variables proc))))
               (replace join-node (lambda (node)
                                    (erase node)
                                    new-call))
               (walk erase (node-children (lambda-body cont)))))
            (else
             nil)))))


;;; Returns label procedure for this variable.

(define (variable-known var)
  (let ((type (variable-type var)))
    (cond ((and (node? type)
                (lambda-node? type))
                ;; ((lambda (var) ...) type)  - fix later for labels
                ;(eq? (node-parent (variable-binder var))
                ;     (node-parent type)))
           type)
          (else nil))))

;;; If true, then the LAMBDA to which this variable is being bound
;;; can always be jumped to (although an environment adjustment may
;;; be needed).

(define (all-refs-are-calls? var)               
  (every? (lambda (ref)
            (eq? (node-role ref) call-proc))
          (variable-refs var)))

;;; Flush ignored arguments from calls to a known procedure.


(define (erase-unused-arguments c vars)
  (iterate loop ((vars vars)
                 (args (call-args c))
                 (new-args '()))
    (cond ((null? vars)
           (relate-shorter-call-args c (reverse! new-args)))
          ((not (used? (car vars)))
           (if (not (empty? (car args)))
               (erase-all (detach (car args))))
           (loop (cdr vars)
                 (cdr args)
                 new-args))
          (else
           (loop (cdr vars)
                 (cdr args)
                 (cons (detach (car args)) new-args))))))

;;; Used in conjunction with above.

(define (remove-unreferenced-variables node)
  (iterate loop ((vars (lambda-variables node)) (n 1))
    (cond ((null? (cdr vars))
           'done)
          ((used? (cadr vars))
           (set (variable-number (cadr vars)) n)
           (loop (cdr vars) (fx+ n 1)))
          (else
           (set (cdr vars) (cddr vars))
           (loop vars n)))))

;;; ---------------------------------------------------------------------
;;; Perform variable substitution

(define (substitute var val detach?)
  (let ((refs (variable-refs var)))
    (noise "substituting: ~A := ~A~%" var (pp-cps-2 val))
    (if detach? (detach val))
    (walk (lambda (ref)      ; careful - refs list is delq!'ed by erase
            (if (not (nonvalue-reference? ref))
                (replace ref (lambda (ref)
                               (erase ref)
                               (copy-node-tree val '() nil)))))
          (copy-list refs))
    (if (and (reference-node? val)
             (eq? 'k (variable-name (reference-variable val))))
        (set (variable-name (reference-variable val)) (variable-name var)))
    (if detach? (erase-all val))))



(define (nonvalue-reference? ref)
  (and (eq? (node-role ref) (call-arg 2))
       (primop-node? (call-proc (node-parent ref)))
       (primop.sets-variable?
         (primop-value (call-proc (node-parent ref))))))

(define (defining-refs var)
  (iterate loop ((refs (variable-refs var)) (res '()))
    (cond ((null? refs)
           res)
          ((nonvalue-reference? ref)
           (loop (cdr refs) (cons ref res)))
          (else
           (loop (cdr refs) res)))))
