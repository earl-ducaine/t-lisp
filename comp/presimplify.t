(herald presimplify
        (syntax-table *orbit-syntax-table*))

;;; (PRESIMPLIFY node)
;;;===========================================================================
;;;   This handles any simplification that needs to be done before the children
;;; of the node are simplified.  These are simplifications that change the
;;; arguments of calls in some way that affects what are considered exits
;;; or that move the arguments further down the tree.

(define (presimplify node)
  (cond ((not (call-node? node))
         nil)
        ((lambda-node? (call-proc node))
         (presimplify-let node))
        ((known-primop (call-proc node))
         => (lambda (primop)
              (if (or (null? (primop.formals primop))
                      (primop.constructed? primop))
                  (primop.presimplify primop node)
                  nil)))
        (else
         nil)))

;;; (PRESIMPLIFY-LET let-node)
;;;========================================================================
;;;  ((lambda () x)) => x
;;;  Join points have their own procedure.
;;;  Otherwise, the lambda variables are substituted for if possible.  When
;;; we're done with this, all remaining arg nodes will be lambdas to which
;;; there are at least two calls.

(define (presimplify-let let-node)
  (let ((node (call-proc let-node)))
    (cond ((or (fx< (length (call-args let-node))
                    (length (lambda-variables node)))
               (and (null? (lambda-rest-var node))
                    (fx> (length (call-args let-node))
                         (length (lambda-variables node)))))
           (error "wrong number of arguments to a lambda node ~S" let-node))
          ((and (null? (lambda-variables node))
                (null? (lambda-rest-var node)))
           (replace let-node (detach (lambda-body node)))
           t)
          ((and (fx= 2 (call-exits (lambda-body node)))
                (null? (lambda-rest-var node))
                (fx= 1 (length (lambda-variables node)))
                (lambda-node? ((call-arg 1) let-node))
                (cdr (variable-refs (car (lambda-variables node)))))
           (presimplify-join let-node))
          (else
           (*or (any? true?
                      (map (lambda (var val)
                             (cond ((not (used? var))
                                    nil)
                                   ((or (not (lambda-node? val))
                                        (null? (cdr (variable-refs var))))
                                    (substitute var val t)
                                    t)
                                   (else
                                    (set (variable-type var) val)  ; Why?
                                    nil)))
                           (lambda-variables node)
                           (call-args let-node)))
           ; Do something with the rest variable?
                (erase-unused-arguments let-node (lambda-variables node))
                (remove-unreferenced-variables node))))))

;;; (PRESIMPLIFY-JOIN join-node)
;;;===========================================================================
;;;  (IF (IF A B C) D E) => (IF A (IF B D E) (IF C D E)).
;;; In CPS this is
;;;  ((LAMBDA (C)
;;;     <call with two exits>)
;;;   (LAMBDA (X)
;;;     (PRIMOP/COND <exit1> <exit2> PRIMOP/TEST <?> X)))
;;;
;;; which magically becomes
;;;  ((LAMBDA (C1 C2)
;;;     <call with two exits>
;;;       [with C replaced by (LAMBDA (X)
;;;                             (PRIMOP/COND C1 C2 PRIMOP/TEST <?> X))])
;;;   <exit1>
;;;   <exit2>)

(define (presimplify-join join-node)
  (let ((proc (call-proc join-node))
        (cont ((call-arg 1) join-node)))
    (destructure (((cond? exit1 exit2 test? #f ref)
                   (call-proc+args (lambda-body cont))))
      (cond ((and (fx= 1 (length (lambda-variables cont)))
                  (null? (lambda-rest-var cont))
                  (primop-ref? cond? primop/conditional)
                  (primop-ref? test? primop/test)
                  (variable-ref? ref (lambda-cont-var cont))
                  (null? (cdr (variable-refs (lambda-cont-var cont)))))
             (let* ((c1 (create-variable 'c))
                    (c2 (create-variable 'c))
                    (new-proc (create-lambda-node 'p (list nil c1 c2)))
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
                       (replace ref (copy-node-tree cont '())))
                     (variable-refs (lambda-cont-var proc)))
               (replace join-node new-call)
               (walk erase (node-children (lambda-body cont)))
               t))
            (else
             nil)))))

;;; (PRESIMPLIFY-VALUES node)
;;;============================================================================
;;; (values cont a b c)
;;;  ==>  (cont a b c)

(define (presimplify-values node)
  (let ((cont (car (call-args node))))
    (set (call-exits node) 0)
    (replace-call-args node (cdr (copy-list (call-args node))))
    (replace (call-proc node) cont)
    t))

;;; (PRESIMPLIFY-RECEIVE-VALUES node)
;;;============================================================================
;;; (RECEIVE-VALUES <cont>
;;;                 (LAMBDA (C1 . <vars>) . <body>)
;;;                 (LAMBDA (C2) . <exp>))
;;; =>
;;; If C2 has less than two references
;;; <exp>[C2 replaced by (LAMBDA <vars>
;;;                        ((LAMBDA (C1) . <body>) <cont>))]
;;; Otherwise
;;; ((LAMBDA (C2) . <exp>)
;;;  (LAMBDA <vars>
;;;    ((LAMBDA (C1) . <body>) <cont>)))
;;;

(define (presimplify-receive-values node)
  (destructure (((cont body-lambda exp-lambda) (call-args node)))
    (cond ((not (and (fx= 3 (length (call-args node)))
                     (lambda-node? body-lambda)
                     (lambda-node? exp-lambda)))
           nil)
          (else
           (replace-var (lambda-cont-var body-lambda)
                        cont
                        (lambda-body body-lambda)
                        t)
           (set (lambda-all-variables body-lambda)
                (delq! (lambda-cont-var body-lambda)
                       (lambda-all-variables body-lambda)))
           (cond ((null? (cdr (variable-refs (lambda-cont-var exp-lambda))))
                  (substitute (lambda-cont-var exp-lambda) body-lambda t)
                  (replace node (detach (lambda-body exp-lambda)))
                  (erase exp-lambda))
                 (else
                  (let ((new-node (create-call-node 2 1)))
                    (relate call-proc new-node (detach exp-lambda))
                    (relate (call-arg 1) new-node (detach body-lambda))
                    (replace node new-node))))
           t))))

;;; (PRESIMPLIFY-PREDICATE node)
;;;============================================================================
;;; (<type>? cont x) => (test cont <type>? x)

(define (presimplify-predicate node)
  (destructure (((pred cont arg) (call-proc+args node)))
    (let ((new (create-call-node 4 1))
          (primop (if (primop-node? pred)
                      pred
                      (create-primop-node (known-primop pred)))))
      (walk detach (call-proc+args node))
      (if (reference-node? pred) (erase pred))
      (relate call-proc new (create-primop-node primop/test))
      (relate-call-args new `(,cont ,primop ,arg))
      (replace node new)
      t)))

;;; (PRESIMPLIFY-TO-CONDITIONAL node)
;;;============================================================================
;;; (<cond> cont arg1 arg2) =>
;;; (primop/conditional (lambda () (cont #t))
;;;                     (lambda () (cont #f))
;;;                      <cond> arg1 arg2)
;;; where <cond> is one of test, eq?, fx<, etc.

(define (presimplify-to-conditional node)
  (destructure (((pred cont arg1 arg2) (call-proc+args node)))
    (let ((primop (if (primop-node? pred)
                      pred
                      (create-primop-node (known-primop pred)))))
      (walk detach (call-proc+args node))
      (if (reference-node? pred) (erase pred))
      (replace node
               (s-exp->node-tree `(exits 1
                                         (clambda (c)
                                           (exits 2
                                                  ,primop/conditional
                                                  (clambda () (exits 0 c '#t))
                                                  (clambda () (exits 0 c '#f))
                                                  ,primop
                                                  ,arg1
                                                  ,arg2))
                                         ,cont)))
      t)))
