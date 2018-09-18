(herald vaxbase
        (support (make-empty-support-env 'empty)))

;;; BASIC PRIMOPS
;;;============================================================================
;;; These are all known to alphatize, simplify, etc.

;;; *primop
;;;============================================================================

(define-wired *primop
  (primop *primop ()))

;;; Place marking primops
;;;============================================================================
;;; These are used by alpha to mark points in the tree.

(define-wired continuation
  (primop continuation ()))

(define-wired undefined
  (primop undefined ()))

(define-wired undefined-effect
  (primop undefined-effect ()))


;;; Defining primops
;;;============================================================================
;;; These assign values global variables.

(define-wired (make-definer support-type)
  (primop make-definer (support-type)
    ((primop.generate self node)
     (generate-define-var node))
    ((primop.type self node)
     '(proc (cont) loc? top?))
    ((primop.arg-specs self)
     '(pointer *))
    ((primop.simplify self node)
     (if (eq? 'constant (car (primop.arglist self)))
         (simplify-define-constant node)
         nil))
    ((primop.defines-support? self) t)
    ((primop.uses-L-value? self) t)
    ((primop.support-variant self)
     (xcase (car (primop.arglist self))
       ((lset)       support/lset)
       ((define)     support/define)
       ((constant)   support/constant)
       ((integrable) support/integrable)
       ((wired)      support/wired)))))

(define-wired *define
  (make-definer 'define))

(define-wired *lset
  (make-definer 'lset))

(define-wired *define-integrable
  (make-definer 'integrable))

(define-wired *define-constant
  (make-definer 'constant))

(define-wired *define-wired
  (make-definer 'wired))

;;; Random other basic primops
;;;============================================================================

(define-wired Y
  (primop Y ()
    ((primop.generate self node)
     (generate-labels node))
    ((primop.simplify self node)
     (simplify-y node))
    ((primop.special? self) t)
    ((primop.type self node)
     '(proc cont? proc?))))

(define-wired receive-values
  (primop receive-values ()
    ((primop.generate self node)
     (generate-receive-values node))
    ((primop.special? self) t)
    ((primop.type self node)
     '(proc (cont) proc?))
    ((primop.presimplify self node)
     (presimplify-receive-values node))
    ((primop.arg-specs self)
     '(* nil))))

(define-wired conditional
  (primop conditional ()
    ((primop.generate self node)
     (primop.generate (primop-value ((call-arg 3) node)) node))
    ((primop.arg-specs self)
     '(* * *))
    ((primop.conditional? self) t)
    ((primop.simplify self node)
     (primop.simplify (primop-value ((call-arg 3) node)) node))
    ((primop.type self node)
     '(proc (cont) (cont) proc? top? top?))))
       
(define-wired test
  (primop test ()
    ((primop.generate self node)
     (destructure (((then else () type arg) (call-args node)))
       (primop.test-code (primop-value type)
                         node
                         (access-value node (leaf-value arg)))
       (emit-jump 'jeql else then)))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.simplify self node)
     (simplify-test node))
    ((primop.arg-specs self)
     '(* * *))
    ((primop.conditional? self) t)))

(define-wired true?
  (primop true? ()
    ((primop.test-code self node arg)
     (emit vax/cmpl arg nil-reg))
    ((primop.presimplify self node)
     (presimplify-predicate node))
    ((primop.type-predicate? self) t)))

(define-wired *set-var
  (primop *set-var ()
    ((primop.generate self node)
     (generate-set node
                   ((call-arg 2) node)
                   ((call-arg 3) node)))
    ((primop.uses-L-value? self) t)
    ((primop.type self node)
     '(proc (cont) loc? top?))))

(define-wired *locative
  (primop *locative ()
    ((primop.uses-L-value? self) t)))

(define-wired make-locative
  (primop make-locative ()))

(define-wired contents
  (primop contents ()))

(define-wired set-contents
  (primop set-contents ()))

(define-wired let-reference
  (primop let-reference ()))

(define-wired values
  (primop values ()
    ((primop.presimplify self node)
     (presimplify-values node))))

(define-wired call-with-continuation
  (primop call-with-continuation ()
    ((primop.generate self node)
     (generate-call-with-continuation node))
    ((primop.closed-compiled? self) t)
    ((primop.arg-specs self) '(*))))

(define-wired (make-nonstandard-call arg-specs used return name)
  (primop make-nonstandard-call (arg-specs used return name)
    ((primop.generate self node)
     (generate-nonstandard-call node
                                (car (primop.arglist self))
                                (cadr (primop.arglist self))
                                (caddr (primop.arglist self))))
    ((primop.arg-specs self)
     (car (primop.arglist self)))
    ((primop.external-name self)
     (cadddr (primop.arglist self)))
    ((primop.closed-compiled? self) t)))

(define-wired %make-pair
  (make-nonstandard-call '() '(0 9) 9 '%make-pair))

(define-wired %make-extend
  (make-nonstandard-call '(9 1) '(0) 9 '%make-extend))


;;; To deal with objects and their ilk.
(define-wired proc+handler
  (primop proc+handler ()))

(define-wired handler
  (primop handler ()))
