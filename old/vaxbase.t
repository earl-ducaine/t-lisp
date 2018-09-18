(herald vaxbase)


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

(define-wired n-ary
  (primop n-ary ()))


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
         (simplify-define-constant node)))
    ((primop.defines-support? self) t)
    ((primop.sets-variable? self) t)
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

(define-wired *block
  (primop *block ()
    ((primop.generate self node)
     (generate-block node))
    ((primop.special? self) t)
    ((primop.type self node)
     '(proc (cont) proc?))
    ((primop.simplify self node)
     (simplify-block node))
    ((primop.pp-cps self node indent stream)
     (pp-cps-block node indent stream))
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
     (primop.simplify-as-conditional (primop-value ((call-arg 3) node)) node))
    ((primop.type self node)
     '(proc (cont) (cont) proc? top? top?))))
       
(define-wired test
  (primop test ()
    ((primop.generate self node)
     (destructure (((then else () type arg) (call-args node)))
       (primop.test-code (primop-value type)
                         node
                         (make-leaf-accessable node arg))
       (emit-jump node "jeql" else then)))
    ((primop.simplify self node)
     (simplify-to-conditional node))
    ((primop.simplify-as-conditional self node)
     (simplify-test node))
    ((primop.arg-specs self)
     '(* * *))
    ((primop.conditional? self) t)))

(define-wired true?
  (primop true? ()
    ((primop.test-code self node arg)
     (icreate node "cmpl" arg nil-reg))
    ((primop.simplify self node)
     (simplify-predicate node))
    ((primop.type-predicate? self) t)))

(define-wired *set-var
  (primop *set-var ()
    ((primop.generate self node)
     (generate-set node
                   ((call-arg 2) node)
                   ((call-arg 3) node)))
    ((primop.sets-variable? self) t)
    ((primop.type self node)
     '(proc (cont) loc? top?))))

(define-wired *locative
  (primop *locative ()
    ((primop.sets-variable? self) t)))

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
    ((primop.simplify self node)
     (simplify-values node))))

(define-wired setter
  (primop setter ()
    ((primop.simplify self node)
     (simplify-setter node))))
