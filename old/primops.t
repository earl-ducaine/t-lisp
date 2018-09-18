(herald primops
        (syntax-table *orbit-syntax-table*))

(define-predicate primop?)

(define-predicate primop.defines-support?)
(define-predicate primop.sets-variable?)
(define-predicate primop.special?)
(define-predicate primop.type-predicate?)
(define-predicate primop.conditional?)
(define-predicate primop.comparator?)
(define-predicate primop.constructed?)

(define-operation (primop.environment primop) nil)
(define-operation (primop.id primop) nil)
(define-operation (primop.source primop) nil)
(define-operation (primop.formals primop) '())
(define-operation (primop.arglist primop) '())

(define-operation (primop.support-variant primop) nil)
(define-operation (primop.generate primop node) nil)
(define-operation (primop.type primop node) 'top?)
(define-operation (primop.simplify primop node) nil)
(define-operation (primop.transform primop node) nil)
(define-operation (primop.simplify-as-conditional primop node) nil)
(define-operation (primop.closed-compiled? primop) nil)

(define-operation (primop.integrate? primop node)
  (eq? (node-role node) call-proc))

(define-operation (primop.accessor primop) nil)
(define-operation (primop.setter primop) nil)
(define-operation (primop.arg-specs primop) nil)
(define-operation (primop.test-code primop node arg) nil)
(define-operation (primop.compare-code primop node arg) nil)

(define-operation (primop.used-registers primop) nil)
(define-operation (primop.return-reg primop) nil)

(define-operation (primop.pp-cps self node indent stream) nil)
