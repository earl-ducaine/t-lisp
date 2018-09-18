(herald primops
        (syntax-table *orbit-syntax-table*))

(define-predicate primop?)

;;; Primop environments
;;; Alphatize, early binding, support
(define-operation (primop.id primop) nil)
(define-operation (primop.environment primop) nil)
(define-operation (primop.source primop) nil)

;;; Parameterized primops
;;; Simplify, PRIMOP.ARGLIST is used internally by primop methods.
(define-predicate primop.constructed?)
(define-operation (primop.formals primop) '())
(define-operation (primop.arglist primop) '())

;;; Simplification
(define-predicate primop.uses-L-value?)
(define-operation (primop.simplify primop node) nil)
(define-operation (primop.presimplify primop node) nil)
(define-operation (primop.integrate? primop node)
  (eq? (node-role node) call-proc))

;;; Code generation
(define-predicate primop.special?)
(define-predicate primop.closed-compiled?)
(define-operation (primop.generate primop node) nil)
(define-operation (primop.type primop node) 'top?)
(define-operation (primop.arg-specs primop) nil)


;;; Non generic, specific to certain types of primops

;;; Creating support
(define-predicate primop.defines-support?)
(define-operation (primop.support-variant primop) nil)

;;; Flow of control
(define-predicate primop.conditional?)
(define-predicate primop.type-predicate?)
(define-operation (primop.test-code primop node arg) nil)
(define-operation (primop.compare-code primop node arg) nil)

;;; Special calling sequences
(define-operation (primop.external-name primop) nil)
(define-operation (primop.used-registers primop) nil)
(define-operation (primop.return-reg primop) nil)

;;; Locations
(define-predicate primop.location?)
(define-operation (primop.location-offset primop node) nil)
