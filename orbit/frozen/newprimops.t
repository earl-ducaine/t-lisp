(herald newprimops)


(define-wired (make-nonstandard-call arg-specs)
  (primop make-nonstandard-call (arg-specs)
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


;(define-wired %make-pair
 ; (make-nonstandard-call '() '(0 9) 9 '%make-pair))

;(define-wired %make-extend
  ;(make-nonstandard-call '(9 1) '(0) 9 '%make-extend))

(define-wired foo (make-nonstandard-call 1))
