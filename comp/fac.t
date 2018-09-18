(herald fac)

(define (fac n)
  (if (fx= n 0) 1 (fx* n (fac (fx- n 1)))))

;(define (f x)
;  (labels (((foo z) (foo (foo z))))
;     (foo x)))
