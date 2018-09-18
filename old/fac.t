(herald fac)


(define (fac n)
  (labels (((f x) (if (fx= x 0) 1 (fx* x (f (fx- x 1))))))
    f))
