(herald fact)

(define (fact n)
  (labels (((f a i) (if (fx= i 0) a (f (fx* a i) (fx- i 1)))))
    (f 1 n)))
