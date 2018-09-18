(herald tak)

(define (foo x y z)
  (labels (((tak x y z)
  (if (not (fx< y x))
         z
         (tak (tak (fx- x 1) y z)
              (tak (fx- y 1) z x)
              (tak (fx- z 1) x y)))))
  (tak x y z)))
