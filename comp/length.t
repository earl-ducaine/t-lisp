(herald length)

(define (length l)
  (do ((i 0 (fx+ i 1))
       (l l (cdr l)))
      ((eq? foo l) i)))
