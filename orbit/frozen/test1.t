(herald test1)

(define-wired car
  (object (lambda (x)
            (contents (car-loc x)))
    ((locatizer self)
     (lambda (x)
       (car-loc x)))
    ((setter self)
     (lambda (x y)
       (set-contents (car-loc x) y)))))
