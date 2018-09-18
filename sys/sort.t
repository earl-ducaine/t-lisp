(herald (tsys sort t 10)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; A very crude SORT

;;; We gratefully acknowledge George J. Carette of MIT.

(define-local-syntax (rplacd&pop x y)
  (let ((cell (generate-symbol 'rplacd&pop)))
    `(let ((,cell ,x))
       (set ,x (cdr ,x))
       (set (cdr ,cell) ,y)
       ,cell)))

(define (zsort! list judge)
  (cond ((null? list) '())
        (else
         (let ((proof (rplacd&pop list '())))
           (iterate loop ((goats '())
                          (sheep '()))
             (cond ((null? list)
                    (append! (zsort! goats judge)
                             proof
                             (zsort! sheep judge)))
                   ((judge (car list) (car proof))
                    (loop (rplacd&pop list goats) sheep))
                   (else
                    (loop goats (rplacd&pop list sheep)))))))))

(define (zsort list judge) (zsort! (copy-list list) judge))

(define sort! zsort!)

(define sort zsort)
