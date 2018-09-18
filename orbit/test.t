(herald test)

;;; ---------------------------------------------------------------------
;;; Test routines for ORBIT compiler.

(define (foo n)
  (labels (((loop a i)
            (if (= i 0) a (loop (+ a i) (- i 1)))))
    (loop 0 n)))

(define (fact n)
  (labels (((loop a i)
            (if (= i 0) a (loop (* a i) (- i 1)))))
     (loop 1 n)))

(define (fact2 n)
  (labels ((loop (lambda (a i)
                   (if (= i 0) a (loop (* a i) (- i 1))))))
     (loop 1 n)))

(define (rfact n)
  (labels (((fact n)
            (if (= n 0) 1 (* n (fact (- n 1))))))
    (fact n)))

(define (par l flag)
  (labels (((odd l)
            (if (null? l) 'odd
               (if (car l) (even (cdr l)) (odd (cdr l)))))
           ((even l)
            (if (null? l) 'even
                (if (car l) (odd (cdr l)) (even (cdr l))))))
    (if flag (even l) (odd l))))
