(herald sets)

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;; Utility operations on sets: ADJOIN, UNION, INTERSECTION, REMQ, SETDIFF

;;; The empty set.

(define (empty-set)
  '())

;;; Add an element X to a set S.

(define (adjoin x s)
  (cond ((memq? x s) s)
        (else (cons x s))))

;;; Union of two sets.

(define (union x y)
  (cond ((null? x) y)
        (else (do ((z y (cdr z))
                   (u x (adjoin (car z) u)))
                  ((null? z) u)))))

;;; Intersection of two sets.

(define (intersection x y)
  (cond ((null? x) '())
        ((null? y) '())
        ((memq (car x) y)
         (cons (car x) (intersection (cdr x) y)))
        (else (intersection (cdr x) y))))

(define (intersection? x y)
  (cond ((null? x) nil)
        ((null? y) nil)
        ((memq (car x) y) t)
        (else (intersection? (cdr x) y))))

;;; Remove an element X from a set S, non-destructively.

(define (setremq x s)
  (cond ((null? s) s)
        ((eq? (car s) x) (cdr s))
        (else (let ((y (setremq x (cdr s))))
                (cond ((eq? y (cdr s)) s)
                      (else (cons (car s) y)))))))

;;; Difference of two sets: (SETDIFF A B) = {X|XA   XB}

(define (setdiff x y)
  (do ((z x (cdr z))
       (w '() (cond ((memq (car z) y) w)
                    (else (cons (car z) w)))))
      ((null? z) w)))
