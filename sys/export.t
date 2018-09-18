(herald (tsys export t 7)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;; Export utilities.

(lset *tsys-exports* '())

(define (declare-tsys-exports l)
  (push *tsys-exports* l)
  (car l))

(lset *tsys-syntax-exports* '())

(define (declare-tsys-syntax-exports l)
  (push *tsys-syntax-exports* l)
  (car l))

(define (export-tsys to-env)
  (let ((n 0)
        (from-env *t-implementation-env*))
    (let ((f (lambda (var)
               (set n (fx+ n 1))
               (*define to-env var
                        (*VALUE from-env var)))))
      (walk (lambda (l) (walk f l))
            *tsys-exports*))
    (let ((from-syn (env-syntax-table from-env))
          (to-syn   (env-syntax-table to-env)))
      (let ((f (lambda (sym)
                 (set n (fx+ n 1))
                 (let ((desc (syntax-table-entry from-syn sym)))
                   (cond ((null? desc)
                          (error "no syntax for ~S" sym))
                         (else
                          (set (syntax-table-entry to-syn sym) desc)))))))
        (walk (lambda (l) (walk f l))
              *tsys-syntax-exports*)))
    n))
