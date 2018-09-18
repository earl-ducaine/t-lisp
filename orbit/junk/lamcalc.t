(herald lamcalc)

;;; Lambda calculator.

(define (substitute exp var val)
  (xcond ((self-evaluates? exp) exp)
         ((symbol? exp)
          (cond ((eq? exp var) val)
                (else exp)))
         ((lambda-exp? exp)
          (cond ((memq? var (cadr exp)) exp)
                (else
                 `(lambda ,(cadr exp)
                    ,(substitute (caddr exp) var val)))))
         ((pair? exp)
          (map (lambda (exp) (substitute exp var val))
               exp))))

(define (reference-count exp var)
  (xcond ((self-evaluates? exp) 0)
         ((symbol? exp)
          (cond ((eq? exp var) 1)
                (else 0)))
         ((lambda-exp? exp)
          (cond ((memq? var (cadr exp)) 0)
                (else (reference-count (caddr exp) var))))
         ((pair? exp)
          (apply + (map (lambda (exp) (reference-count exp var))
                        exp)))))

(define (evaluate exp)
  (xcond ((self-evaluates? exp) exp)
         ((symbol? exp)
          (or (global-value exp) exp))
         ((lambda-exp? exp) exp)
          ;`(lambda ,(cadr exp)
          ;   ,(evaluate (caddr exp)))
         ((pair? exp)
          (evaluate-call (map evaluate exp)))))

(define (evaluate-call exp)
  (let ((proc (car exp))
        (args (cdr exp)))
    (cond ((lambda-exp? proc)
           (iterate loop ((body (evaluate (caddr proc)))
                          (vars (cadr proc))
                          (args args)
                          (new-vars '())
                          (new-args '()))
                    (cond ((null? args)
                           (cond ((null? new-args)
                                  body)
                                 (else
                                  `((lambda ,(reverse! new-vars) ,body)
                                    ,@(reverse! new-args)))))
                          ((subst-candidate? body (car vars) (car args))
                           (loop (evaluate (substitute body (car vars) (car args)))
                                 (cdr vars)
                                 (cdr args)
                                 new-vars
                                 new-args))
                          (else
                           (loop body
                                 (cdr vars)
                                 (cdr args)
                                 (cons (car vars) new-vars)
                                 (cons (car args) new-args))))))
          ((get-primop proc)
           =>
           (lambda (primop)
             (primop-evaluate primop exp)))
          (else exp))))

(define (subst-candidate? body var val)
  (or (self-evaluates? val)
      (<= (reference-count body var) 1)))

(import *t-implementation-env* make-table table-entry)

(define *primops* (make-table '*primops*))

(define (get-primop exp)
  (and (symbol? exp) (table-entry *primops* exp)))

(define-operation (primop-evaluate primop exp) exp)

(define (make-strict-primop proc)
  (object nil
          ((primop-evaluate self exp)
           (cond ((every self-evaluates? (cdr exp))
                  (make-literal (apply proc
                                       (map literal-value (cdr exp)))))
                 (else exp)))
          ((print self stream)
           (format stream "#{Strict-primop~_~S~_~S}"
                   (object-hash self)
                   (or (identification proc) proc)))))

(define (make-predicate-primop proc)
  (object nil
          ((primop-evaluate self exp)
           (cond ((every self-evaluates? (cdr exp))
                  (if (apply proc (map literal-value (cdr exp)))
                      't
                      'nil))
                 (else exp)))
          ((print self stream)
           (format stream "#{Strict-primop~_~S~_~S}"
                   (object-hash self)
                   (or (identification proc) proc)))))

(define-macro (define-primop name val)
  `(set (table-entry *primops* ',name) ,val))

(define-primop +    (make-strict-primop +))
(define-primop -    (make-strict-primop -))
(define-primop *    (make-strict-primop *))
(define-primop /    (make-strict-primop /))
(define-primop =    (make-predicate-primop =))
(define-primop <    (make-predicate-primop <))
(define-primop >    (make-predicate-primop >))
(define-primop eq?  (make-strict-primop eq?))
(define-primop car  (make-strict-primop car))
(define-primop cdr  (make-strict-primop cdr))
(define-primop cons (make-strict-primop cons))

(define-primop if-true
  (object nil
          ((primop-evaluate self exp)
           (cond ((self-evaluates? (cadr exp))
                  (evaluate (list
                             (xcase (literal-value (cadr exp))
                               ((true) (caddr exp))
                               ((false) (cadddr exp))))))
                 (else exp)))
          ((identification self) 'if-true)))

;;; Expression predicates

(define (lambda-exp? exp)
  (and (pair? exp) (eq? (car exp) 'lambda)))

(define (self-evaluates? exp)
  (or (and (pair? exp) (eq? (car exp) 'quote))
      (self-evaluating-value? exp)))

(define (self-evaluating-value? value)
  (or (number? value)
      (string? value)
      (char? value)))

(define (make-literal value)
  (cond ((self-evaluating-value? value) value)
        (else `(quote ,value))))

(define (literal-value exp)
  (cond ((and (pair? exp)
              (eq? (car exp) 'quote))
         (cadr exp))
        (else exp)))

(define (global-value symbol)
  (table-entry *global-env* symbol))

(define *global-env* (make-table '*global-env*))

(define-macro (define-value var val)
  `(block (set (table-entry *global-env* ',var) ',val)
          ',var))

(define-value Y
  (lambda (F)
    ((lambda (x) (F (x x)))
     (lambda (x) (F (x x))))))

(define-value fact
  (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))

(define-value gen-fact
  (lambda (fact)
    (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))))

(define-value t 'true)

(define-value nil 'false)
