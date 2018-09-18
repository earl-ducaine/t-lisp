(herald normal)

;;; Normal order evaluator.

(define (normal exp env)
  (xcond ((self-evaluates? exp) exp)
         ((symbol? exp) (value exp env))
         ((lambda-exp? exp)
          `(lambda ,(cadr exp)
             ,(normal (caddr exp) env)))
         ((pair? exp) (normal-call exp env))))

(define (normal-call exp env)
  (let ((proc (normal (car exp) env))
        (args (map (lambda (exp) (normal exp env)) (cdr exp))))
     (normal-apply proc args)))

(define (normal-apply proc args)
  (cond ((lambda-exp? proc)
         (normal (caddr proc)
                 (map cons (cadr proc) args)))
        ((and (primop? proc)
              (every self-evaluates? args))
         (make-literal
            (primop-apply proc
                          (map literal-value args))))
        (else
         (cons proc args)))))

(define (primop? exp)
  (and (symbol? exp)
       (true? (get-primop exp))))

(define (primop-apply primop args)
  (apply (get-primop primop) args))

(lset *primops* '())

(define-macro (define-primop pat . maybe-val)
  `(block (push *primops*
                (cons* ',(car pat)
                       (create-primop ,(or (car maybe-val) (car pat)))

(define-structure-type primop proc strictness)

(define (get-primop symbol)
  (case symbol
    ((+) +)
    ((-) -)
    ((*) *)
    ((/) /)
    ((=) =)
    ((<) <)
    ((>) >)
    ((eq?) eq?)
    ((car) car)
    ((cdr) cdr)
    ((cons) cons)
    (else nil)))

(define (value exp env)
  (cond ((assq exp env) => cdr)
        (else exp)))

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

(define (norm exp)
  (normal exp *global-env*))

(define-macro (define-normal var val)
  `(block (push *global-env* (cons ',var ',val))
          ',var))

(lset *global-env* '())

(define-normal Y
  (lambda (F)
    ((lambda (x) (F (x x)))
     (lambda (x) (F (x x))))))

(define-normal fact
  (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))

(define-normal gen-fact
  (lambda (fact)
    (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))))
