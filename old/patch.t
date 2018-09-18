(herald patch)

(define (lambda-self-var node)
  (car (lambda-variables node)))



(define (create-lambda-node vars)
  (let ((node (create-node lambda-node? 1))
        (vars (cond ((null? (car vars))
                     (cons (create-variable 'p) (cdr vars)))
                    (else vars))))
    (set (lambda-variables node) vars)
    (set (lambda-strategy node) nil) 
    (set (lambda-live node) nil)
    (set (lambda-env node) nil)
    (do ((vars vars (cdr vars))
         (n 0 (fx+ n 1)))
        ((null? vars))
      (let ((var (car vars)))
        (cond ((used? var)
               (set (variable-binder var) node)
               (set (variable-number var) n)))))
    node))
