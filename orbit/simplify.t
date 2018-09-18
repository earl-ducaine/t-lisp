(herald simplify
        (syntax-table *orbit-syntax-table*))

(define (simplify node)
  (cond ((lambda-node? node)
