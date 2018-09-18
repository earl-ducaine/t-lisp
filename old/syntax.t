(herald syntax)

;;; Macros for compiler.

;;; Kludges

(*define *standard-env* '*orbit-syntax-table*
  (env-syntax-table (the-environment)))

(set (syntax-table-entry *standard-syntax-table* 'define-wired)
     (macro-expander (define-wired . rest)
       `(define . ,rest)))

(define-syntax primop nil)
