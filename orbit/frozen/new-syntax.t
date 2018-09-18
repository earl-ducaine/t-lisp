(herald new-syntax)

;;; Syntax changes from T2 to T3.  The only one there was is gone.  There
;;; are more on the horizon, e.g. LOCATIVE.

(define *new-standard-syntax-table*
  (make-syntax-table *standard-syntax-table* '*new-standard-syntax-table*))
