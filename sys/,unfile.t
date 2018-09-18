(herald unfile (env t))

(define-local-syntax (unix-check val)
  (if (fxN= val 0)
      (unix-error)
      T))
	

;*** FILE-EXISTS?
;*** ===========================================================
;***
(let ((buf (make-stat-buf)))
  (define (file-exists? fname)
    (fx= 0 (xcall *stat-xenoid* 
                  (string->asciz (filename->string fname))
                  buf)))
)


;*** FILE-PROBE
;*** ===========================================================
;***
(define (file-probe fname)
  
