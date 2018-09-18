(herald vmscalls
	(env tsys))

;;; Class (DSC$K_CLASS_S) and Dtype (DSC$K_DTYPE_T) for strings
(define-constant %%static-string #x010D)

;+++ This doesn't take the offset into account - it should.
(define-integrable (string->descriptor str)
  (let ((desc (make-bytev 8)))
    (set (bref-16 desc 0) (string-length str))
    (set (bref-16 desc 2) %%static-string)
    (set (xref desc 1) (string-pointer str))
    desc))

(define-integrable (string->descriptor! str desc)
    (set (bref-16 desc 0) (string-length str))
    (set (bref-16 desc 2) %%static-string)
    (set (xref desc 1) (string-pointer str))
    desc)

(define-integrable (descriptor->string desc)
  (let ((str (chopy "")))
    (set (string-pointer str) (xref desc 1))
    (set (string-length  str) (bref-16 desc 0))
    str))

(define-integrable (discriptor->string! desc str)
  (set (string-pointer str) (xref desc 1))
  (set (string-length  str) (bref-16 desc 0))
  str)

(define $trnlog_xeno (make-xenoid (integer->pointer #x7FFEE058)))

(define (vms$trnlog in)
  (with-buffer (buf 63)
    (check-arg string? in vms$trnlog)
    (let ((i        (string->descriptor (string-upcase in)))
	  (o        (string->descriptor buf))
	  (table    (make-bytev 4))
	  (acmode   (make-bytev 4)))
      (vms-i/o $trnlog_xeno i o o table acmode 0)
      (copy-string (descriptor->string o)))))

(define $trnlog_xeno (make-xenoid (integer->pointer #x7FFEE058)))

(define (vms$crelog lognam eqlnam)
    (check-arg string? lognam vms$trnlog)
    (check-arg string? eqlnam vms$trnlog)
    (let ((log (string->descriptor (string-upcase lognam)))
	  (eql (string->descriptor eqlnam)))
      (vms-i/o $crelog_xeno 2 log eql %%user_mode)))


;; define vmscall
;; (define-vms $trnlog in:string out:string . table:fix acmode=user dsbmsk=0)
;; => (define (vms$trnlog in out . table acmode dsbmsk)
;;        (checkarg string out vms$trnlog)


;;(define-syntax (define-vms call-name . args)
