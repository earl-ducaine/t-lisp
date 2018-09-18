(Herald unfile
        (env tsys)
        (syntax-table
         (block (*require 'unixmacros '(tsys unixmacros) (the-environment))
                (env-syntax-table (the-environment)))))

;*** FILE HANDLING ROUTINES
;*** ===========================================================
;*** Any arguments with the prefix FNAME are coerced into
;*** filenames.  
;*** ...

;*** (->FILENAME-WITH-DEFAULTS name fs dir . type gen)
;*** ==========================================================================
;*** This should be in FS, but for the moment...
;***
(define (->filename-with-defaults name fs dir . rest)
  (let* ((fname   (expand-filename name))
         (xfs     (filename-fs   fname))
         (xdir    (filename-dir  fname))
         (xname   (filename-name fname))
         (xtype   (filename-type fname))
         (xgen    (filename-generation fname))
         (type    (car rest))
         (gen     (cadr rest)))
    (let ((fs   (if xfs   xfs   fs))
          (dir  (if xdir  xdir  dir))
          (type (if xtype xtype type))
          (gen  (if xgen  xgen  gen)))
      (make-filename fs dir xname type gen))))

(define-local-syntax (unix-check val)
  (if (fxN= val 0)
      (unix-error)
      T))

(define (unix-error)
  (error "from UNIX substrate - ~a~%" (unix-error-string)))

(define (make-stat-block) 
  (make-bytev 64))

(define-integrable (st_size stat-block)
  (bref-32 stat-block 20))

(define-integrable (st_mtime stat-block)
  (bref-pointer stat-block 32))

; ++ should use unsigned bref-16-U

(define-integrable (st_mode stat-block)
  (fixnum-logand #x0FFFF (bref-16 stat-block 8)))


;*** FILE-GET-STAT (internal-routine)
;*** ===========================================================
;*** Returns a bytev with the stat-block associated with PATH
;*** or NIL if the file is not found.
;***
(define (file-get-stat path stat-block)
  (pointer-equal? 0 (xcall *stat-xenoid* (->ipathname path) stat-block)))


;*** EXPAND-FILENAME
;*** ===========================================================
;*** Returns an expanded Unix filename.
;***
(define (expand-filename fname)
  (->filename (->pathname fname)))

(define (->pathname fname)
  (with-buffer (buf 126)
   (let ((str (filename->string (->filename fname))))
     (xcall *expand-path-xenoid* (string-pointer str) (string-pointer buf))
     (asciz->string (string-pointer buf)))))

(define-integrable (->ipathname fname)             ;; internal use only
  (string-pointer (->pathname fname)))

;*** There should be a pool of stat-blocks, but for now ...

(define (local-probe-generation fname)
  (cond ((local-file-exists? fname) 0)  ; losing unix.
        (else nil)))

;*** LOCAL-FILE-EXISTS?
;*** ===========================================================
;***
(define (local-file-exists? fname)
  (file-get-stat fname (make-stat-block)))

;*** FILE-PROBE
;*** ===========================================================
;*** Returns an expanded filename if the file exists, otherwise nil.
;***
(define (file-probe fname)
  (let ((fname (expand-filename fname)))
    (gc-defer
     (cond ((file-get-stat fname (make-stat-block))
            (->filename-with-defaults fname (local-fs) (working-directory)))
           (else nil)))))


;*** FILE-LENGTH
;*** ===========================================================
;***
(define (file-length fname)
  (let ((stat-block (make-stat-block)))
    (cond ((file-get-stat fname stat-block)
           (st_size stat-block))
          (else (unix-error)))))


;*** FILE-WRITE-DATE
;*** ===========================================================
;***
(define (file-write-date fname)
  (let ((stat-block (make-stat-block)))
    (cond ((file-get-stat fname stat-block)
           (let ((itime (make-internal-time)))
             (set (bref-pointer itime 0) (st_mtime stat-block))
             itime))
            (else (unix-error)))))


;*** FILE-DIRECTORY?
;*** ===========================================================
;***
(define (file-directory? fname)
  (let ((stat-block (make-stat-block)))
    (file-get-stat fname stat-block)
    (fxN= 0 (fixnum-logand (st_mode stat-block) #o040000))))


;*** FILE-MOVE
;*** ===========================================================
;***
;+++ bogus version for the moment, doesn't handle networks.
(define (file-move from to)
  (gc-defer
    (let ((val (xcall *rename-xenoid* (->ipathname from) (->ipathname to))))
      (if (pointer-not-equal? 0 val)
          (unix-error)
          T))))


;*** FILE-DELETE
;*** ===========================================================
;***
(define (file-delete fname)
    (if (pointer-not-equal? 0 (xcall *unlink-xenoid* (->ipathname fname)))
        (unix-error)
        T))

;*** FILE-NEWER?
;*** ===========================================================
;***
(define (file-newer? fname1 fname2)
  (itime> (file-write-date fname1) (file-write-date fname2)))


;*** WORKING-DIRECTORY
;*** ===========================================================
;***
(define working-directory
  (object (lambda ()
            (with-buffer (buf 62)
              (let ((ptr (xcall *getwd-xenoid* (string->asciz buf))))
                (cond ((pointer-equal? 0 ptr)
                       (unix-error))
                      (else
                       (asciz->string ptr))))))
    ((setter self)
     (lambda (path)
       (cond ((pointer-equal? 0 (xcall *chdir-xenoid* (->ipathname path)))
               path)
             (else
              (unix-error)))))))


;*** HOME-DIRECTORY
;*** ===========================================================
;***
(define (home-directory)
  (unix-getenv "HOME"))
