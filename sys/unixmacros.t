(HERALD UNIXMACROS
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; UNIX Macros


;*** Known problems
;*** ======================================================================
;*** - WITH-BUFFER should be part of buffers package.


;*** (XCALL XENOID . REST)
;*** =======================================================================
;*** This macro is used to handle call-xenoid.
;***
(define-syntax (xcall xenoid . rest)
    `(gc-defer (call-xenoid ,xenoid ,@(reverse  rest))))


;*** (WITH-BUFFER (VAR SIZE) . BODY)
;*** =======================================================================
;*** This macro is used to allocate and release an buffer.
;*** This should use GET-BUFFER if size is nil.
;***
(define-syntax (with-buffer spec . body)
  (let ((gen-get-buffer (lambda () (if (cdr spec)
                                       `(get-buffer-of-size ,(cadr spec))
                                       '(*get-buffer*)))))
    `(let ((,(car spec) ,(gen-get-buffer)))
       (block0 (block ,@body)
               (release-buffer ,(car spec))))))
