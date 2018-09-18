(HERALD VMSMACROS
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; VMS Macros

;*** Known problems
;*** ======================================================================
;*** - VMS-I/O doesn't handle RMS completion status.
;*** - WITH-BUFFER should be part of buffers package.
;*** - Is VMS-CHECK-STATUS gratuitous?


;*** (XCALL XENOID . REST)
;*** =======================================================================
;*** This macro is used to handle call-xenoid.
;*** ?? Should use pointer->integer, alas it's not available at this stage of
;***    boot.
;***
(define-syntax (xcall xenoid . rest)
    `(gc-defer
      (let ((status (pointer->fixnum (call-xenoid ,xenoid ,@(reverse! rest)))))
        (set *last-xenoid* ,xenoid)
        (set *last-vms-status* status))))



;*** (VMS-I/O XENOID CHAN . REST)
;*** =======================================================================
;*** This macro is used to handle errors from the VMS/RMS substrate.
;***
(define-syntax (vms-i/o xenoid . rest)
    `(vms-check-i/o-status (xcall ,xenoid ,@rest)))


;**s (VMS-CALL XENOID . REST)
;*** =======================================================================
;*** This macro is used to handle errors from the VMS substrate.
;***
(define-syntax (vms-call xenoid . rest)
    `(vms-check-status (xcall ,xenoid ,@rest)))


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
