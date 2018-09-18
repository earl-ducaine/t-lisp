(HERALD VMSFAULT
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Fault and interrupt handling

;;; Hack to allow different fault frame representations on the VAX and 68K.

(define fault-reg
  (object (lambda (frame regnum)
            (let ((val (xref frame regnum)))
              (if (fx= regnum %%sp) (pointer-add val 1) val)))
          ((setter self) set-fault-reg)))

(define (set-fault-reg frame regnum val)
  (xset frame regnum
        (if (fx= regnum %%sp) (pointer-subtract val 1) val)))

(define-constant %%vms-ap 12)

;;;  ?? Are these needed?
(define-constant %%ss$-accvio     12) ;*must be changed
(define-constant %%ss$-artres   1140)
(define-constant %%ss$-break    1044)
(define-constant %%ss$-cmoduser 1060)


;;; Ugh.  Clean this up.
;*** (*EXCEPTION-FAULT* frame type)
;*** ==================================================================
;***
(define (*exception-fault* frame type)
    (ignore type)
    (let* ((msg   (%make-string 126))
           (fault (xcall *excpt-msg-xenoid* msg (fault-reg frame %%ap))))
      (select fault
        ((%%ss$-artres %%ss$-cmoduser)   ;??finish this up
         (bind ((*fault-frame* frame))
           (set *re-enter-on-fault?* t)
           (error msg)
           (not-proceedable)))
              (else
               (fault-breakpoint msg frame)))
      (cleanup-interrupt-frame frame)))


;;; Interrupts

;*** (*INTERUPT-FAULT* frame type)
;*** ==================================================================
;***
(define (*interrupt-fault* frame type)
    (ignore type)
    (enable-ctrl-C)
    (fault-breakpoint "INTERRUPT" frame)
    (cleanup-interrupt-frame frame))

;*** (*STOP-FAULT* frame type)
;*** ==================================================================
;***
(define (*stop-fault* frame type)
    (ignore type)
    (ignore frame)
    (stop)
    (set *re-enter-on-fault?* t)
    (cleanup-interrupt-frame frame))

(define (ENABLE-CTRL-Y)
    (call-xenoid *enable-ctrly-xenoid* *stop-handler*) T)
(define (DISABLE-CTRL-Y)
    (call-xenoid *disable-ctrly-xenoid*) T)
(define (ENABLE-CTRL-C)
    (call-xenoid *enable-ctrlc-xenoid* *interrupt-handler*) T)
(define (DISABLE-CTRL-C)
    (call-xenoid *disable-ctrlc-xenoid*) T)
    

    


;*** (EXCEPTION-MODE)  ?? should be a switch
;*** ==================================================================
;***
(define (exception-mode)
    (call-xenoid *$setexv-xenoid*
                 0
                 (fixnum->pointer 3)
                 (xenoid-pointer *exception-handler*)
                 0)
    t)


;*** (DBG-MODE)  ?? should be a switch
;*** ==================================================================
;***
(define (dbg-mode)
    (call-xenoid *$setexv-xenoid* 0 (fixnum->pointer 3) 0 0)
    t)


;;; No-op for now; make it real later.

(DEFINE (RESET-STACK-GUARD) NIL)


;*** (INITIALIZE-INTERRUPT-HANDLERS)
;*** ==================================================================
;***
(define (initialize-interrupt-handlers)
    (if (not (experimental?)) (exception-mode))
    (disable-ctrl-Y)                           ; First enable in Init_T
    (enable-ctrl-Y)
    (enable-ctrl-C)
    t)
