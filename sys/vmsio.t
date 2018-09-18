(HERALD VMSIO
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; VMS/RMS interface

;*** Known problems
;*** ======================================================================
;*** - Need to check whether buffer was large enough in CHANNEL_GET.
;*** - The buffer problem needs a better solution.


;;; Error handling stuff

;*** VMS PREDICATES
;*** =======================================================================
(define-integrable (VMS-SUCCESS? status)  (fxN= 0 (fixnum-logand 1 status)))
(define-integrable (VMS-INFO? status)     (fxN= 0 (fixnum-logand 3 status)))
(define-integrable (VMS-WARNING? status)  (fxN= 0 (fixnum-logand 0 status)))
(define-integrable (VMS-ERROR? status)    (fxN= 0 (fixnum-logand 2 status)))
(define-integrable (VMS-SEVERE-ERROR? status)
    (fxN= 0 (fixnum-logand 4 status)))

(define-integrable (VMS-EOF? status)
    (cond ((fx= 98938 status) *eof*)
          (else nil)))

;*** (VMS-STATUS->STRING status)
;*** =======================================================================
;*** Return a string which corresponds to status. *Conses.
;***
(define (vms-status->string status)
     (let* ((buffer (%make-string 128))
            (code (xcall *vms-error-xenoid* (fixnum->pointer status) buffer)))
       (cond ((vms-success? code) buffer)
             (else (error "from *VMS-error-xenoid* - status=~x" code)))))
               

(lset *last-xenoid*     nil)    ; Holds last xenoid called
(lset *last-vms-status* nil)    ; Holds last status returned from a call-xenoid

;*** (LAST-VMS-STATUS)
;*** =======================================================================
;*** A settable object which holds the last status code returned by a
;*** xenoid.
(define last-vms-status
      (object (lambda ()
                (format () "from VMS/RMS ~%        ~s returned ~s ~s"
                           *last-xenoid*
                           *last-vms-status*
                           (vms-status->string *last-vms-status*)))
              ((setter self)
               (lambda (xenoid status)
                  (set *last-xenoid*     xenoid)
                  (set *last-vms-status* status)))))


;*** (VMS-CHECK-I/O-STATUS status:FIXNUM)
;*** =======================================================================
;*** Check the status returned by an I/O xenoid.  If the call was successful
;*** then return NIL.  Otherwise, save the status. If an EOF was encountered
;*** then return *eof*, otherwise, ERROR.
;*** ?? This should handle rms completion codes
;***
(define (vms-check-i/o-status status)
    (cond ((vms-success? status) nil)
          ((vms-eof? status))
          (else (error (last-vms-status)))))


(comment  ; is this a good idea
    ;*** (VMS-CHECK-STATUS status)
    ;*** =======================================================================
    ;*** Check the status returned by a xenoid and take appropriate action.
    ;*** Is this too general??
    ;***
    (define (vms-check-status status)
        (cond ((vms-success? status) (*vms-success-handler* status))
              ((vms-info?    status) (*vms-info-handler*    status))
              ((vms-warning? status) (*vms-warning-handler* status))
              ((vms-error?   status) (*vms-error-handler*   status))
              (else (error "bad status in VMS-CHECK-STATUS ~x" status))))
     
     
    ;;; The STANDARD STATUS HANDLERS.
    ;*** ========================================================================
    (lset *VMS-SUCCESS-HANDLER* (lambda (status) T))
    (lset *VMS-INFO-HANDLER*    (lambda (status) T))
    (lset *VMS-WARNING-HANDLER*
          (lambda (status)
            (error "VMS warning ~a" (vms-status->string status))))
    (lset *VMS-ERROR-HANDLER*
          (lambda (status)
            (error "from VMS - ~a" (vms-status->string status))))
)

;;;  LOCAL CHANNEL I/O ROUTINES
;;;  From here on use VMS-CALL or VMS-I/O

(define-constant %%input-channel  0)
(define-constant %%output-channel 1)
(define-constant %%append-channel 2)


;*** (CONVERT-MODE mode)
;*** =====================================================================
;*** Convert T mode keyword list into a value for *channel-open-xenoid*
(define (convert-mode mode)
    (cond ((memq? 'in     mode) %%input-channel)
          ((memq? 'out    mode) %%output-channel)
          ((memq? 'append mode) %%append-channel)
          (else
           (error "bad mode list ~s in convert-modes" mode))))

;;;; CHANNEL I/O

(define (vm-buffer-writec b ch)
  (let* ((len     (string-length b))
         (new-len (fx+ len 1)))
    (if (fx> new-len 126) (error "buffer overflow ~s" b))
    (set (string-length b) new-len)
    (set (nthchar b len) ch)))

(define (vm-buffer-writes b s)
  (let* ((len     (string-length b))
         (new-len (fx+ len (string-length s))))
    (if (fx> new-len 126) (error "buffer overflow ~s" b))
    (set (string-length b) new-len)
    ;; horror!  this conses!
    (string-replace (string-nthtail b len) s (string-length s))))

(lset *get-buffer*     (lambda () (%make-string 126)))
(lset *buffer-writec*  vm-buffer-writec)
(lset *buffer-writes*  vm-buffer-writes)

;*** CHANNEL ACCESSORS
;*** =====================================================================
;*** These will only work with IN or OUT channels not IN&OUT
;***
(define-integrable (CHAN-BUFFER      chan)(xref chan 0))
(define-integrable (CHAN-PTR         chan)(xref chan 1))
(define-integrable (CHAN-FAB         chan)(xref chan 2))
(define-integrable (CHAN-MODE        chan)(xref chan 3))
(define-integrable (CHAN-SIZE        chan)(string-length (chan-buffer chan)))
(define-integrable (BUFFER-COUNT     buf) (string-length buf))
(define-integrable (BUFFER-EMPTY?    buf) (fx= (string-length buf) 0))
(define-integrable (SET-BUFFER-EMPTY buf) (set (string-length buf) 0))

(define-integrable (RESET-CHAN-PTR   chan)
;*** =====================================================================
;*** really just want to reinitialize the pointer but...
    (block0
     (chopy! (chan-ptr chan) (chan-buffer chan))
     (set (string-length (chan-ptr chan)) 0)))


;*** CHANNEL HANDLER
;*** =====================================================================
(define handle-channel
    (%handler ((print-type-string self) "Channel")))


;*** (MAKE-CHANNEL xenoid mode)
;*** =====================================================================
;***    ?? use get-buffer and do ensure buffer
(define (make-channel xenoid mode)
    (let ((chan (make-extend-n *channel-template* 4))
          (buf  (*get-buffer* 510)))
      (set (xref chan 0) buf)
      (set (xref chan 1) (chopy buf))
      (set (xref chan 2) (xenoid-pointer xenoid)) ;Addr of the VMS record
      (set (xref chan 3) mode)
      (reset-chan-ptr chan)
      chan))


;*** (CHANNEL? chan)
;*** =====================================================================
(define-integrable (channel? chan)
  (and (extend? chan) (eq? (extend-template chan) *channel-template*)))
       

;*** (INPUT-CHANNEL? chan)
;*** =====================================================================
(define-integrable (input-channel? chan)
  (and (extend? chan)
       (eq? (extend-template chan) *channel-template*)
       (fx= (chan-mode chan) %%input-channel)))


;*** (OUTPUT-CHANNEL? chan)
;*** =====================================================================
(define-integrable (output-channel? chan)
  (and (extend? chan)
       (eq? (extend-template chan) *channel-template*)
       (or (fx= (chan-mode chan) %%output-channel)
           (fx= (chan-mode chan) %%append-channel))))

;*** (TTY-CHANNEL? chan)
;*** =====================================================================
(define-integrable (tty-channel? chan)
    (cond ((eq? *ttyin-channel* chan))
          ((eq? *ttyout-channel* chan))
          (else
           (fxN= 1 (xcall *channel-istty-xenoid* (chan-fab chan))))))


;*** (CHANNEL-OPEN pathname mode . size)
;*** =====================================================================
;*** This should have a way of handling the channel size.
;***  *channel-open-xenoid* should probably return the channel size??
;***
(define (channel-open pathname mode)
     (let* ((xeno   (make-xenoid nil))
            (mode   (convert-mode mode))
            (status (xcall *channel-open-xenoid*
                           pathname
                           (fixnum->pointer mode)
                           xeno)))
       (cond ((vms-success? status) (make-channel xeno mode))
             (else nil))))


;*** (CHANNEL-OPEN-FILENAME fname mode)
;*** =====================================================================
;***
(define (channel-open-filename  fname mode)
    (channel-open (filename->string fname) mode))


;*** (CHANNEL-READC chan)
;*** =====================================================================
;***
(define (channel-readc chan)
  (let ((ptr (chan-ptr chan)))
    (cond ((and (buffer-empty? ptr) (channel-read-line-1 chan)))
          (else (block0 (string-head  ptr)
                        (string-tail! ptr))))))


;*** (CHANNEL-READ-LINE chan)
;*** =====================================================================
;***
(define (channel-read-line chan)
  (let ((ptr (chan-ptr chan)))
    (cond ((and (buffer-empty? ptr) (channel-read-line-1 chan)))
          (else (block0 (copy-string ptr)
                        (set (string-length ptr) 0))))))


;*** (CHANNEL-READ-LINE-1 CHAN)
;*** =====================================================================
;*** Returns *EOF* if eof encountered otherwise nil.
;***
(define (channel-read-line-1 chan)
    (let ((ptr (chan-ptr chan)))
      (reset-chan-ptr chan)
      (block0
       (if (eq? chan *ttyin-channel*)
           (vms-i/o *tty-getstring-xenoid* ptr)
           (vms-i/o *channel-get-xenoid* (chan-fab chan) ptr))
       (*buffer-writec* ptr #\NEWLINE))))


;*** (CHANNEL-WRITEC chan c)
;*** =====================================================================
(define (channel-writec chan c)
    (cond ((char= c #\NEWLINE) (channel-newline chan))
          ((eq? chan *ttyout-channel*)
           (vms-i/o *tty-putchar-xenoid* (char->pointer c)))
          (else (*buffer-writec* (chan-ptr chan) c)))
    T)

;*** (CHANNEL-WRITES chan s)
;*** =====================================================================
(define (channel-writes chan str)
    (cond ((eq? chan *ttyout-channel*)
           (vms-i/o *tty-putstring-xenoid* str))
          (else (*buffer-writes* (chan-ptr chan) str)))
    T)


;*** (CHANNEL-FORCE-OUTPUT chan)
;*** =====================================================================
;***
(define (channel-force-output chan)
    (cond ((eq? chan *ttyout-channel*))
          (else (let ((ptr (chan-ptr chan)))
                  (cond ((not (buffer-empty? ptr))
                         (vms-i/o *channel-put-xenoid* (chan-fab chan) ptr)
                         (set-buffer-empty ptr)))
                  (vms-i/o *channel-force-xenoid* (chan-fab chan)))))
    T)


;*** (CHANNEL-NEWLINE chan)
;*** =====================================================================
;***
(define (channel-newline chan)
    (cond ((eq? chan *ttyout-channel*)
           (vms-i/o *tty-putchar-xenoid* (char->pointer #\RETURN))
           (vms-i/o *tty-putchar-xenoid* (char->pointer #\LINEFEED)))
          (else
           (vms-i/o *channel-put-xenoid* (chan-fab chan) (chan-ptr chan))
           (reset-chan-ptr chan)))
    T)
    

;*** (CHANNEL-CLOSE chan)
;*** =====================================================================
;***
(define (channel-close chan)
   (cond ((or (eq? chan *ttyout-channel*) (eq? chan *ttyout-channel*)))
         (else (if (output-channel? chan) (channel-force-output chan))
               (vms-i/o *channel-close-xenoid* (chan-fab chan))
               (release-buffer (chan-buffer chan))
               (set chan nil)))
    T)


(channel-newline *ttyout-channel*)     ; Just for fun.  - Is this fun?


;;; FILE SYSTEM ROUTINES

(define-constant %%file-not-found 98962)


;*** (LOCAL-PROBE-GENERATION fname)
;*** =====================================================================
;***
(define (local-probe-generation fname)
  (with-buffer (fullname 62)
    (local-file-probe (filename->string fname) fullname)))


;*** (LOCAL-FILE-PROBE fname fullname)
;*** =====================================================================
;***
(define (local-file-probe fname fullname)
  (let* ((gen    (make-xenoid nil))
         (status (xcall *file-probe-xenoid* fname fullname gen)))
    (cond ((vms-success? status) (xenoid-pointer gen))
          ((fx= status %%file-not-found) nil)
          (else (error (last-vms-status))))))


;*** (LOCAL-FILE-EXISTS? fname)
;*** =====================================================================
;***
(define local-file-exists? local-probe-generation)


;;; MATH ROUTINES

;*** (make-math-proc xenoid id)
;*** =====================================================================
;***
(define (make-math-proc xenoid id)
    (object (lambda (x)
              (let ((x (check-arg flonum? x id)))
                (call-xenoid-yielding-flonum xenoid (pointer-address x))))
            ((identification self) id)))

(DEFINE ACOS       (MAKE-MATH-PROC *MTH$DACOS*      'ACOS))
(DEFINE ASIN       (MAKE-MATH-PROC *MTH$DASIN*      'ASIN))
(DEFINE ATAN       (MAKE-MATH-PROC *MTH$DATAN*      'ATAN))

(DEFINE ATANH      (MAKE-MATH-PROC *MTH$DATANH*      'ATANH))

(DEFINE COSH       (MAKE-MATH-PROC *MTH$DCOSH*       'COSH))
(DEFINE SINH       (MAKE-MATH-PROC *MTH$DSINH*       'SINH))
(DEFINE TANH       (MAKE-MATH-PROC *MTH$DTANH*       'TANH))
                                                      
(DEFINE COS        (MAKE-MATH-PROC *MTH$DCOS*       'COS))
(DEFINE SIN        (MAKE-MATH-PROC *MTH$DSIN*       'SIN))
(DEFINE TAN        (MAKE-MATH-PROC *MTH$DTAN*       'TAN))
(DEFINE EXP        (MAKE-MATH-PROC *MTH$DEXP*        'EXP))
(DEFINE LOG        (MAKE-MATH-PROC *MTH$DLOG*        'LOG))
(DEFINE LOG10      (MAKE-MATH-PROC *MTH$DLOG10*      'LOG10))
(DEFINE LOG2       (MAKE-MATH-PROC *MTH$DLOG2*       'LOG2))
(DEFINE RANDOM     (MAKE-MATH-PROC *MTH$RANDOM*      'RANDOM))
(DEFINE SQRT       (MAKE-MATH-PROC *MTH$DSQRT*       'SQRT))

;*** Proceedures of N flonum Aargs
;(DEFINE SINCOS     (MAKE-MATH-PROC *MTH$DSINCOS*    'SINCOS))

;*** Complex proceedures 2 Aargs
;(DEFINE MTH$CDABS  (MAKE-MATH-PROC *MTH$CDABS*       'MTH$CDABS))
;(DEFINE MTH$DIMAG  (MAKE-MATH-PROC *MTH$DIMAG*       'MTH$IMAG))
;(DEFINE MTH$DREAL  (MAKE-MATH-PROC *MTH$DREAL*       'MTH$REAL))
;(DEFINE MTH$CDCOS  (MAKE-MATH-PROC *MTH$CDCOS*       'MTH$CDCOS))
;(DEFINE MTH$CDEXP  (MAKE-MATH-PROC *MTH$CDEXP*       'MTH$CDEXP))
;(DEFINE MTH$CDLOG  (MAKE-MATH-PROC *MTH$CDLOG*       'MTH$CDLOG))
;(DEFINE MTH$CDSIN  (MAKE-MATH-PROC *MTH$CDSIN*       'MTH$CDSIN))
;(DEFINE MTH$CDTAN  (MAKE-MATH-PROC *MTH$CSSIN*       'MTH$CDSIN))
;(DEFINE SINCOS     (MAKE-MATH-PROC *MTH$DSINCOSD*    'SINCOS))
;(DEFINE MTH$CDSQRT (MAKE-MATH-PROC *MTH$CDSQRT*      'MTH$CDSQRT))

;*** 2 Argument proceedures
;(DEFINE ATAND2     (MAKE-MATH-PROC-2 *MTH$DATAND2*   'ATAN))
;(DEFINE MTH$CONJG  (MAKE-MATH-PROC-2 *MTH$CONJG*     'MTH$CONJG))
;(DEFINE MTH$CMPLX  (MAKE-MATH-PROC-2 *MTH$CMPLX*     'MTH$CMPLX))
;;; ... also need power and atan2
;;
;; need reentrrant i/o
;;
;; need functions
;;(file-newer first second chan)
;;
;;
