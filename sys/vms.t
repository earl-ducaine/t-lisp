(HERALD VMS
        (ENV TSYS (TSYS VMSIO)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; VMS interface


;;; Do interesting stuff.  This happens as the initial read-eval-print
;;; is entered, but before the patch file and init file are loaded.

(define (INITIALIZE-OS-STUFF)
  (lset *command-line*  (static->string *cmd-buf-xenoid* 80))
  (lset *working-directory* (static->string *wdir-buf-xenoid* 64))
  (lset *VMS-USER-NAME* (static->string *username-buf-xenoid* 12))
  (set *comment-on-allocation-behavior?* t)
  (set gc-output       zterminal-output)
  (set *get-buffer*    get-buffer-of-size)
  (set *buffer-writec* buffer-writec)
  (set *buffer-writes* buffer-writes)
  (initialize-interrupt-handlers)
  T)

(define (EXPERIMENTAL?)
  (char= #\X (string-upcase *command-line*)))

(define (THE-T-SYSTEM-DIRECTORY)
  (if (experimental?) 'xtsystem 'tsystem))

(define (THE-INIT-FILE-DIRECTORY) 'sys$login)

(define (static->string xeno max-len)
  (let ((str (chopy "")))
    (set (string-pointer str) (xenoid-pointer xeno))
    (set (string-length  str) max-len)
    (set (string-length  str) (string-posq #\NULL str))
    (copy-string str)))


;+++ This should be parsing the command-line
;+++ should use while here

(comment
      (define (LOCAL-COMMAND-LINE)
          (let* ((s      (copy-string (xenoid-pointer *cmd-line-xenoid*)))
                 (len    (string-length s))
                 (tokens nil))
            (iterate -1- ((begin  (skip-whitespace s begin)))
                (cond ((fx>= begin len) tokens)
                      (else (let ((end (skip-to-whitespace s begin)))
                              (append! tokens (string-slice s begin end))
                              (-1- (skip-whitespace s end))))))))

      (define-integrable (skip-whitespace s pos)
          (let ((len (string-length s)))
            (iterate -1- ((i pos))
                     (cond ((fx= i len) i)
                           ((not (whitespace? (nthchar s i))) i)
                           (else (-1- (fx+ i 1)))))))

      (define-integrable (skip-to-whitespace s pos)
          (let ((len (string-length s)))
            (iterate -1- ((i pos))
                     (cond ((fx= i len) i)
                           ((whitespace? (nthchar s i)) i)
                           (else (-1- (fx+ i 1)))))))
)


(define (LUSER-TYPED-EOF-AT-TOP-LEVEL)
  (format (error-output)
          "** Use ^Y or (STOP) to exit T.~%"))


(lset *SUSPEND-HOOK*            ; User hook for suspending process
      (lambda ()
        (channel-newline *ttyout-channel*)
        (channel-writes *ttyout-channel* "Suspending ...")
        T))

(lset *REENTER-HOOK*            ; User hook for reentering process
      (lambda ()
        (muf-args)
        (channel-writes *ttyout-channel* "Reentering ...")
        (channel-newline *ttyout-channel*)
        T))

;;; Get the args and current default directory from MUF

(define (MUF-ARGS)
    (set *command-line* (static->string *cmd-buf-xenoid* 80))
    (set *working-directory* (static->string *wdir-buf-xenoid* 64)))


;;; STOP suspends, and returns T when TAU is resumed.

(define (STOP)
    (*suspend-hook*)
    (xcall *stop-xenoid*)
    (enable-ctrl-Y)
    (*reenter-hook*)
    T)

;;; EXIT mustn't ever return.

(define (EXIT . foo)
  (dbg-mode)
  (vms-i/o *exit-xenoid* (fixnum->pointer (if (null? foo) 1 (car foo)))))

(define (LOCAL-FS-CREATOR) make-vms-fs)

(lset *PRINT-FLONUMS-KLUDGILY?* t)

;;; SAVE-SYSTEM
;++ Low-mem-area is really the high-mem-area and vice versa.

(define (save-system)
  (gc)
  (if (neq? *the-current-area* *high-mem-area*) (gc))
  (set *top-level*
       (lambda ()
         (set *top-level*
              (lambda ()
                (set *top-level* *standard-top-level*)
                (reinitialize-systems)))
         (reset-chan-ptr *ttyin-channel*)
         (dbg-mode)
         (xcall *save-process-xenoid*)))
  (reset))

;;; sprintf(ptr, "%E!", n)

(define (PRINT-FLONUM-KLUDGILY n stream)
  (with-buffer (b 21)
    (xcall *flonum->string-xenoid* (pointer-address n) b)
    (write-string stream b)))

(define (STRING->FLONUM s)
  (let ((n (fixnum->flonum (no-op 0))))
    (xcall *string->flonum-xenoid* s (pointer-address n))
    n))

(define WD
    (object (lambda ()
               (with-buffer (str 62)
                 (vms-i/o *set-wdir-xenoid* 0 str)
                 (copy-string str)))
            ((setter self)
             (lambda (dir)
                 (check-arg string? dir wd)
                 (with-buffer (old-dir 62)
                   (cond ((vms-success? (xcall *set-wdir-xenoid* dir old-dir))
                          dir)
                         (else (last-vms-status))))))))


(define (get-internal-time)
    (let ((time (make-bytev 8)))
      (vms-i/o *get-time-xenoid* time)
      time))

(define (internal-time->string time fmt)
    (with-buffer (buf 62)
      (vms-i/o *time->string-xenoid* time buf (fixnum->pointer fmt))
      (copy-string buf)))

(define (string->internal-time str)
    (let ((time (make-bytev 8)))
      (vms-i/o *time->string-xenoid* str time)
      time))

(define (date)
  (let ((date-str (internal-time->string (get-internal-time) 0)))
    (set (string-length date-str) 11)
    date-str))

(define (time)
  (internal-time->string (get-internal-time) 1))

(define (date&time)
  (internal-time->string (get-internal-time) 0))


(define (sleep internal-time)
  (vms-i/0 *sleep-xenoid* n)
  T)

(define (get-cpu-time)
  (let ((cpu-time (make-bytev 4)))
    (xcall *cpu-time-xenoid* cpu-time)
    cpu-time))
