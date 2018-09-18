(HERALD vmstime (env tsys))

(define *unimplemented*
  (object ()
    ((print self stream)
     (format stream "Not yet implemented."))))

;*** Internal Time
;*** ================================================================
(define %%internal-time-units-per-second%% 1000000000)
(define %%internal-base-date%% *unimplemented*)

(define (make-internal-time) (make-bytev 8))

(define (internal-time? time)
  (and (bytev? time) (fx= (bytev-length time) 8)))

(define (get-internal-time)
    (let ((itime (make-internal-time)))
      (vms-i/o *get-time-xenoid* itime)
      itime))

(define (internal-time-equal? time1 time2)
  (and (internal-time? time1)
       (internal-time? time2)
       (pointer-equal? (xref time1 0) (xref time2 0))
       (pointer-equal? (xref time1 1) (xref time2 1))))

(define (internal-time-less? time1 time2)
  (and (internal-time? time1)
       (internal-time? time2)
       (or (pointer-less? (xref time1 1) (xref time2 1))
	   (pointer-less? (xref time1 0) (xref time2 0)))))

(define (internal-time->string time fmt)
    (with-buffer (buf 62)
      (vms-i/o *time->string-xenoid* time buf (fixnum->pointer fmt))
      (copy-string buf)))

(define (string->internal-time str)
    (let ((time (make-bytev 8)))
      (vms-i/o *time->string-xenoid* str time)
      time))

;;; System independent routines

(define (internal-time-leq? time1 time2)
  (or (internal-time-less?  time1 time2)
      (internal-time-equal? time1 time2)))

(define (internal-time-greater? time1 time2)
  (not (internal-time-leq? time1 time2)))

(define (internal-time-geq? time1 time2)
  (not (internal-time-less? time1 time2)))

(define (internal-time-neq? time1 time2)
  (not (internal-time-equal? time1 time2)))

;*** Time conversion
;*** ==============================================================
;*** Universal time is represented as an integer number of seconds
;*** relative to 00:00 1 January 1900 GMT.
;***
;;; Yuck!  Re-do later
(define (Internal-time->Universal-time Itime)
  (iterate loop1 ((i 6))
    (let ((temp (bref-16 Itime 1)))
      (cond ((fx< i 0) 0)
	    ((fx= temp 0) (loop (fx- i 2)))
	    (else (itime1->Utime1 temp Itime i))))))

(define (Itime1->Utime1 Utime Itime idx)
  (cond ((fx= i 0) (div (+ Utime (bref-16 Itime 0)
			%%internal-time-units-per-second%%)))
	(else
	  (Itime1->Utime1 (+ (ash utime 16) (bref-16 Itime i))
			  idx
			  (fx- i 2)))))

(define (Universal-time->Internal-time Utime)
  (let ((itime (make-internal-time))
	(temp  (* Utime %%internal-time-units-per-second%%)))
    (Utime1->Itime1 temp itime 0)))

(define (Utime1->Itime1 Utime Itime idx)
    (cond ((fx= idx 8) Itime)
	  (else
	    (let ((temp (ash Utime -16)))
	      (bignum-div2 Utime temp (lambda (q r)
					(ignore q)
					(set (bref-16 Itime idx) r)))
	      (Utime1->Itime1 temp Itime (fx+ idx 2))))))

(define (Universal-time->Decoded-time Utime)
  *unimplemented*)

(define (decode-time Utime) (universal-time->decoded-time Utime))

(define (Decoded-time->Universal-time Dtime)
  *unimplemented*)

(define (encode-time Dtime) (decoded-time->universal-time Dtime))

(define (get-universal-time)
  (internal-time->universal-time (get-internal-time)))

(define (get-decoded-time)
  (universal-time->decoded-time (get-universal-time)))


;*** Hacks
;*** =============================================================
;***
(define (date)
  (let ((date-str (internal-time->string (get-internal-time) 0)))
    (set (string-length date-str) 11)
    date-str))

(define (time)
  (internal-time->string (get-internal-time) 1))

(define (date&time)
  (internal-time->string (get-internal-time) 0))


;*** (SLEEP milleseconds)
;*** ==============================================================
;*** Cause the process to hibernate for N milleseconds.
;***
(define (sleep milleseconds)
  (vms-i/0 *sleep-xenoid* (milleseconds->itime milleseconds))
  T)

(define (milleseconds->itime milleseconds)
  (cond ((fixnum? milleseconds)
	 (let ((itime (make-internal-time)))
	   (set (xref itime 0) (fixnum->pointer (fx* milleseconds 10000)))
	   (set (xref itime 1) (fixnum->pointer -1))
	   itime))
	(else (error "argument must be a fixnum."))))

(define (get-process-stats)
  (let ((stats (make-bytev 12)))
    (xcall *process-stats-xenoid* stats)
    stats))

(define (get-cpu-time)
  (let ((stats (get-process-stats)))
    (bref-32 stats 0)))

(define (get-run-time)
  (let ((itime (get-internal-time)))
    (xcall VMS_LIB$SUBX itime *base-run-time* itime))
    itime)

(define (get-page-faults)
  (let ((stats (get-process-stats)))
    (bref-32 stats 4)))

(define (get-process-info)  *unimplemented*)
(define (version)           *unimplemented*)

;*** File dates
;*** =============================================================
;*** The following procedures all return internal times.
;***
(define (file-creation-date filename)
  (let ((date (make-internal-time)))
    (vms-i/o *file-creation-date-xenoid* filename date)
    date))

(define (file-write-date filename)
  (let ((date (make-internal-time)))
    (vms-i/o *file-write-date-xenoid* filename date)
    date))

(define (channel-write-date chan)
  (let ((date (make-internal-time)))
    (vms-i/o *channel-write-date-xenoid* chan date)
    date))
