(herald untime 
        (env tsys)
        (SYNTAX-TABLE
         (BLOCK (*REQUIRE 'UNIXMACROS '(TSYS UNIXMACROS) *SCRATCH-ENV*)
                (ENV-SYNTAX-TABLE *scratch-env*))))

;*** Internal Time Constants (Implementation Dependent)
;*** ============================================================
(define-constant *internal-time-units-per-second* 1)
(define-constant *internal-base-date* "1 January 1970 00:00:00 GMT")

;*** Internal Time and Date routines (Implementation Dependent)
;*** ============================================================
(define (make-internal-time) (make-bytev 4))

(define (get-internal-time)
  (let ((itime (make-internal-time)))
    (set (bref-pointer itime 0) (xcall *time-xenoid* 0))
    itime))

(define-integrable (internal-time-equal? itime1 itime2)
  (pointer-equal? (bref-pointer itime1 0) (bref-pointer itime2 0)))

(define-integrable (internal-time-less? itime1 itime2)
  (pointer-less? (bref-pointer itime1 0) (bref-pointer itime2 0)))

(define-integrable (internal-time-not-greater? itime1 itime2)
  (or (internal-time-less? itime1 itime2)
      (internal-time-equal?  itime1 itime2)))

(define-integrable (internal-time-not-equal? itime1 itime2)
  (not (internal-time-equal? itime1 itime2)))

(define-integrable (internal-time-greater? itime1 itime2)
  (not (internal-time-not-greater? itime1 itime2)))

(define-integrable (internal-time-not-less? itime1 itime2)
  (not (internal-time-less? itime1 itime2)))

;*** Aliases
(define itime=  internal-time-equal?)
(define itimeN= internal-time-not-equal?)
(define itime<  internal-time-less?)
(define itime>= internal-time-not-less?)
(define itime>  internal-time-greater?)
(define itime<= internal-time-not-greater?)


;*** Time Representation Conversion  (Implementation Dependent)
;*** ============================================================

(define (itime->utime itime)
  (pointer->integer (bref-pointer itime 0)))

(define (utime->itime time)
    nil)

(define (itime->dtime idate)
    nil)

(define (dtime->itime time)
    nil)

(define (itime->string itime)
  (let ((str (asciz->string (xcall *ctime-xenoid* itime))))
    (set (nthchar str 24) #\NULL)
    (set (string-length str) 24)
    str))


(define (string->idate str)
    nil)


;*** Performance Metering Routines   (Implementation Dependent)
;*** ============================================================

(define (get-internal-run-time itime)
    nil)

(define (get-internal-cpu-time itime)
  nil)

(define (get-internal-page-faults bv)
  nil)

(define (get-internal-process-statistics bv)
  nil)


;*** CPU identification              (Implementation Dependent)
;*** ============================================================

(define (get-internal-processor-data bv)
  nil)

(comment
;*** Local Host Environment          (Implementation Dependent)
;*** ============================================================
(define Local-Host
  (object (local-host-info self)
    ((Site-Name self)                           ; String
     nil)
    ((Name self) "Yale-APVAX")                  ; String
    ((Aliases self)                             ; List of Strings
     nil)
    ((Machine-Type self)                        ; String
     nil)
    ((Serial-Number self)                       ; Integer
     nil)
    ((Machine-Configuration self)               ; String
     nil)
    ((Operating-System self)                    ; String
     nil)
    ((Host-Network-Hardware self)               ; List of Strings
     nil)
    ((Host-Network-Protocols self)              ; List of Strings
     nil)
    ((Features self)
     nil)
    ;; Other Host Dependent Operations
    ))
)


;*** Timer stuff

;*** (USLEEP N)
;*** =========================================================================
;*** Sleep for N micro-secs.
;***
(define (usleep n)
  (if (pointer-less? (xcall *usleep-xenoid* 0 (fixnum->pointer n))
	             (fixnum->pointer 0))
      (unix-error)
      T))
