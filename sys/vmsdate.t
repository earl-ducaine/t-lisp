(herald vmsdate (env tsys))

;*** Internal Time Constants (Implementation Dependent)
;*** ============================================================
(define-constant *internal-time-units-per-second* 10000000)
(define-constant *internal-base-date* "17 November 1858 00:00:00")

;*** Internal Time and Date routines (Implementation Dependent)
;*** ============================================================
(define (make-internal-time) (make-bytev 8))

(define (get-internal-time itime)
    (vms-i/o *$gettim-xenoid* itime))

(define (internal-date-compare itime1 itime2)
  (cond ((pointer-greater (bref-32 itime1 0) (bref-32 itime2 0)) -1)
        ((pointer-greater (bref-32 itime2 0) (bref-32 itime1 0)) +1)
        (else
         (cond ((pointer-greater (bref-32 itime1 1) (bref-32 itime2 1)) -1)
               ((pointer-greater (bref-32 itime2 1) (bref-32 itime1 1)) +1)
               (else 0)))))


;*** Time Representation Conversion  (Implementation Dependent)
;*** ============================================================

(define (idate->universal-time idate)
    nil)

(define (universal-time->idate time)
    nil)

(define (idate->decoded-time idate)
    nil)

(define (decoded-time->idate time)
    nil)

(define (idate->string idate)
    nil)

(define (string->idate str)
    nil)


;*** Internal Date Comparisons
;*** ============================================================

(define (IDATE= itime1 itime2)
  (fx= 0 (internal-date-compare itime1 itime2)))

(define (IDATE< itime1 itime2)
  (fixnum-negative? (internal-date-compare itime1 itime2)))

(define (IDATE> itime1 itime2)
  (fixnum-positive? (internal-date-compare itime1 itime2)))

(define (IDATE<= itime1 itime2)
  (fixnum-not-positive? (internal-date-compare itime1 itime2)))

(define (IDATE= itime1 itime2)
  (fixnum-not-negative? (internal-date-compare itime1 itime2)))


;*** File dates                      (Implementation Dependent)
;*** ============================================================

(define (file-creation-idate   file|stream)
    nil)
(define (file-backup-idate     file|stream)
    nil)
(define (file-expiration-idate file|stream)
    nil)
(define (file-revision-idate   file|stream)
    nil)
(define (file-revision-number  file|stream)
    nil)


;*** Performance Metering Routines   (Implementation Dependent)
;*** ============================================================

(define (get-internal-run-time itime)
    (vms-i/o *run-time-xenoid* itime))

(define (get-internal-cpu-time itime)
    (vms-i/o *cpu-time-xenoid* itime))

(define (get-internal-page-faults bv)
    (vms-i/o *page-faults-xenoid* bv))

(define (get-internal-process-statistics bv)
    (vms-i/o *process-stats-xenoid* bv))


;*** CPU identification              (Implementation Dependent)
;*** ============================================================

(define (get-internal-processor-data bv)
    (vms-i/o *get-system-info-xenoid* bv))


;*** Local Host Environment          (Implementation Dependent)
;*** ============================================================
(define Local-Host
  (object (local-host-info self)
    ((Site-Name self)                           ; String
     )
    ((Name self) "Yale-APVAX")                  ; String
    ((Aliases self)                             ; List of Strings
     )
    ((Machine-Type self)                        ; String
     )
    ((Serial-Number self)                       ; Integer
     )
    ((Machine-Configuration self)               ; String
     )
    ((Operating-System self)                    ; String
     )
    ((Host-Network-Hardware self)               ; List of Strings
     )
    ((Host-Network-Protocols self)              ; List of Strings
     )
    ((Features self)
     )
    ;; Other Host Dependent Operations
    ))
