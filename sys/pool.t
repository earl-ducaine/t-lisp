(HERALD (TSYS POOL T 34)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;; Dealing with freelists.
;;; Thanks and apologies to Bob Nix.

;;; First, hacks for dealing with a freelist of pairs.

(LSET *PAIR-FREELIST* '())

(DEFINE-INTEGRABLE (CONS-FROM-FREELIST A D)
  (COND ((NULL? *PAIR-FREELIST*)
         (CONS A D))
        (ELSE
         (LET ((CELL *PAIR-FREELIST*))
           (SET *PAIR-FREELIST* (CDR *PAIR-FREELIST*))
           (SET (CAR CELL) A)
           (SET (CDR CELL) D)
           CELL))))

(DEFINE-INTEGRABLE (RETURN-TO-FREELIST CELL)
  (SET (CAR CELL) 'FREE)
  (SET (CDR CELL) *PAIR-FREELIST*)
  (SET *PAIR-FREELIST* CELL)
  T)

(DEFINE (RETURN-LIST-TO-FREELIST L)
  (DO ((L L (BLOCK0 (CDR L) (RETURN-TO-FREELIST L))))
      ((ATOM? L) T)))

;;; Pools are implemented as simple structures.
;;; We ought to be keeping statistics.  Very handy for finding storage leaks.

(DEFINE-STRUCTURE-TYPE %POOL ID GENERATOR FREELIST)
(SET (%POOL-FREELIST (STYPE-MASTER %POOL-STYPE)) '())

(DEFINE (MAKE-POOL ID GENERATOR)
  (LET ((POOL (MAKE-%POOL)))
    (SET (%POOL-GENERATOR POOL) GENERATOR)
    (SET (%POOL-ID        POOL) ID)
    (ADD-TO-POPULATION *POOL-POPULATION* POOL)
    POOL))

(DEFINE (OBTAIN-FROM-POOL POOL)
  (GC-DEFER
   (LET ((FL (%POOL-FREELIST POOL)))
     (COND ((NULL? FL) ((%POOL-GENERATOR POOL)))
           (ELSE
            (BLOCK0 (CAR FL)
                    (SET (%POOL-FREELIST POOL) (CDR FL))
                    (RETURN-TO-FREELIST FL)))))))

(DEFINE (RETURN-TO-POOL POOL THING)
  (GC-DEFER
   (SET (%POOL-FREELIST POOL)
        (CONS-FROM-FREELIST THING (%POOL-FREELIST POOL)))
   T))

(DEFINE-METHODS HANDLE-%POOL
  ((PRINT-TYPE-STRING POOL) (IGNORE POOL) "Pool")
  ((IDENTIFICATION POOL) (%POOL-ID POOL)))

(DEFINE *POOL-POPULATION* (MAKE-POPULATION '*POOL-POPULATION*))

(DEFINE (POOL-PRE-GC-HOOK)
  (SET *PAIR-FREELIST* '())
  (WALK-POPULATION-UNSAFELY (LAMBDA (POOL)
                              (SET (%POOL-FREELIST POOL) '()))
                            *POOL-POPULATION*))

(PUSH *PRE-GC-AGENDA* (CONS 'POOL-PRE-GC-HOOK POOL-PRE-GC-HOOK))

;;; Buffers.

;;; There are ten pools, for buffers of various sizes.
;;;    0    1    2    3     4     5     6     7      8      9
;;;   64  128  256  512  1024  2048  4096  8192  16834  32768

(DEFINE *BUFFER-POOLS* (MAKE-VECTOR 10))

(DEFINE-CONSTANT *SMALLEST-BUFFER-SIZE* 62) ; must be 2 less than a power of 2

(DO ((I 0 (FX+ I 1))
     (N (FX+ *SMALLEST-BUFFER-SIZE* 2) (FX* N 2)))
    ((FX> I 9))
  (VSET *BUFFER-POOLS* I
        (MAKE-POOL `(*BUFFER-POOLS* ,I)
                   (LET ((N-3 (FX- N 3)))
                     (LAMBDA ()
                       (LET ((BUFFER (%MAKE-STRING N-3)))
                         ;; Note that (STRING-TEXT-LENGTH BUFFER) = (- N 2)
                         ;;  because MAKE-STRING adds (at least) one.  This may
                         ;;  change in the future.
                         BUFFER))))))

;;; Return a pool from which one can obtain a buffer whose size is >= N.

(DEFINE (BUFFER-POOL N)
  (COND ((FX<= N *SMALLEST-BUFFER-SIZE*)
         (VREF *BUFFER-POOLS* 0))       ; speed hack for common case
        (ELSE
         (LET ((I (FX- (FIXNUM-HOWLONG (FX+ N 1)) 6)))
           (VREF *BUFFER-POOLS* I)))))

;;; Obtain a buffer.

(DEFINE (GET-BUFFER)
  (LET ((B (OBTAIN-FROM-POOL (VREF *BUFFER-POOLS* 0))))
    (SET (STRING-LENGTH B) 0)
    B))

;;; Obtain a buffer whose size is >= N.

(DEFINE (GET-BUFFER-OF-SIZE N)
  (LET ((B (OBTAIN-FROM-POOL (BUFFER-POOL N))))
    (SET (STRING-LENGTH B) N)
    B))

;;; Release a buffer.

(DEFINE (RELEASE-BUFFER BUFFER)
  ;; If BUFFER isn't a string created by GET-BUFFER and friends then we're
  ;; up shit creek.
  (COND ((NOT (FX= (STRING-BASE BUFFER) 0))
         (ERROR "buffer clobberred~%  ~s" `(RELEASE-BUFFER ,BUFFER)))
        (ELSE
         (RETURN-TO-POOL (BUFFER-POOL (BUFFER-SIZE BUFFER)) BUFFER))))

;;; Makes sure that (BUFFER-SIZE BUFFER) is at least N.  If necessary,
;;; this clobbers BUFFER's text pointer and copies any characters up to
;;; (STRING-LENGTH BUFFER).  (STRING-LENGTH BUFFER) is set to N.

(DEFINE (ENSURE-BUFFER-SIZE BUFFER N)
  (LET ((CURRENT-SIZE (BUFFER-SIZE BUFFER)))
    (COND ((FX< CURRENT-SIZE N)
           (LET ((NEW-BUFFER (GET-BUFFER-OF-SIZE N)))
             (STRING-REPLACE NEW-BUFFER BUFFER (STRING-LENGTH BUFFER))
             ;; Exchange the string texts of the two buffers.
             (GC-DEFER
              (SET (STRING-POINTER NEW-BUFFER)
                   (SWAP (STRING-POINTER BUFFER) (STRING-POINTER NEW-BUFFER))))
             (RELEASE-BUFFER NEW-BUFFER))))
    (SET (STRING-LENGTH BUFFER) N)
    T))                                 ; What do YOU want it to return?

(DEFINE (BUFFER-WRITEC B CH)
  (LET ((LEN (STRING-LENGTH B)))
    (LET ((NEW-LEN (FX+ LEN 1)))
      (IF (FX>= NEW-LEN *SMALLEST-BUFFER-SIZE*) ; horrible speed hack
          (ENSURE-BUFFER-SIZE B NEW-LEN))
      (SET (STRING-LENGTH B) NEW-LEN)
      (SET (NTHCHAR B LEN) CH))))

(DEFINE (BUFFER-WRITES B S)
  (LET ((LEN (STRING-LENGTH B)))
    (LET ((NEW-LEN (FX+ LEN (STRING-LENGTH S))))
      (IF (FX>= NEW-LEN *SMALLEST-BUFFER-SIZE*) ; horrible speed hack
          (ENSURE-BUFFER-SIZE B NEW-LEN))
      (SET (STRING-LENGTH B) NEW-LEN)
      ;; Horror!  This conses!
      (STRING-REPLACE (STRING-NTHTAIL B LEN) S (STRING-LENGTH S)))))
