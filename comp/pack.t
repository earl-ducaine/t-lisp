(HERALD PACK
        (ENV TCOMP))

;;; Copyright (c) 1981 Lawrence Livermore Labroatories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Sorting of temporary names by priority

(DEFINE (SORT-TNS TNS)
  (SORT! TNS
         ;; This function returns true iff TN1 should definitely
         ;;  be dealt with before TN2.
         (LAMBDA (TN1 TN2)
           (COND ((AND (TN-ISLOC TN1) (NOT (TN-ISLOC TN2))) T)
                 ((AND (TN-ISLOC TN2) (NOT (TN-ISLOC TN1))) NIL)
                 ((AND (EQ? (TN-WANTLOC TN1) (TN-WANTLOC TN2))
                       (ALIKEV? (TN-ISLOC TN1) (TN-ISLOC TN2))) ;?  EQUIV?
                  (FX> (TN-WEIGHT TN1) (TN-WEIGHT TN2)))
                 (T (LET ((TIM1 (TN-LOC-IMPORTANCE TN1))
                          (TIM2 (TN-LOC-IMPORTANCE TN2)))
                      (COND ((FX= TIM1 TIM2)
                             (FX< (TN-ID TN1) (TN-ID TN2)))
                            (T (FX> TIM1 TIM2)))))))))

(DEFINE (TN-LOC-IMPORTANCE TN)
  (CASE (TN-WANTLOC TN)
    ((ANY) 0)
    ((POINTER-MEM SCRATCH-MEM) 2)
    ((POINTER-REG SCRATCH-REG) 3)
    ((STACK) 6)))
;;;; Packing of temporary names into registers

(DEFINE-MACRO (MAKE-BUCKET ISLOC)
  `(CONS* ,ISLOC 'AVAILABLE '()))

;;; ... PACK-TNS

;;; The AVAILABLE flag in each bucket is not used right now.
;;; It is there to provide for later installing a hairier packing algorithm.

(DEFINE (PACK-TNS NODE)
  (DO ((TNS *TNS* (CDR TNS))
       (ORDER 0 (FX+ ORDER 1)))
      ((NULL? TNS))
      ----
      (SETF (TN-ORDER (CAR TNS)) ORDER)
      ---)
  (BIND ((*SCRATCH-REGS* (MAPCAR (LAMBDA (X) (MAKE-BUCKET X))
                                 *SCRATCH-REG-NAMES*))
         (*POINTER-REGS* (MAPCAR (LAMBDA (X) (MAKE-BUCKET X))
                                 *POINTER-REG-NAMES*))
         (*SCRATCH-MEM* (LIST (MAKE-BUCKET 0))) ;scratch memory
         (*POINTER-MEM* (LIST (MAKE-BUCKET 0))))        ;pointer memory
    (PACK-TNS-1 NODE)))

(DEFINE (PACK-TNS-1 NODE)
  (DECLARE (SPECIAL *SCRATCH-REGS* *POINTER-REGS* ;*RTS*
                    *SCRATCH-MEM*  *POINTER-MEM*))
  (WALK PACK-TN *TNS*)
  (SETQ *SCRATCH-MEM* (TRIM-TN-AREA *SCRATCH-MEM*))
  (SETQ *POINTER-MEM* (TRIM-TN-AREA *POINTER-MEM*))
  (COND ((CTRACE? 'PACK (FORMAT NIL "There are ~D TN's.~%See TN packing"
                                (LENGTH *TNS*)))
         (DUMP-TN-INFO NIL)))
  (LET ((A (LAMBDA-REGION (NODE-FORM NODE))))
    (SETF (REGION-POINTER-MEM-SIZE A)   ; MAX because of (LAMBDA (X ()) X)
          (MAX (LENGTH *POINTER-MEM*) (LENGTH (LAMBDA-VARS (NODE-FORM NODE)))))
    (SETF (REGION-SCRATCH-MEM-SIZE A) (LENGTH *SCRATCH-MEM*))))

(DEFINE (PACK-TN TN)
  (DECLARE (SPECIAL *SCRATCH-REGS* *POINTER-REGS*       ;*RTS*
                    *SCRATCH-MEM* *POINTER-MEM*))
  (COND ((OR (NULL? (TN-LONFC TN))
             (NULL? (TN-LONLU TN))
             (NULL? (TN-FONFC TN))
             (NULL? (TN-FONLU TN)))
         (BUGLET ((*TN* TN))
                 "lifetime information is missing for a TN"
                 "ignore the fact and lose utterly")))
  (CASE (TN-WANTLOC TN)
    ;((RT)
    ; (OR (PACK-RT TN *RTS*)
        ; (BUGLET ((*TN* TN))
        ;        "couldn't pack a TN into an RT register"
        ;        "what to do?")))
    ((STACK)
     (SETF (TN-REASON TN) (TN-WANTLOC TN)))
    ((SCRATCH-REG)
     (OR (PACK-FIXED TN *SCRATCH-REGS* 'SCRATCH-REG)
;?       (PACK-FIXED TN *RTS* 'SCRATCH-REG)
;?       (PACK-FIXED TN *MORE-SCRATCH-REGS* 'SCRATCH-REG)
         ;;? this bug report is new
         (BUGLET ((*TN* TN))
                 "couldn't pack a TN into a scratch register"
                 "what to do?")))
    ((POINTER-REG)
     (OR (PACK-FIXED TN *POINTER-REGS* 'POINTER-REG)
         ;;? this bug reprt is new
         (BUGLET ((*TN* TN))
                 "couldn't pack a TN into a pointer register"
                 "what to do?")))
    ((SCRATCH-MEM)
     (OR (AND (NULL? (TN-ISLOC TN))
              (TRY-PREFERRED-PACK TN *SCRATCH-MEM* 'SCRATCH-MEM NIL))
         (PACK-EXPAND TN *SCRATCH-MEM* 'SCRATCH-MEM)))
    ((POINTER-MEM)
     (OR (AND (NULL? (TN-ISLOC TN))
              (TRY-PREFERRED-PACK TN *POINTER-MEM* 'POINTER-MEM T))
         (PACK-EXPAND TN *POINTER-MEM* 'POINTER-MEM)))
    ;;? what about RTPREF case; are we serious about the S-1
    ((ANY)
     (COND ((NOT (TN-PTRP TN))
            (OR ;(TRY-PREFERRED-PACK TN *RTS* 'SCRATCH-REG NIL)
                (TRY-PREFERRED-PACK TN *SCRATCH-REGS* 'SCRATCH-REG NIL)
                (TRY-PREFERRED-PACK TN *SCRATCH-MEM* 'SCRATCH-MEM NIL)
                (TRY-TO-PACK TN *SCRATCH-REGS* 'SCRATCH-REG)
                ;(TRY-TO-PACK TN *RTS* 'SCRATCH-REG)
                (PACK-EXPAND TN *SCRATCH-MEM* 'SCRATCH-MEM)))
           ((EQ? (TN-PTRP TN) 'MAYBE)
            (OR ;(TRY-PREFERRED-PACK TN *RTS* 'SCRATCH-REG NIL)
                (TRY-PREFERRED-PACK TN *POINTER-REGS* 'POINTER-REG T)
                (TRY-PREFERRED-PACK TN *SCRATCH-REGS* 'SCRATCH-REG NIL)
                (TRY-PREFERRED-PACK TN *POINTER-MEM* 'POINTER-MEM NIL)
                (TRY-PREFERRED-PACK TN *SCRATCH-MEM* 'SCRATCH-MEM NIL)
                ;(TRY-TO-PACK TN *RTS* 'SCRATCH-REG)
                (TRY-TO-PACK TN *POINTER-REGS* 'POINTER-REG)
                (TRY-TO-PACK TN *SCRATCH-REGS* 'SCRATCH-REG)
                (PACK-EXPAND TN *POINTER-MEM* 'POINTER-MEM)))
           (ELSE
            (OR (TRY-PREFERRED-PACK TN *POINTER-REGS* 'POINTER-REG T)
                (TRY-PREFERRED-PACK TN *POINTER-MEM* 'POINTER-MEM NIL)
                (TRY-TO-PACK TN *POINTER-REGS* 'POINTER-REG)
                (PACK-EXPAND TN *POINTER-MEM* 'POINTER-MEM)))))))

(DEFINE (TRIM-TN-AREA AREA)
  (DO ((A (REVERSE! AREA) (CDR A)))
      ((OR (NULL? A) (NOT (NULL? (CDDAR A))))
       (REVERSE! A))))

(DEFINE (PACK-FIXED TN AREA NAME)
  (COND ((NULL? (TN-ISLOC TN))
         (OR (TRY-PREFERRED-PACK TN AREA NAME NIL)
             (TRY-TO-PACK TN AREA NAME)))
        (T (LET ((PLACE (MEMASSQ (TN-ISLOC TN) AREA)))
             (AND PLACE (PACK-ATTEMPT TN PLACE NAME))))))

(DEFINE (PACK-EXPAND TN AREA NAME)
  (COND ((NULL? (TN-ISLOC TN))
         (OR (TRY-TO-PACK TN AREA NAME)
             (LET ((N (TN-SIZE TN)))
               ;;? This DO is somewhat changed
               (D0 ((J N (FX- J 1))
                    (K (FX+ (FX- (LENGTH AREA) N) 1) (FX+ K 1)))
                   ((FX= J 0)
                    (BUGLET ((*TN* TN) (*AREA* AREA))
                            "failed to pack a TN after expanding area by size of TN"
                            "this can't possibly happen, right?")
                    NIL)
                 ----
                 (LET ((H (LASTCDR AREA)))
                   (SET (CDR H) (LIST (MAKE-BUCKET (FX+ (CAAR H) 1)))))
                 (IF (TRY-TO-PACK TN (+NTHCDR AREA (MAX 0 K)) NAME)
                     (RETURN T))
                 ---))))
        ;;? Steele has a ">" instead of ">="
        ((FX>= (TN-ISLOC TN) (LENGTH AREA))
         (PACK-EXPAND TN
                      (APPEND! AREA (LIST (MAKE-BUCKET (LENGTH AREA))))
                      NAME))
        ((PACK-ATTEMPT TN (+NTHCDR AREA (TN-ISLOC TN)) NAME)
         (SETF (TN-REASON TN) 'SPECIFIC-LOCATION)
         T)
        (T (BUGLET ((*TN* TN) (*AREA* AREA))
                   "attempt to pack a TN into a specific place lost"
                   "who knows?  This is completely ridiculous!")
           NIL)))
                                  
(DEFINE (TRY-PREFERRED-PACK TN AREA NAME STACKOK)
  (TRY-PREFERRED-PACK-1 TN AREA NAME STACKOK T (TN-PREFERENCES TN)))

(DEFINE (TRY-PREFERRED-PACK-1 TN AREA NAME STACKOK INDIRECT PREFS)
  (D0 ((PTNS PREFS (CDR PTNS)))
      ((NULL? PTNS)
       ;; All attempts to preference have failed.  Try indirection?
       (IF INDIRECT
           (D0 ((P (TN-PREFERENCES TN) (CDR P)))
               ((NULL? P) NIL)
             ----
             (AND (NULL? (TN-ISLOC (CAR P)))
                  (TRY-PREFERRED-PACK-1
                   TN AREA NAME STACKOK NIL (TN-PREFERENCES (CAR P)))
                  (RETURN T)))
         NIL))
    ----
    (LET ((PTN (CAR PTNS)))
      (COND ((AND (EQ? (TN-WANTLOC PTN) NAME)
                  (NOT (NULL? (TN-ISLOC PTN)))
                  (D0 ((A AREA (CDR A)))
                      ((NULL? A)
                       ;; Hairy code to switch RT & scratch-reg areas flushed.
                       (BUGLET ((*PTN* PTN) (*TN* TN) (*AREA* AREA) (*NAME* NAME))
                               "TN in *PTN* claims to be allocated to a ~
                                non-existent slot"
                               "will not try to put *TN* in the same slot!")
                       NIL)
                    ----
                    (IF (ALIKEV? (CAAR A) (TN-ISLOC PTN))       ; EQUIV?
                        (RETURN (PACK-ATTEMPT TN A NAME)))
                    ---))
             (SETF (TN-REASON TN) PTN)
             (RETURN T))
            ((AND STACKOK
                  (EQ? (TN-WANTLOC PTN) 'STACK)
                  ;;? Steele does not have this check in his version
                  (NULL? (TN-VAR TN))
                  (OR (FX< (TN-LONLU TN) (TN-LONFC PTN))
                      (FX< (TN-FONLU TN) (TN-FONFC PTN)))
                  ;; Gross hack to fix FIB.  I'm not convinced of its
                  ;;  correctness.  -JAR 2 Feb 82
                  (FX> (NODE-STACKNUM (TN-OWNER TN)) (TN-ISLOC PTN)))
             (COND ((NULL? (TN-ISLOC PTN))
                    (BUGLET ((*TN* TN))
                            "a stack TN doesn't know where it wants to be"
                            "well, I'm certainly not going to attempt to preference to it!"))
                   (T (SETF (TN-WANTLOC TN) 'STACK)
                      (SETF (TN-ISLOC TN) (TN-ISLOC PTN))
                      (SETF (TN-REASON TN) PTN)
                      (RETURN T))))))
    ---))

;;; Could be more efficient on multiple-word TN's if PACK-ATTEMPT
;;; gave TRY-TO-PACK some information about how it lost.

(DEFINE (TRY-TO-PACK TN AREA NAME)
  (D0 ((A AREA (CDR A)))
      ((NULL? A) NIL)
    ----
    (IF (PACK-ATTEMPT TN A NAME) (RETURN T))
    ---))

;;; Attempt to pack a TN into a specific place in an area.

(DEFINE (PACK-ATTEMPT TN AREA NAME)
  (COND ((ALIKEV? (TN-ISLOC TN) (CAAR AREA))    ; EQUIV?
         (PACK TN AREA NAME))           ; Do it!  Who cares!
        (ELSE
         (D0 ((A AREA (CDR A))
              (J (TN-SIZE TN) (FX- J 1)))
             ((ZERO? J)
              (PACK TN AREA NAME))
           ----
           ;;? This DO body has changed
           (COND ((OR (NULL? A)
;                     (NOT (EQ? (CADAR A) 'AVAILABLE))
                      )
                  (RETURN NIL))
                 (T (LET ((CONFLICTP (TN-HAS-BUCKET-CONFLICT? TN (CAR A))))
                      (COND (CONFLICTP
                             (AND *RECORD-TN-CONFLICTS?*
                                  (push (TN-CONFLICTS TN) CONFLICTP))
                             (RETURN NIL))))))
           ---))))

(DEFINE (PACK TN AREA NAME)
  (IF (NULL? (TN-REASON TN))
      (SETF (TN-REASON TN) (TN-WANTLOC TN)))
  (SETF (TN-WANTLOC TN) NAME)
  (SETF (TN-ISLOC TN) (CAAR AREA))
  (DO ((A AREA (CDR A))
       (J (TN-SIZE TN) (FX- J 1)))
      ((ZERO? J))
    ----
    ;; Install TN into bucket(s)                                
    (PUSH (CDDAR A) TN)
    ---)
  T)


;;; If TN can fit into BUCKET, returns NIL for no conflict.
;;; Otherwise the TN in the bucket which conflicts is returned.

(DEFINE (TN-HAS-BUCKET-CONFLICT? TN BUCKET)
  (D0 ((B (CDDR BUCKET) (CDR B)))
      ((NULL? B) NIL)
    ----
    (OR (TNS-MAY-COHABIT? TN (CAR B))
        (RETURN (CAR B)))
    ---))

(DEFINE (TNS-MAY-COHABIT? TN1 TN2)
  (OR ;(MEMQ TN1 (TN-NO-CONFLICT-LIST TN2))  weird idea
      ;(MEMQ TN2 (TN-NO-CONFLICT-LIST TN1))
      (FX< (TN-LONLU TN1) (TN-LONFC TN2))
      (FX< (TN-FONLU TN1) (TN-FONFC TN2))
      (FX> (TN-LONFC TN1) (TN-LONLU TN2))
      (FX> (TN-FONFC TN1) (TN-FONLU TN2))))

