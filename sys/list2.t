(HERALD (TSYS LIST2 T 50)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; List utilities, part 2

(DEFINE (BAD-LIST-INDEX . REST)
  (ERROR "illegal index into list~%  ~S" REST))

(DEFINE (NTHCDR LIST INDEX)
  (LET ((INDEX (CHECK-ARG NONNEGATIVE-FIXNUM? INDEX NTHCDR)))
    (ITERATE LOOP ((L LIST) (I INDEX))
      (COND ((FX= I 0) L)
            ((NULL-LIST? L)
             (BAD-LIST-INDEX 'NTHCDR LIST INDEX))
            (ELSE (LOOP (CDR L) (FX- I 1)))))))

;;; SET-NTHCDR

(DEFINE NTH
  (OBJECT (LAMBDA (LIST INDEX)
            (LET ((INDEX (CHECK-ARG NONNEGATIVE-FIXNUM? INDEX NTH)))
              (ITERATE LOOP ((L LIST) (I INDEX))
                (COND ((NULL-LIST? L)
                       (BAD-LIST-INDEX 'NTH LIST INDEX))
                      ((FX= I 0) (CAR L))
                      (ELSE (LOOP (CDR L) (FX- I 1)))))))
          ((SETTER SELF) SET-NTH)))

(DEFINE (SET-NTH LIST INDEX VALUE)
  (LET ((INDEX (CHECK-ARG NONNEGATIVE-FIXNUM? INDEX SET-NTH)))
    (ITERATE LOOP ((L LIST) (I INDEX))
      (COND ((NULL-LIST? L)
             (BAD-LIST-INDEX 'SET `(NTH ,LIST ,INDEX) VALUE))   ; whattahack
            ((FX= I 0) (SET (CAR L) VALUE))
            (ELSE (LOOP (CDR L) (FX- I 1)))))))

(DEFINE LAST
  (OBJECT (LAMBDA (LIST)
            (CAR (LASTCDR LIST)))
          ((SETTER SELF) SET-LAST)))

(DEFINE (SET-LAST LIST VALUE)
  (SET (CAR (LASTCDR LIST)) VALUE))

(DEFINE (LASTCDR LIST)
  (ITERATE LOOP ((LIST (CHECK-ARG PAIR? LIST LASTCDR)))
    (IF (NOT (PAIR? (CDR LIST))) LIST
      (LOOP (CDR LIST)))))

(DEFINE (CIRCULAR? MOVE X)
  (IF (NULL-LIST? X) NIL
    (ITERATE RACE ((SLOW-RUNNER X) (FAST-RUNNER (MOVE X)))
      (COND ((OR (NULL-LIST? FAST-RUNNER) (NULL-LIST? (MOVE FAST-RUNNER))) NIL)
            ((EQ? SLOW-RUNNER FAST-RUNNER) T) ;Fast runner caught up!
            (ELSE
             (RACE (MOVE SLOW-RUNNER) (MOVE (MOVE FAST-RUNNER))))))))

(DEFINE (PROPER-LIST? X)
  (IF (ATOM? X) (NULL? X)
    (PROPER-LIST? (CDR X))))

(DEFINE (SUBLIST L START COUNT)
  (ITERATE LOOP ((I COUNT)
                 (LL (NTHCDR L START))
                 (RESULT '()))
    (COND ((FX<= I 0) (REVERSE! RESULT))
          ((NULL-LIST? LL)
           (ERROR "argument list is too short~%  (~S ~S ~S ~S)"
                  'SUBLIST L START COUNT))
          (ELSE
           (LOOP (FX- I 1)
                 (CDR LL)
                 (CONS (CAR LL) RESULT))))))



;;; ASS = Association-list lookup

(DEFINE (ASS PRED OBJ LIST)
  (ITERATE LOOP ((LIST LIST))
    (COND ((NULL-LIST? LIST) NIL)
          ((PRED OBJ (CAAR LIST)) (CAR LIST))
          (ELSE (LOOP (CDR LIST))))))

(DEFINE-INTEGRABLE (ASS? PRED OBJ LIST)
  (IF (ASS PRED OBJ LIST) T NIL))

;(DEFINE-INTEGRABLE (ASSQ  OBJ LIST) (ASS  EQ? OBJ LIST))
;(DEFINE-INTEGRABLE (ASSQ? OBJ LIST) (ASS? EQ? OBJ LIST))

(DEFINE (ASSQ OBJ LIST)
  (ITERATE LOOP ((L LIST))
    (IF (NULL-LIST? L) NIL
      (LET ((Z (CAR L)))
        (COND ((NOT (PAIR? Z))
               (LOOP (CONS (ERROR '("association list contains non-pair~%"
                                    "  (ASSQ ~S ~S)")
                                  OBJ
                                  LIST)
                           (CDR L))))
              ((EQ? OBJ (CAR Z)) Z)
              (ELSE (LOOP (CDR L))))))))

(DEFINE-INTEGRABLE (ASSQ? OBJ LIST) (TRUE? (ASSQ OBJ LIST)))

;;; DEL = Deletion from list

(DEFINE (DEL PRED OBJ LIST)
  (COND ((NULL-LIST? LIST) '())
        ((PRED OBJ (CAR LIST))
         (DEL PRED OBJ (CDR LIST)))
        ((MEM? PRED OBJ (CDR LIST))
         (CONS (CAR LIST) (DEL PRED OBJ (CDR LIST))))
        (ELSE LIST)))

(DEFINE-INTEGRABLE (DELQ OBJ LIST) (DEL EQ? OBJ LIST))

(DEFINE (DEL! PRED OBJ LIST)
  (COND ((NULL-LIST? LIST) '())
        ((PRED OBJ (CAR LIST)) (DEL! PRED OBJ (CDR LIST)))
        (ELSE (SET (CDR LIST) (DEL! PRED OBJ (CDR LIST)))
              LIST)))

(DEFINE-INTEGRABLE (DELQ! OBJ LIST) (DEL! EQ? OBJ LIST))


;;;; Questionable cruft.  T compiler wants it.  PRINT-CHAR wants RASS.
;;; What to do about sequence functions?  Be common-lisp compatible?

(DEFINE (POS PRED OBJ L)
  (ITERATE LOOP ((L L)
                 (N 0))
    (COND ((NULL-LIST? L) NIL)
          ((PRED OBJ (CAR L)) N)
          (ELSE (LOOP (CDR L) (FX+ N 1))))))

(DEFINE-INTEGRABLE (POSQ OBJ L) (POS EQ? OBJ L))

(DEFINE (MEMASS PRED X L)
  (ITERATE LOOP ((Z L))
    (COND ((NULL-LIST? Z) NIL)
          ((PRED X (CAAR Z)) Z)
          (ELSE (LOOP (CDR Z))))))

(DEFINE-INTEGRABLE (MEMASSQ OBJ L) (MEMASS EQ? OBJ L))

(DEFINE (RASS PRED ITEM ALIST)
  (ITERATE LOOP ((A ALIST))
    (COND ((NULL-LIST? A) NIL)
          ((PRED ITEM (CDAR A)) (CAR A))
          (ELSE (LOOP (CDR A))))))

(DEFINE-INTEGRABLE (RASSQ OBJ L) (RASS EQ? OBJ L))

;;;; Reverse, Append, ...
;;;           Maclisp                           <T>

;;;     Safe            Unsafe          Safe            Unsafe
;;;
;;;     REVERSE         NREVERSE        REVERSE         REVERSE!
;;;     APPEND          NCONC           APPEND          APPEND!
;;;     ---             NRECONC         APPEND-REVERSE  APPEND-REVERSE!

(DEFINE (APPEND . LISTS)
  (LABELS (((APPEND2 L1 L2)
            ;(IF (NULL? (CHECK-ARG LIST? L1 APPEND)) L2 ...)
            (IF (NULL-LIST? L1) L2
              (CONS (CAR L1) (APPEND2 (CDR L1) L2)))))
    (COND ((NULL? LISTS) '())
          ((NULL? (CDR LISTS)) (CAR LISTS))
          ((NULL? (CDDR LISTS)) (APPEND2 (CAR LISTS) (CADR LISTS)))
          (ELSE (APPEND (CAR LISTS) (APPLY APPEND (CDR LISTS)))))))

;;; Old definition of APPEND.  This was used in the days before EZCLOSE worked.
;(DEFINE (APPEND2 L1 L2)
;  (IF (NULL? L1) L2
;    (CONS (CAR L1) (APPEND2 (CDR L1) L2))))
;
;(DEFINE (APPEND . LISTS)
;  (COND ((NULL? LISTS) '())
;       ((NULL? (CDR LISTS)) (CAR LISTS))
;       ((NULL? (CDDR LISTS)) (APPEND2 (CAR LISTS) (CADR LISTS)))
;       (ELSE (APPEND2 (CAR LISTS) (APPLY APPEND (CDR LISTS))))))

(DEFINE (COPY-LIST LIST) (APPEND LIST '()))

(DEFINE (REVERSE LIST)
  (APPEND-REVERSE LIST '()))

(DEFINE (APPEND-REVERSE LIST SEED)
  (IF (NULL-LIST? LIST) SEED
    (APPEND-REVERSE (CDR LIST) (CONS (CAR LIST) SEED))))

;;; Destructive forms

(DEFINE (APPEND! . LISTS) ; Note: (APPEND! 'atom 'anything) => anything
  (COND ((NULL? LISTS)  '())
        ((NULL? (CDR LISTS)) (CAR LISTS))
        ((NULL? (CDDR LISTS))
         (COND ((NULL-LIST? (CAR LISTS))
                (CADR LISTS))
               (ELSE
                (SET (CDR (LASTCDR (CAR LISTS)))
                     (CADR LISTS))
                (CAR LISTS))))
        (ELSE (APPEND! (CAR LISTS)
                       (APPLY APPEND! (CDR LISTS))))))

;;; REVERSE! and APPEND-REVERSE! live LIST1 because they're needed early on.

;;; Make this open-coded someday.
(DEFINE (DISPLACE X Y)
  (LET ((X (CHECK-ARG PAIR? X DISPLACE))
        (Y (CHECK-ARG PAIR? Y DISPLACE)))
    (SET (CAR X) (CAR Y))
    (SET (CDR X) (CDR Y))
    X))


;;; Test cases:
;;;
;;;     (ANY      PAIR? '())    => ()
;;;     (EVERY    PAIR? '())    => T
;;;     (ANYCDR   PAIR? '())    => ()
;;;     (EVERYCDR PAIR? '())    => ()
;;;
;;;     (ANY      PAIR? '(A))   => ()
;;;     (EVERY    PAIR? '(A))   => ()
;;;     (ANYCDR   PAIR? '(A))   => T
;;;     (EVERYCDR PAIR? '(A))   => ()
;;;
;;;     (ANY      PAIR? '((A))) => T
;;;     (EVERY    PAIR? '((A))) => T
;;;     (ANYCDR   PAIR? '((A))) => T
;;;     (EVERYCDR PAIR? '((A))) => ()
;;;
;;;     (ANY      NULL? '(() A)) => T
;;;     (EVERY    NULL? '(() A)) => ()
;;;     (ANYCDR   NULL? '(() A)) => T
;;;     (EVERYCDR NULL? '(() A)) => ()
;;;
;;;     (ANY      NULL? '())     => ()
;;;     (EVERY    NULL? '())     => T
;;;     (ANYCDR   NULL? '())     => T
;;;     (EVERYCDR NULL? '())     => T

;;; What about making ANY, WALK, etc. take any number of arguments?
;;; Care must be taken here not to involve MAP in ANY's definition, since
;;; MAP calls ANY.

(DEFINE-INTEGRABLE (ANY?      PRED L) (IF (ANY      PRED L) T NIL))
(DEFINE-INTEGRABLE (ANYCDR?   PRED L) (IF (ANYCDR   PRED L) T NIL))

(DEFINE-INTEGRABLE (EVERY?    PRED L) (IF (EVERY    PRED L) T NIL))
(DEFINE-INTEGRABLE (EVERYCDR? PRED L) (IF (EVERYCDR PRED L) T NIL))

(DEFINE (ANY PRED L)
  (COND ((NULL-LIST? L) NIL)
        ((PRED (CAR L)))
        (ELSE (ANY PRED (CDR L)))))

(DEFINE (ANYCDR PRED L)
  (COND ((ATOM? L) (PRED L))            ; Huh?
        ((PRED L))
        (ELSE (ANYCDR PRED (CDR L)))))

(DEFINE (EVERY PRED L)
  (COND ((NULL-LIST? L) T)
        ((PRED (CAR L)) => (LAMBDA (X)
                             (IF (NULL? (CDR L)) X
                               (EVERY PRED (CDR L)))))
        (ELSE NIL)))

(DEFINE (EVERYCDR PRED L)
  (COND ((ATOM? L) (PRED L))            ; Huh?
        ((NOT (PRED L)) NIL)
        (ELSE (EVERYCDR PRED (CDR L)))))

(DEFINE (*AND . X) (EVERY IDENTITY X))
(DEFINE (*OR  . X) (ANY   IDENTITY X))

(DEFINE-INTEGRABLE (*AND? . X) (EVERY? IDENTITY X))
(DEFINE-INTEGRABLE (*OR?  . X) (ANY?   IDENTITY X))

;(DEFINE-SYNTAX (AND? . X) `(IF (AND ,@X) T NIL))
;(DEFINE-SYNTAX (OR?  . X) `(IF (OR  ,@X) T NIL))

(DEFINE-INTEGRABLE (*IF PRED CON ALT) (IF PRED CON ALT))
