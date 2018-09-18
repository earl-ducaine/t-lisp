(HERALD (TSYS LIST1 T 39)
        (PRE-COOK)
        (ENV () (TSYS OPEN) (TSYS ALIASES) (TSYS CARCDR)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; List utilities, part 1

;;; The factoring between this and LIST2 seems rather arbitrary,
;;; except that it is LIST2's contract not to have an DEFINE-INTEGRABLEs.

;;;; Miscellaneous

(DEFINE (LIST . L) L)

;(DEFINE (CONS* FIRST . REST)           ; Elegant N^2-cons definition
;  (IF (NULL? REST) FIRST
;    (CONS FIRST (APPLY CONS* REST))))

(DEFINE (CONS* FIRST . REST)            ; Hackola definition
  (COND ((NULL? REST) FIRST)
        (ELSE (DO ((R REST (CDR R))
		   (Q REST R))
		  ((NULL? (CDR R))
		   (SET (CDR Q) (CAR R))
		   (SET (CAR R) FIRST)
		   (IF (NEQ? R REST) (SET (CDR R) REST))
		   R)))))

;;; Called from NULL-LIST?, which is integrable
;;; Due to gross TC bug, this must precede any calls to NULL-LIST?.

(DEFINE (LOSING-NON-NULL-LIST X)
  (ERROR "expected a list, but got an atom instead~%  (... . ~S)"
         X)
  T)
(DEFINE (LENGTH L)
  (DO ((I 0. (FX+ I 1))
       (L L (CDR L)))
      ((NULL-LIST? L) I)))

;;; This really belongs elsewhere.
;;; Incredibly hacked-up version to bum the shit out.

(DEFINE (MAP1 PROC L)
  (COND ((NULL-LIST? L) '())
        (ELSE
         (LET ((RESULT (NEW-CELL)))
           (SET (CAR RESULT) (PROC (CAR L)))
           (ITERATE LOOP ((L (CDR L)) (R RESULT))
             (COND ((NULL-LIST? L)
                    (SET (CDR R) '())
                    RESULT)
                   (ELSE
                    (LET ((Q (NEW-CELL)))
                      (SET (CDR R) Q)
                      (SET (CAR Q) (PROC (CAR L)))
                      (LOOP (CDR L) Q)))))))))

(DEFINE (WALK1 PROC L)
  (ITERATE LOOP ((L L))
    (COND ((NULL-LIST? L) '*VALUE-OF-WALK*)
          (ELSE
           (PROC (CAR L))
           (LOOP (CDR L))))))

;;; MEM = Membership in list

(DEFINE (MEM PRED OBJ LIST)
  (COND ((NULL-LIST? LIST) NIL)
        ((PRED OBJ (CAR LIST)) LIST)
        (ELSE (MEM PRED OBJ (CDR LIST)))))

(DEFINE (APPEND-REVERSE! OLD-LIST SEED)
  (COND ((NULL-LIST? OLD-LIST) SEED)
        (ELSE
         (ITERATE LOOP ((OLD-CDR (CDR OLD-LIST))
                        (OLD-CAR SEED)
                        (TAIL    OLD-LIST))
           (COND ((NULL-LIST? (CDR TAIL))
                  (SET (CDR TAIL) OLD-CAR)
                  TAIL)
                 (ELSE
                  (SET (CDR TAIL) OLD-CAR)
                  (LOOP (CDR OLD-CDR) TAIL OLD-CDR)))))))

;;; Due to a gross TC bug, this definition must follow the previous one.

(DEFINE (REVERSE! OLD-LIST)
  (APPEND-REVERSE! OLD-LIST '()))
