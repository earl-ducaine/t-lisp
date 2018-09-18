(HERALD (TSYS POPULATE T 36)
        (ENV TSYS (TSYS EARLY)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Populations

;;; Thanks to Carl Hewitt of M.I.T.
;;; A population is like a list except that its members go away if they
;;;  aren't copied out by the GC.  That is, at any point when the only way
;;;  to get to an object is via a population, then the system is free to
;;;  delete the object from the population, thus making it completely
;;;  inaccessible.
;;; The only valid operations on populations are:
;;;  ADD-TO-POPULATION - add an object to a population.
;;;  REMOVE-FROM-POPULATION - remove an object from a population.
;;;  POPULATION->LIST  - make a list of the members of a population.  (This
;;;  has the effect of making the population's members accessible via the
;;;  returned list.)

(DEFINE-CONSTANT %%POPULATION-SIZE 2)
(DEFINE-INTEGRABLE (POPULATION? OBJ)
  (AND (EXTEND? OBJ) (EQ? (EXTEND-TEMPLATE OBJ) *POPULATION-TEMPLATE*)))
(DEFINE-INTEGRABLE (POPULATION-IDENTIFICATION P) (XREF P 0))        ; bletch
(DEFINE-INTEGRABLE (POPULATION-POPULACE P)       (XREF P 1))
(DEFINE-INTEGRABLE (SET-POPULATION-IDENTIFICATION P VAL) (XSET P 0 VAL))
(DEFINE-INTEGRABLE (SET-POPULATION-POPULACE P VAL)       (XSET P 1 VAL))

(DEFINE (MAKE-POPULATION ID)
  (LET ((P (MAKE-POPULATION-1 ID)))
    (ADD-TO-POPULATION *THE-POPULATIONS* P)
    P))

(DEFINE (MAKE-POPULATION-1 ID)          ; not exported!
  (LET ((P (MAKE-EXTEND-N *POPULATION-TEMPLATE* %%POPULATION-SIZE)))
    (SET-POPULATION-IDENTIFICATION P ID)
    (SET-POPULATION-POPULACE P '())
    P))

(DEFINE *THE-POPULATIONS* (MAKE-POPULATION-1 '*THE-POPULATIONS*))

;;; Make the object be a new member of the population.

(DEFINE (ADD-TO-POPULATION P OBJ)
  (LET ((P (CHECK-ARG POPULATION? P ADD-TO-POPULATION)))
    (GC-DEFER (SET-POPULATION-POPULACE P
                                       (CONS OBJ (POPULATION-POPULACE P))))
    T))

(DEFINE (REMOVE-FROM-POPULATION P OBJ)
  (LET ((P (CHECK-ARG POPULATION? P REMOVE-FROM-POPULATION)))
    (GC-DEFER (SET-POPULATION-POPULACE P
                                       (DELQ! OBJ (POPULATION-POPULACE P))))
    T))

;;; This is a little strange.  We really should be doing ENSURE-HEAP-SIZE on
;;; the length of the populace list to make sure the COPY-LIST doesn't trigger
;;; a GC.

(DEFINE (POPULATION->LIST P)
  (LET ((P (CHECK-ARG POPULATION? P POPULATION->LIST)))
    (GC-DEFER (COPY-LIST (POPULATION-POPULACE P)))))

;;; Auxiliary.  This is provided because it might for whatever reason be more
;;; efficient than (WALK PROC (POPULATION->LIST P)); e.g. there might be
;;; fewer pointers around if PROC does a GC.

(DEFINE (POPULATION-WALK P PROC)
  (LET ((Z (POPULATION->LIST P)))
    (DO ((L Z (CDR L)))
	((NULL? L) (RETURN-LIST-TO-FREELIST Z))
      (PROC (CAR L)))))

(DEFINE (WALK-POPULATION P PROC)
  (POPULATION-WALK PROC P))

;;; Internal entry from POOL code for pre-GC-hook.  We really don't want to be
;;; consing immediately before GC'ing, so the above is inappropriate.

(DEFINE (WALK-POPULATION-UNSAFELY PROC P)
  (DO ((L (POPULATION-POPULACE P) (CDR L)))
      ((NULL? L) T)
    (PROC (CAR L))))

;;; All still-active populations are already in newspace at the point this is
;;; called.

(DEFINE (GC-UPDATE-THE-POPULATIONS)
  (GC-UPDATE-POPULATION *THE-POPULATIONS*)
  (WALK-POPULATION-UNSAFELY GC-UPDATE-POPULATION *THE-POPULATIONS*))

;;; Tack agendum on end

(SET *POST-GC-AGENDA*
     (CONS (CONS 'GC-UPDATE-THE-POPULATIONS
		 GC-UPDATE-THE-POPULATIONS)
	   '()))

;;; For extra security we should really check to make sure that the cons-cells
;;; comprising the list L haven't been gc-forwarded.

(DEFINE (GC-UPDATE-POPULATION P)
  (DO ((L (POPULATION-POPULACE P) (CDR L))
       (Z '() (COND ((ALIVE-AFTER-GC? (CAR L))  ; Preserve obj if it was copied
                     (LET ((Z (CONS 0 Z)))
                       (GC-COPY (%LOC CAR Z) (CAR L))   ; Fetch fowarded ptr
                       (SET *GC-POPULATION-MEMBERS*
                            (FX+ *GC-POPULATION-MEMBERS* 1))
                       Z))
                    (ELSE
                     (SET *GC-POPULATION-DEATHS*
                          (FX+ *GC-POPULATION-DEATHS* 1))
                     Z))))
      ((NULL? L)
       (SET-POPULATION-POPULACE P Z))))

;;; There ought to be a CRAWL-EXHIBIT method also.

(DEFINE HANDLE-POPULATION
  (%HANDLER ((IDENTIFICATION P) (POPULATION-IDENTIFICATION P))
            ((PRINT-TYPE-STRING P) (IGNORE P) "Population")))
