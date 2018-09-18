(HERALD (TSYS GC T 236)
        (PRE-COOK)
        (ENV TSYS (TSYS CHUNK) (TSYS STRUCT)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; "Garbage collector"

;;; Routines with names like GC-TRACE-FOO take an object either in newspace or
;;; in some static space and (recursively) revise any pointers into newspace
;;; they may contain.
;;; Routines with names like GC-COPY-FOO are the ones that cons new memory and
;;; then do a GC-TRACE-FOO.  These usually take two arguments: the object to be
;;; copied, and a (machine) location (created using %LOC or %XLOC) into which
;;; to store the pointer to the copy.
;;; This was inspired by Phil Wadler's "tail-recursion modulo cons" idea.
;;; (Given the GC-TRACE concept, isn't it redundant to pass both LOC and OBJ?
;;; I.e. isn't (EQ? OBJ (%LOC-CONTENTS LOC)) always true?)

(DEFINE-INTEGRABLE (OLDSPACE-CONTAINS? OBJ)
  (AND (POINTER-GREATER? OBJ *OLDSPACE-LOW*)
       (POINTER-LESS?    OBJ *OLDSPACE-HIGH*)))

(DEFINE-INTEGRABLE (NEWSPACE-CONTAINS? OBJ)
  (AND (POINTER-GREATER? OBJ *HEAP-BEGIN*)
       (POINTER-LESS?    OBJ (HEAP-POINTER))))

(LSET *REALLY-DOING-GC?* NIL)

(DEFINE *GC-DEBUG?* NIL)

(DEFINE (GC-ROOT)
  (CATCH HACK
    (SET *REALLY-DOING-GC?* T)
    (GC-TRACE-THE-INITIAL-UNITS)
    (GC-TRACE-THE-INITIAL-SYMBOLS)
    ;; Copy out the stack.
    (GC-TRACE-STACK (ESCAPE-PROCEDURE-FRAME HACK))
    (SET *REALLY-DOING-GC?* NIL)))

;;; Trace pointers in the initial units.
;;; Our current world view holds that if a unit is precooked, then none of
;;; its pointers has a chance of escaping to newspace.  This is a simplifying
;;; assumption which will someday have to be revised.

(DEFINE (GC-TRACE-THE-INITIAL-UNITS)
  (DO ((LEN (VECTOR-LENGTH *THE-INITIAL-UNITS*))
       (I 0 (FX+ I 1)))
      ((FX>= I LEN))
    (LET ((U (VREF *THE-INITIAL-UNITS* I)))
      (SELECT (CODE-MAGIC-NUMBER (UNIT-CODE U))
        ((%%CORRECT-FASL %%INCORRECT-FASL)
         (IF *GC-DEBUG?* (GC-FORMAT GC-OUTPUT "Initial unit: ~S~%" U))
         (GC-TRACE-UNIT-1 U (CODE-STRING-AREA-INDEX (UNIT-CODE U))))
        ((%%PRECOOKED)
         ;; Kludge!
         (GC-COPY (%LOC UNIT-SOURCE-FILENAME U) (UNIT-SOURCE-FILENAME U))
         (GC-COPY (%LOC UNIT-HERALD          U) (UNIT-HERALD U)))
        ))))

;;; Trace pointers in initial symbols and vcells.
;;; This depends heavily on exactly what SYSGEN does.

(DEFINE (GC-TRACE-THE-INITIAL-SYMBOLS)
  (IF *GC-DEBUG?* (GC-FORMAT GC-OUTPUT ";Tracing initial symbols.~%"))
  (LET ((LEN (VECTOR-LENGTH *THE-INITIAL-SYMBOLS*)))
    (DO ((I 0 (FX+ I 1)))
        ((FX>= I LEN) 'T)
      (LET ((SYM (VREF *THE-INITIAL-SYMBOLS* I)))
        (LET ((VC (SYMBOL-VCELL SYM)))
          (IF (AND VC (NOT (OLDSPACE-CONTAINS? VC)))
              (GC-TRACE-EXTEND VC %%VCELL-SIZE)))
        (GC-TRACE-EXTEND SYM %%SYMBOL-SIZE)))))

;;; Simplifying assumption for now:  there is only one stack, and it is
;;; THE stack, and it is not "active".  This is no good at all really, but will
;;; suffice for now.

(DEFINE (GC-TRACE-STACK FRAME)
  (GC-COPY (%LOC EXTEND-TEMPLATE FRAME) (EXTEND-TEMPLATE FRAME))
  (LET ((TEM (EXTEND-TEMPLATE FRAME)))
    (IF *GC-DEBUG?*
        (GC-FORMAT GC-OUTPUT "Tracing frame ~S (description = ~S)~%"
                   FRAME
                   (GET-PROC-NAME TEM)))
    (LET ((CHUNK (TEMPLATE-CHUNK TEM))
          (SUPER (TEMPLATE-SUPERIOR-TEMPLATE TEM)))
      (COND (SUPER
             ;; This is utterly bogus.  We should really be looking for a
             ;; nonstandard-gc-method for the frame.  Sigh.
             (LET ((POINTER-COUNT (FX+ (LET ((Z (CHUNK-POINTER-COUNT CHUNK)))
                                         (IF (FX= Z 255.) -1 Z))
                                       1)))
               (COND ((FIXNUM-ODD? POINTER-COUNT)
                      (ZERROR
 "bogus stack frame - (GC-TRACE-STACK ~S)~%  Pointer-count = ~S~%  Try (RET) to proceed"
                              FRAME
                              POINTER-COUNT)))
               (DO ((I 0 (FX+ I 1)))
                   ((FX>= I POINTER-COUNT)
                    (LET ((PRED (%XLOC FRAME (FX+ (FRAME-SIZE FRAME) 1)))
                          (LIMIT (FX- -2 (CHUNK-POINTER-MEM-SIZE
                                             (TEMPLATE-CHUNK SUPER)))))
                      (DO ((I -2 (FX- I 1)))
                          ((FX<= I LIMIT)
                           (GC-TRACE-STACK PRED))
                        (GC-COPY (%XLOC PRED I) (XREF PRED I)))))
                 (GC-COPY (%XLOC FRAME I) (XREF FRAME I)))))
            ((EQ? TEM *STACK-BASE-TEMPLATE*)
             'T)
            ((EQ? TEM *FAULT-FRAME-TEMPLATE*)
             (SET *GC-FAULT-FRAME-LOSSAGE?* FRAME)
             (GC-TRACE-STACK (FAULT-FRAME-PREVIOUS FRAME)))
	    ((EQ? TEM *DISPATCH-RET-TEMPLATE*)
	     ;; Format of frame is:
	     ;;  xref -1:  dispatch return point
	     ;;        0:  zero, if alignment was necessary
	     ;;   0 or 1:  pointer to previous frame
	     ;;      ...:  other arguments
	     ;;      N-3:  self
	     ;;      N-2:  next
	     ;;      N-1:  operation
	     ;;        N:  object
	     ;;      N+1:  template for previous frame
             (LET ((LIMIT (VFRAME-SIZE FRAME)))
               (DO ((I 0 (FX+ I 1)))
                   ((FX>= I LIMIT)
                    (GC-TRACE-STACK (VFRAME-PREVIOUS FRAME)))
                 (GC-COPY (%XLOC FRAME I) (XREF FRAME I)))))
            (ELSE
             (SET *GC-FRAME-LOSSAGE?* FRAME))
            ))))

(LSET *GC-NOISILY?* T)
(LSET *GC-NOTE-FREQUENCY* 10000)

(DEFINE-INTEGRABLE-ONLY (NOTE-GC-COPY FROM TO)
  (COND (*GC-NOISILY?*
         (COND ((FX>= (SET *GC-COPY-TICK* (FX+ *GC-COPY-TICK* 1))
                      *GC-NOTE-FREQUENCY*)
                (NOTE-GC-COPY-1 FROM TO))))))

(DEFINE (NOTE-GC-COPY-1 FROM TO)
  (SET *GC-COPY-COUNT* (FX+ *GC-COPY-COUNT* *GC-COPY-TICK*))
  (SET *GC-COPY-TICK* 0)
  (GC-FORMAT GC-OUTPUT ";(~S) Copying from ~X to ~X.~%"
             *GC-COPY-COUNT*
             (POINTER->FIXNUM FROM)
             (POINTER->FIXNUM TO)))

(DEFINE-INTEGRABLE-ONLY (NOTE-GC-REPEAT OBJ)
  (COND (*GC-NOISILY?*
         (SET *GC-REPEAT-COUNT* (FX+ *GC-REPEAT-COUNT* 1)))))

(LSET *GC-LOSER-BREAK?* NIL)

(DEFINE (GC-LOSER LOC OBJ MESSAGE)
  (SET *GC-LOSER-COUNT* (FX+ *GC-LOSER-COUNT* 1))
  (SET *GC-LOSER* (POINTER->FIXNUM OBJ))
  (SET *GC-LOSER-LOC* (POINTER->FIXNUM LOC))
  (COND (*GC-LOSER-BREAK?*
         (ZERROR "~A found in GC (source=~X dest=~X)~
             ~%  Please report this to the system implementors.~
             ~%  Type the end-of-file character to proceed."
                 MESSAGE
                 *GC-LOSER*
                 *GC-LOSER-LOC*))
        (ELSE
         (GC-FORMAT GC-OUTPUT ";Corrupt datum (~A): ~X -> ~X~%"
                    MESSAGE
                    *GC-LOSER*
                    *GC-LOSER-LOC*)))
  (%SET-LOC-CONTENTS LOC MESSAGE))

;;; Stuff for copying strings is in VAXGC.T / M68GC.T

;;; Stuff for handling flonums.  The foward marker
;;; and GC-COPY-FLONUM is in VAXGC.T / M68GC.T

(DEFINE-INTEGRABLE-ONLY (FLONUM-FORWARDED? OBJ)
  (EQ? (FLONUM-LOW-PESO OBJ) (FIXNUM->POINTER *FLONUM-FORWARD-MARKER*)))

(DEFINE-INTEGRABLE FLONUM-FORWARDED FLONUM-HIGH-PESO)

(DEFINE-INTEGRABLE-ONLY (SET-FLONUM-FORWARDED OBJ NEW-OBJ)
  (SET (FLONUM-HIGH-PESO OBJ) NEW-OBJ)
  (SET (FLONUM-LOW-PESO  OBJ) (FIXNUM->POINTER *FLONUM-FORWARD-MARKER*)))

;;; pairs ...

(DEFINE-INTEGRABLE-ONLY (PAIR-FORWARDED-CAR? A)
  (AND (PAIR? A) (NEWSPACE-CONTAINS? A)))

(DEFINE-INTEGRABLE-ONLY (PAIR-FORWARDED? OBJ)
  (PAIR-FORWARDED-CAR? (CAR OBJ)))

(DEFINE-INTEGRABLE-ONLY (SET-PAIR-FORWARDED OBJ NEW-OBJ)
  (SET (CAR OBJ) NEW-OBJ))

(DEFINE-INTEGRABLE-ONLY (GC-COPY-PAIR LOC OBJ)
  (LET ((A (CAR OBJ)))                  ; Save for later
    (COND ((PAIR-FORWARDED-CAR? A)
           (%SET-LOC-CONTENTS LOC A)
           (NOTE-GC-REPEAT OBJ))
          (ELSE
           (SET *GC-PAIR-COUNT* (FX+ *GC-PAIR-COUNT* 1))
           (LET ((NEW-OBJ (NEW-CELL)))
             (NOTE-GC-COPY OBJ NEW-OBJ)
             (%SET-LOC-CONTENTS LOC NEW-OBJ)
             (SET-PAIR-FORWARDED OBJ NEW-OBJ)
             (GC-COPY (%LOC CDR NEW-OBJ) (CDR OBJ))     ; linearize
             (GC-COPY (%LOC CAR NEW-OBJ) A))))))

;;; templates ...

(DEFINE-INTEGRABLE-ONLY (TEMPLATE-FORWARDED? TEM)
  (FX= (TEMPLATE-LOW-BYTE TEM) 254.))

(DEFINE-INTEGRABLE TEMPLATE-FORWARDED TEMPLATE-CHUNK)

(DEFINE-INTEGRABLE-ONLY (SET-TEMPLATE-FORWARDED TEM NEW-TEM)
  (SET-TEMPLATE-LOW-BYTE TEM 254.)
  (SET-TEMPLATE-CHUNK    TEM NEW-TEM))

(DEFINE (GC-COPY-TEMPLATE LOC OBJ)
  (COND ((TEMPLATE-FORWARDED? OBJ)
         (%SET-LOC-CONTENTS LOC (TEMPLATE-FORWARDED OBJ)))
        ((FX= (TEMPLATE-JUMP-OPCODE OBJ) %%JUMP-OPCODE)
         (LET ((UNIT (TEMPLATE-UNIT OBJ)))
           (COND ((NOT (UNIT? UNIT))
                  (GC-LOSER LOC OBJ "template with bad unit"))
                 (ELSE
                  (LET ((NEW-UNIT (GC-MOVE-UNIT UNIT)))
                    (%SET-LOC-CONTENTS LOC (TEMPLATE-FORWARDED OBJ))
                    (GC-TRACE-UNIT NEW-UNIT))))))
        (ELSE
         (GC-LOSER LOC OBJ "bad template"))))

;;; extends ...

;;; This is not integrable-only, because POPULATE needs it.
(DEFINE-INTEGRABLE (EXTEND-FORWARDED? OBJ)
  (NEWSPACE-CONTAINS? (EXTEND-TEMPLATE OBJ)))

(DEFINE-INTEGRABLE (SET-EXTEND-FORWARDED OBJ NEW-OBJ)
  (SET (EXTEND-TEMPLATE OBJ) NEW-OBJ))

(DEFINE-INTEGRABLE-ONLY (GC-COPY-EXTEND LOC OBJ)
  (LET ((TEM (EXTEND-TEMPLATE OBJ)))
    (SELECT (POINTER-TAG TEM)
      ((%%EXTEND-TAG)
       ;; If already copied, return forwarded pointer.
       ;; We could do the NEWSPACE-CONTAINS? check, but that would be redundant.
       (NOTE-GC-REPEAT OBJ)
       (%SET-LOC-CONTENTS LOC TEM))
      ((%%TEMPLATE-TAG)
       (COND ((OLDSPACE-CONTAINS? TEM)
              (COND ((TEMPLATE-FORWARDED? TEM)
                     ;; Not a TPROC, because if it were, then OBJ would
                     ;; have been forwarded along with its template.
                     (GC-COPY-EXTEND-1 LOC OBJ (TEMPLATE-FORWARDED TEM)))
                    (ELSE
                     (LET ((NEW-UNIT (GC-MOVE-UNIT (TEMPLATE-UNIT TEM))))
                       (LET ((TEM (EXTEND-TEMPLATE OBJ)))
                         (COND ((EXTEND? TEM)
                                ;; OBJ got forwarded!  All done, trace unit.
                                (%SET-LOC-CONTENTS LOC TEM)
                                (GC-TRACE-UNIT NEW-UNIT))
                               (ELSE
                                ;; Closure.  Can't copy out unit until
                                ;;  extend gets forwarded.  (why?)
                                (GC-COPY-EXTEND-1 LOC OBJ
                                                  (TEMPLATE-FORWARDED TEM))
                                (GC-TRACE-UNIT NEW-UNIT))))))))
             (ELSE
              ;; Extend is in oldspace, but its template isn't.  Therefore,
              ;;  the extend cannot be a TPROC.
              (GC-COPY-EXTEND-1 LOC OBJ TEM))))
      (ELSE
       (GC-LOSER LOC OBJ "extend with horrible template")))))

;;; This routine explicitly ignores OBJ's template field.  Note that OBJ
;;; must be an extend which is not internal to a unit (i.e. not a TPROC).

(DEFINE (GC-COPY-EXTEND-1 LOC OBJ TEM)
  (LET ((CHUNK (TEMPLATE-CHUNK TEM)))
    (COND ((CHUNK-STANDARD-GC? CHUNK)
           (SET *GC-EXTEND-COUNT* (FX+ *GC-EXTEND-COUNT* 1))
           (LET ((P (CHUNK-POINTER-COUNT CHUNK))
                 (S (CHUNK-SCRATCH-COUNT CHUNK)))
             (LET ((NEW-OBJ (COPY-EXTEND OBJ TEM (FX+ P S))))
               (NOTE-GC-COPY OBJ NEW-OBJ)
               (%SET-LOC-CONTENTS LOC NEW-OBJ)
               (SET-EXTEND-FORWARDED OBJ NEW-OBJ)
               (GC-TRACE-EXTEND NEW-OBJ P)
               )))
          (ELSE
           (SET *GC-NONSTANDARD-COUNT*
                (FX+ *GC-NONSTANDARD-COUNT* 1))
           ((TEMPLATE-GC-METHOD TEM) LOC OBJ)))))

;;; Copy out zeroth slot last?  Why not.

(DEFINE (GC-TRACE-EXTEND OBJ SIZE)
  (IF (FX> SIZE 0)
      (LET ((LIMIT (FX- SIZE 1)))
        (DO ((I 0 (FX+ I 1)))
            ((FX>= I LIMIT)
             (GC-COPY (%XLOC OBJ LIMIT) (XREF OBJ LIMIT)))
          (GC-COPY (%XLOC OBJ I) (XREF OBJ I))))))

;;; And at last: the main dispatch for the GC.  This must appear in the file
;;; following all the DEFINE-INTEGRABLE-ONLY's defining routines that this
;;; calls... what lossage...

(DEFINE (GC-COPY LOC OBJ)
  (COND ((OLDSPACE-CONTAINS? OBJ)
         (SELECT (POINTER-TAG OBJ)
           ((%%EXTEND-TAG)   (GC-COPY-EXTEND   LOC OBJ))
           ((%%PAIR-TAG)     (GC-COPY-PAIR     LOC OBJ))
           ((%%STRING-TAG)   (GC-COPY-STRING   LOC OBJ))
           ((%%TEMPLATE-TAG) (GC-COPY-TEMPLATE LOC OBJ))
           ((%%FLONUM-TAG)   (GC-COPY-FLONUM   LOC OBJ))
           (ELSE             (%SET-LOC-CONTENTS LOC OBJ))))
        ((NONVALUE? OBJ)
         (GC-COPY LOC (NONVALUE->VALUE OBJ))
         (%SET-LOC-CONTENTS LOC (VALUE->NONVALUE (%LOC-CONTENTS LOC))))
        (ELSE (%SET-LOC-CONTENTS LOC OBJ))))

;;; Utility for use by population stuff.

(DEFINE (ALIVE-AFTER-GC? OBJ)
  (COND ((OLDSPACE-CONTAINS? OBJ)
         (SELECT (POINTER-TAG OBJ)
           ((%%EXTEND-TAG)   (  EXTEND-FORWARDED? OBJ))
           ((%%PAIR-TAG)     (    PAIR-FORWARDED? OBJ))
           ((%%STRING-TAG)   (  STRING-FORWARDED? OBJ))
           ((%%TEMPLATE-TAG) (TEMPLATE-FORWARDED? OBJ))
           ((%%FLONUM-TAG)   (  FLONUM-FORWARDED? OBJ))
           (ELSE             T)))
        ((NONVALUE? OBJ)
         (ALIVE-AFTER-GC? (NONVALUE->VALUE OBJ)))
        (ELSE T)))


;;; Nonstandard GC methods for weird extend types.

;;; We assume that the VECTOR-TEMPLATE is not an "active" object.

(DEFINE (GC-COPY-VECTOR LOC OBJ)
  (LET ((NEW-OBJ (%COPY-VECTOR OBJ)))
    (%SET-LOC-CONTENTS LOC NEW-OBJ)
    (NOTE-GC-COPY OBJ NEW-OBJ)
    (SET-EXTEND-FORWARDED OBJ NEW-OBJ)
    (GC-TRACE-EXTEND NEW-OBJ (VECTOR-LENGTH OBJ))))

(DEFINE (GC-COPY-BYTEV LOC OBJ)
  (LET ((NEW-OBJ (%COPY-BYTEV OBJ)))
    (%SET-LOC-CONTENTS LOC NEW-OBJ)
    (NOTE-GC-COPY OBJ NEW-OBJ)
    (SET-EXTEND-FORWARDED OBJ NEW-OBJ)))

;(DEFINE (GC-COPY-BITV LOC OBJ)
;  (LET ((NEW-OBJ (%COPY-BITV OBJ)))
;    (%SET-LOC-CONTENTS LOC NEW-OBJ)
;    (NOTE-GC-COPY OBJ NEW-OBJ)
;    (SET-EXTEND-FORWARDED OBJ NEW-OBJ)))

(DEFINE (GC-COPY-STRUCTURE LOC OBJ)
  (LET ((NEW-OBJ (COPY-STRUCTURE OBJ))) ; COPY-STRUCTURE is integrable!
    (%SET-LOC-CONTENTS LOC NEW-OBJ)
    (NOTE-GC-COPY OBJ NEW-OBJ)
    (SET-EXTEND-FORWARDED OBJ NEW-OBJ)
    (GC-TRACE-EXTEND NEW-OBJ (%STRUCT-SIZE OBJ))))


;;; Cf. the relocation code in LOAD.

(DEFINE (GC-COPY-UNIT LOC UNIT)
  (LET ((NEW-UNIT (GC-MOVE-UNIT UNIT)))
    (%SET-LOC-CONTENTS LOC NEW-UNIT)
    (GC-TRACE-UNIT NEW-UNIT)))

;;; This is called from several places.  Returns new unit.  Doesn't trace it.

(DEFINE (GC-MOVE-UNIT UNIT)
  (LET ((NEW-UNIT (COPY-UNIT UNIT)))
    (IF *GC-DEBUG?*
        (GC-FORMAT GC-OUTPUT ";Copying unit at ~X to ~X.~%"
                   (POINTER->FIXNUM UNIT)
                   (POINTER->FIXNUM NEW-UNIT)))
    (SET *GC-UNIT-COUNT* (FX+ *GC-UNIT-COUNT* 1))
    (SET-UNIT-FORWARDED UNIT NEW-UNIT)))

;;; Set forwardness of unit and any objects internal to it.
;;; The only memory this should touch is that inside the OLD unit.

(DEFINE (SET-UNIT-FORWARDED UNIT NEW-UNIT)
  (SET-EXTEND-FORWARDED UNIT NEW-UNIT)
  (LET ((UNIT-SIZE (CODE-STRUCTURE-AREA-INDEX (UNIT-CODE UNIT))))
    (ITERATE LOOP ((I 1))
      (COND ((FX< I UNIT-SIZE)
             (LET ((ITEM (XREF UNIT I)))
               (COND ((TEMPLATE-GUTS? ITEM)
                      ;; If I is odd then NEW-ITEM-LOC is even peso address.
                      (LET ((    TEM (MAKE-POINTER (%XLOC UNIT I)
                                                   %%TEMPLATE-TAG))
                            (NEW-TEM (MAKE-POINTER (%XLOC NEW-UNIT I)
                                                   %%TEMPLATE-TAG)))
                        (SET-TEMPLATE-FORWARDED TEM NEW-TEM)
                        (COND ((EQ? (XREF UNIT (FX+ I 2)) TEM)
                               ;; Should be able to just add in a const here.
                               (XSET UNIT (FX+ I 2)
                                     (CHANGE-TAG (POINTER-ADD NEW-TEM 1)
                                                 %%TEMPLATE-TAG
                                                 %%EXTEND-TAG))
                               (LOOP (FX+ I 4)))
                              (ELSE (LOOP (FX+ I 2))))))
                     (ELSE (LOOP (FX+ I 2))))))
            (ELSE NEW-UNIT)))))

(DEFINE-INTEGRABLE (INTERNAL-TO-UNIT? OBJ UNIT SIZE)
  (AND (POINTER-NOT-LESS? OBJ UNIT)
       ;; We would like to say: (%XLOC UNIT SIZE) instead of (POINTER-ADD ...)
       ;; but the compiler loses when we do.
       (POINTER-NOT-GREATER? OBJ (POINTER-ADD UNIT (POINTER-ASHR SIZE 1)))))

;(DEFINE (INTERNAL-TO-UNIT-CODE? OBJ CODE)      ; not used.
;  (AND (POINTER-NOT-LESS? OBJ CODE)
;       (POINTER-NOT-GREATER? OBJ (POINTER-ADD CODE (CODE-SIZE CODE)))))

(DEFINE (ACTUAL-CODE-SIZE CODE)
  (LET ((Z (CODE-CODE-SIZE CODE)))      ; Ugh!
    (COND ((POINTER-LESS? Z 4)          ; Arbitrary threshhold.  Should usually
           (CODE-SIZE CODE))            ; be 1 if it's not the real size.
          (ELSE
           ;; Round machine number up to nearest fixnum.  Gross me out!
           (POINTER-ADDRESS (POINTER-ADD Z (FIXNUM->POINTER 7)))))))

;;; Make verbatim copy of unit, except for internal pointers, which should
;;;  get adjusted.
;;; After the initial copy, the only memory this routine should touch is that
;;;  inside the NEW unit.

(DEFINE (COPY-UNIT UNIT)
  (LET ((CODE (UNIT-CODE UNIT)))
    (LET ((UNIT-SIZE (CODE-STRUCTURE-AREA-INDEX CODE))
          (NEW-CODE (LET ((SIZE (ACTUAL-CODE-SIZE CODE)))
                      (COPY-MEM (MAKE-CODE SIZE) CODE (FIXNUM-ASHL SIZE 1)))))
      (LET ((NEW-UNIT (MAKE-EXTEND-N *UNIT-TEMPLATE* UNIT-SIZE)))
        (COPY-MEM NEW-UNIT UNIT UNIT-SIZE)
        (SET (UNIT-CODE NEW-UNIT) NEW-CODE)
        (LET ((UNIT-DISPLACEMENT (POINTER-SUBTRACT NEW-UNIT UNIT))
              (CODE-DISPLACEMENT (POINTER-SUBTRACT NEW-CODE CODE)))
          (ITERATE LOOP ((I 0))
            (COND ((FX>= I UNIT-SIZE) NEW-UNIT)
                  (ELSE
                   (LET ((ITEM (XREF NEW-UNIT I)))
                     (COND ((TEMPLATE-GUTS? ITEM)
                            (LET ((NEW-TEM (MAKE-POINTER (%XLOC NEW-UNIT I)
                                                         %%TEMPLATE-TAG)))
                              (SET (TEMPLATE-CHUNK NEW-TEM)
                                   (POINTER-ADD (TEMPLATE-CHUNK NEW-TEM)
                                                CODE-DISPLACEMENT)))
                            (LOOP (FX+ I 2)))
                           (ELSE
                            (SELECT (POINTER-TAG ITEM)
                              ((%%TEMPLATE-TAG %%EXTEND-TAG)
                               (COND ((INTERNAL-TO-UNIT? ITEM UNIT UNIT-SIZE)
                                      (XSET NEW-UNIT I
                                            (POINTER-ADD ITEM UNIT-DISPLACEMENT))))
                               (LOOP (FX+ I 1)))
                              (ELSE (LOOP (FX+ I 1)))))
                           ))))))))))

(DEFINE (GC-TRACE-UNIT UNIT)
  (GC-TRACE-UNIT-1 UNIT (CODE-STRUCTURE-AREA-INDEX (UNIT-CODE UNIT))))

(DEFINE (GC-TRACE-UNIT-1 UNIT UNIT-SIZE)
  (ITERATE LOOP ((I 0))
    (COND ((FX< I UNIT-SIZE)
           (LET ((ITEM (XREF UNIT I)))
             (COND ((TEMPLATE-GUTS? ITEM)
                    ;; If unit is inactive, then so is code.  Do nothing.
                    (LOOP (FX+ I 2)))
                   (ELSE
                    (GC-COPY (%XLOC UNIT I) ITEM)
                    (LOOP (FX+ I 1)))))))))
