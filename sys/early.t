(HERALD (TSYS EARLY T 146)
        (ENV TSYS)
        (PRE-COOK))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Random low-level system code

;;; There's not really any pattern to what's in this file.  It simply consists
;;; of any routines which are needed in the initialization sequence, or don't
;;; belong anywhere else, or are very low-level (e.g. grovel through the stack
;;; or interrupt frames), or need to be callable by the GC (and thus should be
;;; pre-cooked).

(LSET ++ '++)                   ; Make vcells accessible from debugger
(LSET ** '**)

;;; Operation stuff needs (?)

(DEFINE (TRUE . X) (IGNORE X) T)
(DEFINE (FALSE . X) (IGNORE X) NIL)

;;; CHECK-ARG is called early and called often.  So here it is in the flesh.

(DEFINE-INTEGRABLE (CHECK-ARG PREDICATE EXPRESSION FN)
  (COND ((PREDICATE EXPRESSION) EXPRESSION)
        (ELSE (*CHECK-ARG PREDICATE EXPRESSION FN))))

;;; Consers

;;; For MAKE-EXTEND-N, SIZE is longwords; for MAKE-EXTEND, it is quadwords.
;;; Note that constant folding will reduce the 2nd arg to MAKE-EXTEND.
;;; MAKE-EXTEND should be renamed to be %MAKE-EXTEND.
;;; MAKE-EXTEND-N should be renamed to be MAKE-EXTEND.

(DEFINE-INTEGRABLE (MAKE-EXTEND-N TEM SIZE)
  (MAKE-EXTEND TEM (FIXNUM-ASHR (FX+ SIZE 2) 1)))


;; Given number of useful pesos in a vector-type extend, return number 
;; of quadwords needed to represent it

(DEFINE-INTEGRABLE (PESO-COUNT->EXTEND-SIZE SIZE)
  (FIXNUM-ASHR (FX+ SIZE 4) 1))

;;; "Vector" implies the -1 elt is the template, -2 elt is the size
;;; "EPP" is "elts per peso" (This should at least use CEILING or something.)
;;; Should check available space here.

(DEFINE (MAKE-VECTOR-EXTEND TEM SIZE SIZE-CONVERTER)
  (LET ((SIZE (CHECK-ARG NONNEGATIVE-FIXNUM? SIZE MAKE-VECTOR)))
    (LET ((SIZE-IN-QUADS (PESO-COUNT->EXTEND-SIZE (SIZE-CONVERTER SIZE))))
      (COND ((FX> SIZE-IN-QUADS 5000000)        ;Random number, fix later
             (ERROR "attempt to create a ridiculously large vector~%  ~S"
                    `(MAKE-VECTOR ,SIZE)))
            (ELSE
             (GC-DEFER
              (LET ((V (POINTER-ADD (MAKE-EXTEND 0 SIZE-IN-QUADS) 1)))
                (SET (EXTEND-TEMPLATE V) TEM)
                (SET (VECTOR-LENGTH V) SIZE)   ; This is a gross hack.
                V)))))))

(DEFINE (MAKE-VECTOR SIZE)
  (MAKE-VECTOR-EXTEND *VECTOR-TEMPLATE*
                      SIZE
                      (LAMBDA (SIZE) SIZE)))

(DEFINE (MAKE-BYTEV SIZE)
  (MAKE-VECTOR-EXTEND *BYTEV-TEMPLATE*
                      SIZE
                      (LAMBDA (SIZE) (FIXNUM-ASHR (FX+ SIZE 3) 2))))

;(DEFINE (MAKE-BITV SIZE)
;  (MAKE-VECTOR-EXTEND *BITV-TEMPLATE*
;                      SIZE
;                      (LAMBDA (SIZE) (FIXNUM-ASHR (FX+ SIZE 31) 5))))

(DEFINE (MAKE-VCELL ID)
  (LET ((VCELL (MAKE-EXTEND-N *VCELL-TEMPLATE* %%VCELL-SIZE)))
    (SET (VCELL-ID VCELL) ID)
    (SET (VCELL-INFO VCELL) NIL)
    (SET (VCELL-CONTENTS VCELL) (VALUE->NONVALUE VCELL))
    VCELL))

(DEFINE (MAKE-SYMBOL PNAME)
  (LET ((SYMBOL (MAKE-EXTEND-N *SYMBOL-TEMPLATE* %%SYMBOL-SIZE)))
    (SET (SYMBOL-PNAME SYMBOL) PNAME)
    (SET (SYMBOL-VCELL SYMBOL) NIL)
    (SET (SYMBOL-PLIST SYMBOL) '())
    SYMBOL))

(DEFINE (MAKE-BOGUS-ENTITY PROC HDLR)
  (LET ((OBJ (MAKE-EXTEND-N *BOGUS-ENTITY-TEMPLATE* %%BOGUS-ENTITY-SIZE)))
    (SET (BOGUS-ENTITY-PROCEDURE OBJ) PROC)
    (SET (BOGUS-ENTITY-HANDLER OBJ) HDLR)
    OBJ))

(DEFINE %OBJECT MAKE-BOGUS-ENTITY)

;;; Random utility (needed in setup for OPEN, PRIMOPS, etc.)

(define (make-accessor fetch store)
  (object fetch
          ((setter self) store)
          ((print-type-string self) "Accessor")
          ((identification self) (identification fetch))
          ((get-loaded-file self) (get-loaded-file fetch))))

(DEFINE (MAKE-XENOID PTR)
  (LET ((OBJ (MAKE-EXTEND-N *XENOID-TEMPLATE* %%XENOID-SIZE)))
    (SET (XENOID-POINTER OBJ) PTR)
    OBJ))

(DEFINE-INTEGRABLE (MAKE-CODE SIZE)     ; used by GC and by LOAD-RAW
  (POINTER-ADDRESS (MAKE-EXTEND 0 SIZE)))       ; kludge!

;;; We should be GC-DEFERing here...
(DEFINE (COPY-EXTEND OBJ TEM SIZE)
  (LET ((NEW-OBJ (MAKE-EXTEND-N TEM SIZE)))      ;-N - UK
    (COPY-MEM NEW-OBJ OBJ SIZE)))

;;; Template of dest extend is unaffected.

(DEFINE (COPY-EXTEND! TO-OBJ FROM-OBJ SIZE)
  (COPY-MEM TO-OBJ FROM-OBJ SIZE))

;;; GC needs COPY-VECTOR.

(DEFINE (%COPY-VECTOR VECTOR)
  (LET ((LEN (VECTOR-LENGTH VECTOR)))
    (%VECTOR-REPLACE (MAKE-VECTOR LEN) VECTOR LEN)))    ; .see OPEN.T

(DEFINE (%COPY-BYTEV BYTEV)
  (LET ((LEN (BYTEV-LENGTH BYTEV)))
    (COPY-MEM (MAKE-BYTEV LEN)
              BYTEV
              (FIXNUM-ASHR (FX+ LEN 3) 2))))

;
;;; Random stuff

;;; Make a copy of a string.  This really belongs in the STRING module.

(DEFINE (COPY-STRING STRING)
  (%COPY-STRING (CHECK-ARG STRING? STRING COPY-STRING)))

(DEFINE (%COPY-STRING STRING)
  (LET ((LEN (STRING-LENGTH STRING)))
    (STRING-REPLACE (%MAKE-STRING LEN) STRING LEN)))

(DEFINE (VECTOR-FILL VECTOR VALUE)      ; This really belongs in VECTOR module,
  (LET ((SIZE (VECTOR-LENGTH VECTOR)))  ; but it is needed here.
    (DO ((I 0 (FX+ I 1)))
        ((FX>= I SIZE) VECTOR)
      (VSET VECTOR I VALUE))))

;;; General symbol table stuff.

(DEFINE (MAKE-SYMBOL-TABLE SIZE)
  (VECTOR-FILL (MAKE-VECTOR SIZE) '()))

;;; INTERN and REALLY-INTERN are exactly the same except that REALLY-INTERN is
;;; allowed to use the string provided for the symbol's pname if the
;;; symbol doesn't already exist.  Note, however, that in addition to
;;; wanting to avoid consing, REALLY-INTERN bypasses the call to ENFORCE.
;;; This is because the bootstrap sequence calls RELOCATE-UNIT, which
;;; doesn't want to depend on the existence of ENFORCE, because the file
;;; that defines ENFORCE might itself need relocating.  This is a kludge
;;; and should be fixed at some point.

;;; INTERN, v.t. To confine or impound, esp. during a war.  (Webster.)

(DEFINE (INTERN STRING SYMBOL-TABLE)
  (LET ((STRING (CHECK-ARG STRING? STRING INTERN)))
    (INTERN-1 STRING
              SYMBOL-TABLE
              (LAMBDA (STRING)
                (MAKE-SYMBOL (%COPY-STRING STRING))))))

(DEFINE (REALLY-INTERN STRING SYMBOL-TABLE)
  ;; Speed hack: avoid doing the CHECK-ARG.
  (INTERN-1 STRING
            SYMBOL-TABLE
            MAKE-SYMBOL))

(DEFINE (INTERNED STRING SYMBOL-TABLE)
  (LET ((STRING (CHECK-ARG STRING? STRING INTERNED)))
    (INTERN-1 STRING
              SYMBOL-TABLE
              FALSE)))

(DEFINE (INTERNED? STRING SYMBOL-TABLE)
  (TRUE? (INTERNED STRING SYMBOL-TABLE)))

(DEFINE (INTERN-SYMBOL SYMBOL SYMBOL-TABLE)
  (LET ((SYMBOL (CHECK-ARG SYMBOL? SYMBOL INTERN-SYMBOL)))
    (INTERN-1 (SYMBOL-PNAME SYMBOL)
              SYMBOL-TABLE
              (LAMBDA (STRING) (IGNORE STRING) SYMBOL))))       ; Cons!

(DEFINE (INTERN-1 STRING SYMBOL-TABLE NOT-THERE)
  (LET ((INDEX (FIXNUM-REMAINDER (STRING-HASH STRING)
                                 (VECTOR-LENGTH SYMBOL-TABLE))))
    (LET ((BUCKET (VREF SYMBOL-TABLE INDEX)))
      (ITERATE LOOP ((L BUCKET))
        (COND ((NULL? L)
               (LET ((NEW-SYMBOL (NOT-THERE STRING)))
                 (IF NEW-SYMBOL
                     (VSET SYMBOL-TABLE INDEX (CONS NEW-SYMBOL BUCKET)))
                 NEW-SYMBOL))
              ((%STRING-EQUAL? STRING (SYMBOL-PNAME (CAR L)))
               (CAR L))
              (ELSE (LOOP (CDR L))))))))

;;; STRING->SYMBOL uses one (global) symbol table in particular.

(DEFINE *THE-SYMBOL-TABLE* (MAKE-SYMBOL-TABLE 2039))    ; 2039 is prime

(DEFINE (STRING->SYMBOL STRING)
  (INTERN STRING *THE-SYMBOL-TABLE*))

(DEFINE (REALLY-STRING->SYMBOL STRING)
  (REALLY-INTERN STRING *THE-SYMBOL-TABLE*))

;;; Initialization loop: transfer the initial symbols into the symbol table.

(DO ((LEN (VECTOR-LENGTH *THE-INITIAL-SYMBOLS*))
     (I 0 (FX+ I 1)))
    ((FX= I LEN) 'DONE)
  (LET ((SYMBOL (VREF *THE-INITIAL-SYMBOLS* I)))
    (LET ((INDEX (FIXNUM-REMAINDER (STRING-HASH (SYMBOL-PNAME SYMBOL))
                                   (VECTOR-LENGTH *THE-SYMBOL-TABLE*))))
      (VSET *THE-SYMBOL-TABLE*
            INDEX
            (CONS SYMBOL (VREF *THE-SYMBOL-TABLE* INDEX)))
      )))

;;; Env lookup hack for RELOC.

(DEFINE (*SYSTEM-BOOT-ENV* ID LOCAL? CREATE?)
  (IGNORE LOCAL?)
  (OR (SYMBOL-VCELL ID)
      (IF CREATE?
          (SET (SYMBOL-VCELL ID)
               (MAKE-VCELL ID))
        NIL)))

(SET *THE-BOOT-ENV* *SYSTEM-BOOT-ENV*)

;;; Env warning messages.

(LSET *PRINT-ENV-WARNINGS?* T)

(LSET *PROBLEMS* '())

(DEFINE (ENV-WARN MSG ID)
  (COND (*Z?*
         (PUSH ID *PROBLEMS*))
        (ELSE
         (REALLY-ENV-WARN MSG ID))))

;;; Stub for FORMAT.

(DEFINE (FORMAT STREAM F . STUFF)
  (COND (*Z?*
         (APPLY ZFORMAT STREAM F STUFF))
        (ELSE
         (REALLY-FORMAT STREAM F STUFF))))

;;; AEFAULT needs this for FIM-fault lookup table.

(DEFINE (%ASSQ OBJ LIST)
  (ITERATE LOOP ((L LIST))
    (IF (NULL? L) NIL
      (LET ((Z (CAR L)))
        (COND ((EQ? OBJ (CAR Z)) Z)
              (ELSE (LOOP (CDR L))))))))
