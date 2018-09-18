(HERALD (TCOMP EMIT T 508)
        (ENV TCOMP))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Code emitters

(LSET *PRE-COOK?* NIL)
(LSET *DOING-SYSGEN?* NIL)
(LSET *GENERATE-QUEUE* NIL)

(WALK (LAMBDA (X) (PUT (CAR X) 'SIZE (CADR X)))
      '((PESO 4)
        (ADDRESS 4)
        (EXTERNAL-ADDRESS 4)
        (VCELL-POINTER 4)
        (POINTER 4)
        (HALFPESO 2)
        (BYTE 1)))

;;; Where does this really belong?

(LSET *GENTAGNUM* 0)
(LSET *GENTAG-QUALIFIER* NIL)

(DEFINE (GENTAG PREFIX)
  (SET *GENTAGNUM* (FX+ *GENTAGNUM* 1))
  (COND ((NOT *ENABLE-LAP-COMMENTARY?*)
         (CONS 'LABEL *GENTAGNUM*))
        (*GENTAG-QUALIFIER*
         (CONCATENATE-SYMBOL PREFIX
                     '-
                     *GENTAGNUM*
                     '-
                     *GENTAG-QUALIFIER*))
        (ELSE (CONCATENATE-SYMBOL PREFIX
                          '-
                          *GENTAGNUM*))))

(DEFINE (TARGET:FIXNUM? X)
  (FIXNUM? X))                ; temporary kludge

(DEFINE (TARGET:FLONUM? X) (FLONUM? X))

;;; T iff the object file is to be in LISP instead of native assembler syntax.

(LSET *LISP-ASSEMBLY-SYNTAX?* NIL)

(DEFINE (TEXT-SECTION)
  (COND ((NEQ? *ASSEMBLY-SECTION* 'TEXT)
         (SET *ASSEMBLY-SECTION* 'TEXT)
         (COND (*LISP-ASSEMBLY-SYNTAX?* (EMIT '(TEXT))) ; Ugh
               (ELSE (REALLY-EMIT '(TEXT)))))))

(STAT-COUNTER *DATA-SECTION-ENTRY-COUNT* "number of entries into data section")

(DEFINE (DATA-SECTION)
  (COND ((NEQ? *ASSEMBLY-SECTION* 'DATA)
         (SET *ASSEMBLY-SECTION* 'DATA)
         (COND (*LISP-ASSEMBLY-SYNTAX?* (EMIT '(DATA)))
               (ELSE (REALLY-EMIT '(DATA))))
         (EMIT-LINKAGE-AREA))))

(DEFINE (EMIT-LINKAGE-AREA)
  ;; Hack.  Ugh.
  (LET ((L (REVERSE! *THE-LINKAGE-AREA*)))
    (SET *THE-LINKAGE-AREA* '())
    (WALK (LAMBDA (X) (X)) L))
  (INCR *DATA-SECTION-ENTRY-COUNT*))

(DEFINE (BEGIN-ALIGNED-PURE-DATUM)
  (TEXT-SECTION)
  (EMIT '(ALIGN 3)))

(DEFINE (BEGIN-ALIGNED-IMPURE-DATUM)
  (DATA-SECTION)
  (COND ((NULL? *OFFSET*) (EMIT '(ALIGN 3)) (SET *OFFSET* -10001)) ; kludge!
        ((EVEN? (FIXNUM-IDENTITY *OFFSET*)) (EMIT-TO-UNIT '(PESO 8)))))

(DEFINE (MEVAL-LAP-OFFSET X)
  (COND ((INTEGER? X) X)
        ((SYMBOL? X)
         (LET ((PROBE (GET X 'SYM)))
           (IF PROBE (MEVAL-LAP-OFFSET PROBE) X)))
        ((NOT (PAIR? X)) (BUG))
        (ELSE
         (XCASE (CAR X)
                ((~)
                 (LET ((Z (MEVAL-LAP-OFFSET (CADR X))))
                   (IF (INTEGER? Z) (- 0 Z) X)))
                ((+ - * /)
                 (LET ((Z1 (MEVAL-LAP-OFFSET (CADR X)))
                       (Z2 (MEVAL-LAP-OFFSET (CADDR X))))
                   (COND ((AND (INTEGER? Z1) (INTEGER? Z2))
                          ((CASE (CAR X)
                                 ((+) +)
                                 ((-) -)
                                 ((*) *)
                                 ((/) DIV))
                           Z1 Z2))
                         (ELSE X))))
                ((<< >>) `(,(CAR X) ,(MEVAL-LAP-OFFSET (CADR X))
                                    ,(MEVAL-LAP-OFFSET (CADDR X))))
                ((COMMA)
                 (MEVAL-LAP-OFFSET (EVAL (CADR X) *TC-ENV*)))
                ((LABEL VERBATIM) X)))))

(PUT 'NULL-CHARACTER 'SYM '%%CHAR-TAG)

;;; Isn't the following gratuitous?

(DEFINE (TARGET:CHAR->ASCII CHAR)
  (CHAR->ASCII CHAR))

(DEFINE (OUTPUT-STRING TAG STRING)
  (LET ((TEXTTAG (GENTAG 'TXT)))
    (BEGIN-ALIGNED-IMPURE-DATUM)
    (OUTPUT-STRING-HEADER TAG TEXTTAG STRING)
    (SET *OFFSET* (FX+ *OFFSET* 2))
    ;; Text always goes in pure space, at least for now
    (TEXT-SECTION)
    (OUTPUT-STRING-TEXT TEXTTAG STRING)))

;;; See EMIT-SUPPORT for OUTPUT-STRING-HEADER and OUTPUT-STRING-TEXT


;;; The following stuff has to do with code chunks, relocation, and all that.

(DEFINE-LAP-CONSTANT %%REL-ITEM-TYPE-FIELD-POS  27)     ; bits 27 through 31
(DEFINE-LAP-CONSTANT %%REL-ITEM-TYPE-FIELD-SIZE 5)

(DEFINE-LAP-CONSTANT %%REL-ITEM-INDEX-FIELD-POS  3)     ; bits 3 through 28
(DEFINE-LAP-CONSTANT %%REL-ITEM-INDEX-FIELD-SIZE 24)    ; 5 + 24 + 3 = 32

(DEFINE-LAP-CONSTANT %%TEMPLATE-REL-TYPE 0)
(DEFINE-LAP-CONSTANT %%NULL-REL-TYPE     1)     ; Null
(DEFINE-LAP-CONSTANT %%INTEGER-REL-TYPE  2)     ; Bignum
(DEFINE-LAP-CONSTANT %%FILENAME-REL-TYPE 3)     ; Filename
(DEFINE-LAP-CONSTANT %%FETCH-REL-TYPE    4)     ; Fetch from another unit slot
(DEFINE-LAP-CONSTANT %%TRUE-REL-TYPE     5)     ; True object
(DEFINE-LAP-CONSTANT %%SYMBOL-REL-TYPE   8)     ; Symbol
(DEFINE-LAP-CONSTANT %%FREE-REL-TYPE    12)     ; Variable reference
(DEFINE-LAP-CONSTANT %%SET-REL-TYPE     13)     ; SET
(DEFINE-LAP-CONSTANT %%LOCAL-REL-TYPE   16)     ; DEFINE
(DEFINE-LAP-CONSTANT %%LSET-REL-TYPE    20)     ; LSET
(DEFINE-LAP-CONSTANT %%VTEM-REL-TYPE    24)     ; Vector template

;;; This is an a-list (static linktag . structtag)

(COMPILATION-GLOBAL *THE-VCELLS*)

;;; Return an operand which accesses pointer to value cell.  (?)

(DEFINE (STATIC-LOC STATIC REFTYPE)
  (UNIT-REF (UNIT-STATIC-TAG STATIC REFTYPE)))

(STAT-COUNTER *VALUE-CELL-COUNT* "number of value cell pointers in unit")

;;; Return tag within unit for peso which points to a value cell.

(DEFINE (UNIT-STATIC-TAG STATIC REFTYPE)
  (LET* ((STATIC (IF (STATIC? STATIC) STATIC (->STATIC STATIC T)))
         (PROBE (ASSQ STATIC *THE-VCELLS*)))
    (COND (PROBE
           (COND ((MEMQ REFTYPE '(DEF SETQ LSET))
                  (SET (CADR PROBE) REFTYPE)))
           (CADDR PROBE))
          (ELSE
           (LET* ((VTAG (GENTAG 'STATIC))
                  (STUFF (LIST STATIC REFTYPE VTAG '() )))
             (COND ((NOT *SUPPRESS-FLINK?*)
                    (INCR *VALUE-CELL-COUNT*)
                    (COND (*BUFFER-LINKAGE-AREA?*
                           (PUSH *THE-LINKAGE-AREA*
                                 (QLOZURE (STUFF)
                                   (LAMBDA ()
                                     (EMIT-VCELL-LINK STUFF)))))
                          (ELSE
                           ;; Lose, must emit it right away.
                           (DATA-SECTION)
                           (EMIT-VCELL-LINK STUFF)
                           (TEXT-SECTION)))))
             (PUSH *THE-VCELLS* STUFF)
             VTAG)))))

(DEFINE (EMIT-VCELL-LINK STUFF)
  ;; STUFF is (STATIC REFTYPE VTAG FOOTAG . SYMTAG)
  (LET ((STATIC (CAR STUFF))
        (VTAG (CADDR STUFF)))
    (EMITTAG VTAG)
    (EMITREMARK "Value cell pointer")
    (COND (*PRE-COOK?*
           (LET ((XTAG
                  (MAKE-SPECIAL-TAG 'V (STATIC-IDENTIFIER STATIC))))
             (IF (NOT *DOING-SYSGEN?*) (EMIT `(EXTERNAL ,XTAG)))
             (EMIT-TO-UNIT `(ADDRESS ,XTAG))))
          (ELSE
           (LET ((FOOTAG (GENTAG 'YFOO)))
             (SET (CADDDR STUFF) FOOTAG)
             (EMIT-TO-UNIT `(PESO ,FOOTAG)))))))

;;; Return an operand for a flonum in DWFLO representation.

(DEFINE (FLONUM-LOC OBJ)
  (LET ((TAG (GENTAG 'FL)))
    (ENQUEUE *GENERATE-QUEUE*
             (QLOZURE (OBJ TAG)
               (LAMBDA ()
                 (EMIT '(ALIGN 2))
                 (EMITTAG TAG)
                 (OUTPUT-FLONUM OBJ))))
    `(REL ,TAG)))

;;; Returns an operand, to be used in code.

(DEFINE (QUOTE-LOC OBJ)
  (COND ((NULL? OBJ) '(SLINK NULL))
        ((EQ? OBJ T) '(SLINK TRUE))     ; ?
        ((CHAR? OBJ)
         `(LIT (+ %%CHAR-TAG
                  ,(LSH (TARGET:CHAR->ASCII OBJ) *SHIFT*))))
        ((TARGET:FIXNUM? OBJ)
         `(LIT ,(* OBJ 8)))   ;; NOT FX* !!!  
        (ELSE
         (LET ((QTAG (GENTAG 'QUOTE)))
           (COND (*BUFFER-LINKAGE-AREA?*
                  (PUSH *THE-LINKAGE-AREA*
                        (QLOZURE (QTAG OBJ)
                          (LAMBDA ()
                            (EMIT-QUOTE-LINK QTAG OBJ)))))
                 (ELSE
                  (DATA-SECTION)
                  (EMIT-QUOTE-LINK QTAG OBJ)
                  (TEXT-SECTION)))
           (UNIT-REF QTAG)))))

(DEFINE (EMIT-QUOTE-LINK QTAG OBJ)
  (EMITTAG QTAG)
  (EMIT-VIRTUAL-POINTER OBJ))

(DEFINE (EMIT-TO-UNIT X)
  (EMIT X)
  (INCR *OFFSET*))

(DEFINE (EMIT-VANILLA X)
  (EMIT-TO-UNIT (COND (*PRE-COOK?* `(ADDRESS ,X))
                      (ELSE        `(PESO (- ,X ,*THE-UNIT-0-TAG*))))))

;;; Emit a peso which is to hold a pointer to OBJ.

(DEFINE (EMIT-VIRTUAL-POINTER OBJ)
  (EMIT-TO-UNIT (EMIT-VIRTUAL-POINTER-1 OBJ)))

(DEFINE (EMIT-VIRTUAL-POINTER-1 OBJ)
  (COND ((NULL? OBJ)
         (COND (*PRE-COOK?* '(ADDRESS NULL))
               (ELSE '(PESO (+ (<< %%NULL-REL-TYPE
                                   %%REL-ITEM-TYPE-FIELD-POS)
                               %%REL-ITEM-TAG)))))
        ((CHAR? OBJ)
         `(PESO (+ %%CHAR-TAG
                   ,(LSH (TARGET:CHAR->ASCII OBJ) *SHIFT*))))
        ((TARGET:FIXNUM? OBJ)
         `(PESO ,(LSH OBJ *SHIFT*)))
        ;; String case must precede symbol case for Maclisp.
        ((STRING? OBJ)
         (COND (*PRE-COOK?* `(ADDRESS ,(GET-STRING-TAG OBJ)))
               (ELSE `(PESO (- ,(GET-STRING-TAG OBJ) ,*THE-UNIT-0-TAG*)))))
        ((SYMBOL? OBJ)
         (LET ((STUFF (UNIT-INTERN OBJ)))
           (COND (*PRE-COOK?*
                  `(ADDRESS ,(MAKE-SPECIAL-TAG 'S OBJ)))
                 (ELSE
                  `(PESO ,(OR (CADDR STUFF)
                              (LET ((FOOTAG (GENTAG 'ZFOO)))
                                (SET (CADDR STUFF) FOOTAG)
                                FOOTAG)))))))
        ((PAIR? OBJ)
         (EMIT-VIRTUAL-PAIR OBJ))
        ((FLONUM? OBJ)
         (LET ((TAG (GENTAG 'FL)))
           (PUSH *THE-FLONUMS* (CONS TAG OBJ))
           `(CODE-ADDRESS (+ ,TAG %%FLONUM-TAG))))
        ((VECTOR? OBJ)
         (EMIT-VIRTUAL-VECTOR OBJ))
        ((INTEGER? OBJ)
         (EMIT-INTEGER OBJ))
        ((EQ? OBJ *TRUE-OBJECT*)
         (COND (*PRE-COOK?*
                `(ADDRESS ,(MAKE-SPECIAL-TAG 'S 'T)))   ; temp hack
               (ELSE
                '(PESO (+ (<< %%TRUE-REL-TYPE
                              %%REL-ITEM-TYPE-FIELD-POS)
                           %%REL-ITEM-TAG)))))
        ((FILENAME? OBJ)
         (EMIT-VIRTUAL-FILENAME OBJ NIL))
        (ELSE
         (EMIT-VIRTUAL-OTHER OBJ))))

(DEFINE (EMIT-VIRTUAL-OTHER OBJ)
  (BUG "don't yet know how to emit this kind of quoted structure: ~S"
       "will use the symbol **BAD-QUOTATION-BUG-VALUE** in its place"
       OBJ)
  (EMIT-VIRTUAL-POINTER-1 '**BAD-QUOTATION-BUG-VALUE**))

(DEFINE (EMIT-VIRTUAL-PAIR OBJ)
  (LET ((TAG (GET-PAIR-TAG OBJ)))
    (COND (*PRE-COOK?*
           `(ADDRESS (+ ,TAG %%PAIR-TAG)))
          (ELSE
           `(PESO (- (+ ,TAG %%PAIR-TAG) ,*THE-UNIT-0-TAG*))))))

(DEFINE (GET-PAIR-TAG OBJ)
  (LET ((TAG (GENTAG 'PAIR)))
    (PUSH *THE-STRUCTURE-AREA*
          (QLOZURE (TAG OBJ)
            (LAMBDA ()
              (BEGIN-ALIGNED-IMPURE-DATUM)
              (EMITTAG TAG)
              (EMIT-VIRTUAL-POINTER (CAR OBJ))
              (EMIT-VIRTUAL-POINTER (CDR OBJ))
              )))
    TAG))

(DEFINE (EMIT-VIRTUAL-VECTOR OBJ)
  (LET ((TAG (GENTAG 'VEC)))
    (PUSH *THE-STRUCTURE-AREA*
          (QLOZURE (TAG OBJ)
            (LAMBDA ()
              (BEGIN-ALIGNED-IMPURE-DATUM)
              (EMIT-VIRTUAL-POINTER NIL)       ; Garbage word
              (EMIT-VIRTUAL-POINTER (VECTOR-LENGTH OBJ))
              (EMIT-VECTOR-TEMPLATE)
              (EMITTAG TAG)
              (DO ((I 0 (FX+ I 1)))
                  ((FX= I (VECTOR-LENGTH OBJ)))
                (EMIT-VIRTUAL-POINTER (VREF OBJ I))))))
    (COND (*PRE-COOK?*
           `(ADDRESS ,TAG))
          (ELSE
           `(PESO (- ,TAG ,*THE-UNIT-0-TAG*))))))

(DEFINE (EMIT-VECTOR-TEMPLATE)
  (COND (*PRE-COOK?*
         (EMIT-TO-UNIT '(ADDRESS VECTOR-TEMPLATE)))
        (ELSE
         (EMIT-TO-UNIT '(PESO (+ %%REL-ITEM-TAG
                                 (<< %%VTEM-REL-TYPE
                                     %%REL-ITEM-TYPE-FIELD-POS)))))))

(DEFINE (EMIT-INTEGER N)
  (LET ((SIGN (IF (LESS? N 0) -1 1)))
    (DO ((N (ABS N) (DIV N 65536.))     ; random number
         (L '() (CONS (REMAINDER N 65536.) L)))
        ((ZERO? N)
         `(PESO (+ %%REL-ITEM-TAG
                   (- ,(GET-PAIR-TAG (CONS SIGN L))
                      ,*THE-UNIT-0-TAG*)
                   (<< %%INTEGER-REL-TYPE
                       %%REL-ITEM-TYPE-FIELD-POS)))))))

(LSET *RELOC-CAN-DEAL-WITH-FILENAMES?* NIL)

(DEFINE (EMIT-VIRTUAL-FILENAME FNAME KLUDGE?)
  (LET* ((DIR (FILENAME-DIR FNAME))
         (L `(,(FS-NAME (OR (FILENAME-FS FNAME) (LOCAL-FS)))
              ,DIR
              ,(FILENAME-NAME FNAME)
              ,(FILENAME-TYPE FNAME)
              ,(FILENAME-GENERATION  FNAME))))
    (COND ((OR *PRE-COOK?*
               (NOT *RELOC-CAN-DEAL-WITH-FILENAMES?*)
               (AND KLUDGE?
                    (OR (EQ? DIR 'T)
                        (EQ? DIR 'TSYS))))
           (EMIT-VIRTUAL-POINTER-1 `(**FILENAME** ,@L)))
          (ELSE
           `(PESO (+ %%REL-ITEM-TAG
                     (- ,(GET-PAIR-TAG L) ,*THE-UNIT-0-TAG*)
                     (<< %%FILENAME-REL-TYPE
                         %%REL-ITEM-TYPE-FIELD-POS)))))))

(COMPILATION-GLOBAL *THE-STRINGS*)

(DEFINE (GET-STRING-TAG STRING)
  (LET ((TAG (GENTAG 'STR)))
    (PUSH *THE-STRINGS* (CONS* STRING TAG '()))
    TAG))

(COMPILATION-GLOBAL *THE-SYMBOLS*)

;;; Returns list (SYMBOL TAG () . ()) for peso in scratch area which is to hold
;;; a pointer to a symbol.  Extra slots are for (SYMBOL TAG FOOTAG . PNTAG).

(DEFINE (UNIT-INTERN SYM)
  (OR (ASSQ SYM *THE-SYMBOLS*)
      ;; STUFF is (SYMBOL TAG FOOTAG PNTAG . TEXTTAG)
      (LET ((STUFF (LIST SYM (GENTAG 'SYM) NIL NIL)))
        (PUSH *THE-SYMBOLS* STUFF)
        STUFF)))

;;; Generate an operand which accesses into the unit.

(DEFINE (UNIT-REF TAG)
  (COND (*TP-LOC*
         `(UREF ,TAG ,*TP-LOC*))
        (*PRE-COOK?*
         `(DATUM ,TAG))
        (ELSE
         (BUG "trying to do (UNIT-REF ~S), but *TP-LOC* is nil"
              "will use a DATUM operand, but it will lose")
         `(DATUM ,TAG))))

;;; Glom hum futz gluck.

(DEFINE (UNIT-ADDR-REF TAG) `(ADDR ,(UNIT-REF TAG)))

(DEFINE (GENERATE-XENOID X)
  (IF (NOT *PRE-COOK?*)
      (BUG "you can't do (XENOID ~S) in a fasloadable file"
           "will pretend everything's fine"
           X))
  (LET ((XX (IF (STRING? X) `(VERBATIM ,X) X)))
    (LET ((XTAG (GENTAG 'XENOID)))
      (BEGIN-ALIGNED-IMPURE-DATUM)
      (EMITREMARK "Xenoid")
      (EMIT-TO-UNIT '(ADDRESS XENOID-TEMPLATE))
      (IF (OR (SYMBOL? X) (STRING? X))
          (EMIT `(EXTERNAL ,XX)))
      (EMITTAG XTAG)
      (EMIT-TO-UNIT `(ADDRESS ,XX))
      (TEXT-SECTION)
      (UNIT-ADDR-REF XTAG))))

(DEFINE-LAP-CONSTANT %%PROCEDURE?-BIT-POS   0)
(DEFINE-LAP-CONSTANT %%ENTITY?-BIT-POS      1)
(DEFINE-LAP-CONSTANT %%RETURN?-BIT-POS      2)
(DEFINE-LAP-CONSTANT %%FRAME?-BIT-POS       2)
(DEFINE-LAP-CONSTANT %%STANDARD-GC?-BIT-POS 3)
(DEFINE-LAP-CONSTANT %%DEFINER?-BIT-POS     4)
(DEFINE-LAP-CONSTANT %%LEXPR?-BIT-POS       5)

;;; BEGIN-CHUNK spews out the leader slots and bits for a code chunk.
;;; The definitive document on chunks is NO:CHUNK.TXT.  Maintain synchronicity!

;;; TTAGS is of the form (CTAG TTAG . PTAG)
;;; PROCEDURE-INFO is one of
;;;  null, if instances aren't callable and aren't return pts
;;;  (RETURN)
;;;  (EXPR . cruft)
;;;  (LEXPR . cruft)
;;; where cruft = (#args ptr-mem-sz scr-mem-sz)

(DEFINE (BEGIN-CHUNK TTAGS              ; (ctag tag . ptag)
                     SUPERIOR-LOC       ; superior template offset or nil
                     SIZE-INFO          ; (ptr . scr) or gc-method slot
                     PROCEDURE-INFO     ; ... see above, or nil
                     HANDLER-LOC        ; handler ptag or nil
                     DEFINEE-LOC)       ; definee qtag or nil
  (LET ((CTAG (CAR TTAGS))
        (TTAG (CADR TTAGS))
        (PROCEDURE?-FLAG (AND PROCEDURE-INFO
                              (NOT (EQ? (CAR PROCEDURE-INFO) 'RETURN))))
        (ENTITY?-FLAG HANDLER-LOC)
        (FRAME?-FLAG (AND PROCEDURE-INFO (EQ? (CAR PROCEDURE-INFO) 'RETURN)))
        (LEXPR?-FLAG (AND PROCEDURE-INFO (EQ? (CAR PROCEDURE-INFO) 'LEXPR)))
        (DEFINER?-FLAG DEFINEE-LOC)
        (STANDARD-GC?-FLAG (PAIR? SIZE-INFO)))
    (COND ((NOT *SUPPRESS-FLINK?*)
           ;; indentation is wrong... should fix someday...
    ;;; Beware pending remark...
;   (EMIT '(ALIGN 1))                   ; Vax only?
    ;; Definee: unit index for definee (# longwords from beg of unit) (?)
    (COND (DEFINER?-FLAG
           (EMITREMARK "Definee")
           (EMIT `(HALFPESO (/ (- ,DEFINEE-LOC ,*THE-UNIT-TAG*) 4)))))
    ;; Handler: # quadwords before template (usu. negative?)
    (COND (ENTITY?-FLAG
           (EMITREMARK "Handler")
           (EMIT `(HALFPESO (/ (+ (- ,TTAG ,HANDLER-LOC)
                                  ,(FX- TARGET:%%EXTEND-TAG TARGET:%%TEMPLATE-TAG))
                               8)))))
    ;; ptr-mem-sz, scr-mem-sz, #args-or-fill
    (COND (PROCEDURE?-FLAG
           (IF (NOT (FX= (LENGTH PROCEDURE-INFO) 4))
               (BUG "bad procedure info in BEGIN-CHUNK: ~S"
                    "will use it anyhow but the chunk will be pretty screwy"
                    PROCEDURE-INFO))
           (EMITREMARK "Ptr-mem, scr-mem, nargs")
           (EMIT `(BYTE ,@(CDDR PROCEDURE-INFO) ,(CADR PROCEDURE-INFO))))
          ((OR DEFINER?-FLAG ENTITY?-FLAG (NEQ? *TARGET-MACHINE* 'VAX)) ;bletch
           (EMITREMARK "Fill")
           (EMIT '(BYTE -1))))
    ;; Bits: PROCEDURE? ENTITY? FRAME?
    ;;     STANDARD-GC? DEFINITION? LEXPR?
    (EMITREMARK "Flags")
    ;; T sux - doesn't have multi-arg plus.
    (EMIT `(BYTE ,(FX+ (FX+ (FX+ (IF PROCEDURE?-FLAG   (LSH 1 TARGET:%%PROCEDURE?-BIT-POS) 0)
                                 (IF ENTITY?-FLAG      (LSH 1 TARGET:%%ENTITY?-BIT-POS)  0))
                        (FX+ (IF FRAME?-FLAG       (LSH 1 TARGET:%%FRAME?-BIT-POS)  0)
                             (IF STANDARD-GC?-FLAG (LSH 1 TARGET:%%STANDARD-GC?-BIT-POS) 0)))
                     (FX+ (IF DEFINER?-FLAG (LSH 1 TARGET:%%DEFINER?-BIT-POS)     0)
                          (IF LEXPR?-FLAG   (LSH 1 TARGET:%%LEXPR?-BIT-POS)       0)))))
    ;; Instance description: either ptr-sz/scr-sz or gc tproc's # qws bfor temp
    (COND ((OR (SYMBOL? SIZE-INFO)
               (AND (PAIR? SIZE-INFO)
                    (EQ? (CAR SIZE-INFO) 'LABEL)))
           (EMITREMARK "Nonstandard GC method")
           (EMIT `(HALFPESO (/ (+ (- ,TTAG ,SIZE-INFO) %%EXTEND-TAG) 8))))
          ((PAIR? SIZE-INFO)
           (EMITREMARK "Instance size (ptr,scratch)")
           (EMIT `(BYTE ,(CAR SIZE-INFO) ,(CDR SIZE-INFO))))
          (ELSE (BUG "yucky size-info in BEGIN-CHUNK: ~S" "foo" SIZE-INFO)))
    ;; Superior's template: # quadwords before template
    (COND (SUPERIOR-LOC
           (EMITREMARK "Superior")
           (EMIT `(HALFPESO (/ (- ,TTAG ,SUPERIOR-LOC) 8))))
          (ELSE (EMIT '(HALFPESO 0))))
    ;; Unit origin offset: # quadwords before template
    (EMITREMARK "Unit locator")
    (EMIT `(HALFPESO (/ (- ,TTAG ,*THE-UNIT-0-TAG*) 8)))
    )) ; close (cond ((not *suppress-flink*) ...))
    (EMITTAG CTAG)
    ))

;;; Make this agree with RELOC
;;;  UNIT-BEGIN-OFFSET - in file, distance between the code and the unit
;;;  SCRATCH-AREA-OFFSET - in unit, offset to scratch vector
;;;  STRUCTURE-AREA-OFFSET - in unit, offset for "structure area"
;;;  LINKAGE-AREA-OFFSET - in unit, offset of linkage area for tp-relative refs
;;;  What about string headers??

(COMPILATION-GLOBAL *THE-CODE-TAG*)
(COMPILATION-GLOBAL *THE-CODE-END-TAG*)
(COMPILATION-GLOBAL *THE-CODE-CODE-END-TAG*)
(COMPILATION-GLOBAL *THE-UNIT-TAG*)
(COMPILATION-GLOBAL *THE-UNIT-0-TAG*)
(COMPILATION-GLOBAL *THE-UNIT-END-TAG*)
(COMPILATION-GLOBAL *THE-ENTRY-TAG*)
(COMPILATION-GLOBAL *THE-LINKAGE-AREA*)
(COMPILATION-GLOBAL *THE-STRUCTURE-AREA*)
(COMPILATION-GLOBAL *THE-STRUCTURE-AREA-TAG*)
(COMPILATION-GLOBAL *THE-SCRATCH-AREA-TAG*)
(COMPILATION-GLOBAL *THE-STRING-AREA-TAG*)
(COMPILATION-GLOBAL *OFFSET*)
(COMPILATION-GLOBAL *THE-UNIT-DEFS*)
(COMPILATION-GLOBAL *THE-FLONUMS*)
(COMPILATION-GLOBAL *BUFFER-LINKAGE-AREA?*)
(COMPILATION-GLOBAL *MAKE-SYSTEM?*)     ; blug

;;; To make an incompatible change to the SLINK, fasl file format, calling
;;; sequence, or anything else which makes old fasl files not loadable into
;;; new T's, add 2 to the following magic numbers so that LOAD-RAW will verify
;;; compatibility.

;;; Two varieties of FASL to cope with the way that the losing UNIX assembler
;;; emits the second longword of a template.

(DEFINE-LAP-CONSTANT %%PRECOOKED      1011)
(DEFINE-LAP-CONSTANT %%INCORRECT-FASL 1012)
(DEFINE-LAP-CONSTANT %%CORRECT-FASL   1013)

;;; Called from file transducer at beginning of file compilation.
;;; H is the file's HERALD.

(DEFINE (BEGIN-ASSEMBLY-FILE H FNAME)
  (SET *BUFFER-LINKAGE-AREA?*
       (AND (NEQ? *ASSEMBLER-TYPE* 'VAX-UNIX)
            (NEQ? *ASSEMBLER-TYPE* 'PYRAMID-UNIX)))
  (LET* ((ID (FILENAME-NAME (->FILENAME (CADR H))))
         (FOO (IF (SYMBOL? ID) ID 'ANON)))
    (INITIALIZE-UNIT-TAGS FOO)
    ;; Next peso emitted = (XREF UNIT *OFFSET*)
    (SET *OFFSET* 0)

    (PUSH *UNDO-LIST*
          (LAMBDA ()
            (WALK (LAMBDA (X) (REMPROP X 'SYM))
                  *THE-UNIT-DEFS*)))

    (COND ((NOT *SUPPRESS-FLINK?*)
           (COND ((NOT *PRE-COOK?*)
                  (OUTPUT-SYSBUILD-ITEM '(END))))

           (BEGIN-ASSEMBLY-FILE-1 FOO)
           (BEGIN-TEXT-SECTION)
           ;; Set up impure area (unit) leader info.
           (DATA-SECTION)
           (WALKCDR PROCESS-LAP-ITEM
                    `((ALIGN 3)                 ;"Impure area begins here."
                      (GLOBL ,*THE-UNIT-0-TAG*)
                      ,*THE-UNIT-0-TAG*
                      ,(COND (*PRE-COOK?* '(ADDRESS UNIT-TEMPLATE))
                             (ELSE '(PESO 0)))  ;"Template for unit as extend"
                      ,*THE-UNIT-TAG*))
           
           ;; (ASSERT (= %%UNIT-CODE-OFFSET 0))
           (EMIT-TO-UNIT `(ADDRESS ,*THE-CODE-TAG*))    ; (0) code
           ;; (ASSERT (= %%UNIT-THING-OFFSET 4))
           (EMITREMARK "Distinguished object")
           (EMIT-VANILLA *THE-ENTRY-TAG*)               ; (1) thing
           (COND ((OR *MAKE-SYSTEM?* *DOING-SYSGEN?*)
                  (EMIT-VIRTUAL-POINTER NIL)            ; (2) source filename
                  (EMIT-VIRTUAL-POINTER NIL)            ; (3) id
                  (EMIT-VIRTUAL-POINTER NIL)            ; (4) environment info
                  (EMIT-VIRTUAL-POINTER NIL))           ; (5) herald clauses
                 (ELSE
                  (EMIT-TO-UNIT (EMIT-VIRTUAL-FILENAME FNAME T));(2) source filename
                  (EMIT-VIRTUAL-POINTER (CADR H))       ; (3) id
                  (EMIT-VIRTUAL-POINTER NIL)            ; (4) environment info
                  (EMIT-VIRTUAL-POINTER (CDDR H))))     ; (5) herald clauses
           (TEXT-SECTION)))))

(DEFINE (INITIALIZE-UNIT-TAGS FOO)
  (SET *THE-CODE-TAG* (CONCATENATE-SYMBOL 'Q- FOO))
  (SET *THE-UNIT-0-TAG* (CONCATENATE-SYMBOL 'D- FOO))
  (SET *THE-UNIT-TAG* (GENTAG 'UNIT))
  (SET *THE-ENTRY-TAG* (GENTAG 'ENTRY))
  (SET *THE-STRUCTURE-AREA-TAG* (GENTAG 'STR))
  (SET *THE-SCRATCH-AREA-TAG* (GENTAG 'SCR))
  (SET *THE-STRING-AREA-TAG* (GENTAG 'STRINGS))
  (SET *THE-UNIT-END-TAG* (GENTAG 'END))
  (SET *THE-CODE-CODE-END-TAG* (GENTAG 'END))  ; awful name
  (SET *THE-CODE-END-TAG* (GENTAG 'END)))

(DEFINE (BEGIN-TEXT-SECTION)
  (TEXT-SECTION)                        
  (WALKCDR PROCESS-LAP-ITEM
           `(;; Set up pure area (code-group) leader info.
             (ALIGN 3)          "Pure area begins here."
             ,*THE-CODE-TAG*
             ;; Make these fields agree with the accessor definitions
             ;;  given in COMMON.
             (PESO ,(LSH (COND (*PRE-COOK?* TARGET:%%PRECOOKED)
                               ((EQ? *ASSEMBLER-TYPE* 'VAX-VMS) 
                                TARGET:%%CORRECT-FASL)
                               (ELSE TARGET:%%INCORRECT-FASL))
                         *SHIFT*))
             (PESO (- ,*THE-CODE-END-TAG* ,*THE-CODE-TAG*))
             (PESO (* (- ,*THE-STRUCTURE-AREA-TAG* ,*THE-UNIT-TAG*) 2))
             (PESO (* (- ,*THE-STRING-AREA-TAG*    ,*THE-UNIT-TAG*) 2))
             (PESO (* (- ,*THE-SCRATCH-AREA-TAG*   ,*THE-UNIT-TAG*) 2))
             (PESO (* (- ,*THE-UNIT-END-TAG*       ,*THE-UNIT-TAG*) 2))
             (PESO (- ,*THE-CODE-CODE-END-TAG* ,*THE-CODE-TAG*))
             (PESO 0)                   ; For future expansion
             )))

(DEFINE (FINISH-ASSEMBLY-FILE)
  (COND ((NOT *SUPPRESS-FLINK?*)

         ;; No more code chunks past this point.
         (BEGIN-ALIGNED-PURE-DATUM)
         (EMITTAG *THE-CODE-CODE-END-TAG*)

         ;; Structure area.  Only list structure for now, maybe more crap later.
         (DATA-SECTION)
         (EMITTAG *THE-STRUCTURE-AREA-TAG*)
         (SET *THE-STRUCTURE-AREA* (REVERSE! *THE-STRUCTURE-AREA*))
         (DO ()                         ; Processing one item may cause others 
             ((NULL? *THE-STRUCTURE-AREA*))     ; to appear.
           ((POP *THE-STRUCTURE-AREA*)))
         (TEXT-SECTION)

         (FINISH-THE-VCELLS)            ; emits nothing

         (FINISH-THE-FLONUMS)           ; starts and ends aligned!
         (FINISH-THE-STRINGS)           ; ends up in data section
         (FINISH-THE-SYMBOLS)           ; ends up in data section

         (END-ASSEMBLY-FILE)
         )))

;;; Scan the value cells, augmenting the *THE-SYMBOLS* list via UNIT-INTERN.
;;; Also print useful info about free vs. bound variables and all that.

(DEFINE (FINISH-THE-VCELLS)
  (SET *THE-VCELLS* (REVERSE! *THE-VCELLS*))
  (LET ((DEFINES '())
        (LSETS '())
        (FREES '()))
    (DECLARE (SPECIAL DEFINES LSETS FREES))
    (WALK (LAMBDA (STUFF)
            (DECLARE (SPECIAL DEFINES LSETS FREES))
            ;; STUFF is (STATIC REFTYPE VTAG FOOTAG . SYMTAG)
            (LET ((ID (STATIC-IDENTIFIER (CAR STUFF))))
              (COND (*PRE-COOK?*
                     (OUTPUT-SYSBUILD-ITEM `(STATIC ,ID ,(CADR STUFF))))
                    (ELSE
                     (SET (CDDDDR STUFF)
                          (CADR (UNIT-INTERN ID)))))
              (CASE (CADR STUFF)
                ((DEF)  (PUSH DEFINES ID))
                ((LSET) (PUSH LSETS   ID))
                (ELSE   (PUSH FREES   ID)))))
          *THE-VCELLS*)
    (FORMAT *NOISE-MINUS-TERMINAL* "~2&~s~2&~s~2&~s~2&"
            (LIST 'FREE-VARIABLES    (REVERSE! FREES))
            (LIST 'DEFINED-VARIABLES (REVERSE! DEFINES))
            (LIST 'LSET-VARIABLES    (REVERSE! LSETS))))
  ;; Horrible hack to make pre-cooking win.
  (COND (*PRE-COOK?*
         (SET *THE-VCELLS* '()))))

;;; Emit literal flonums.  Assume prior alignment.

(DEFINE (FINISH-THE-FLONUMS)
  (COND ((NOT (NULL? *THE-FLONUMS*))
         (SET *THE-FLONUMS* (REVERSE! *THE-FLONUMS*))
         (WALK (LAMBDA (Z)
                 (EMITTAG (CAR Z))
                 (OUTPUT-FLONUM (CDR Z)))
               *THE-FLONUMS*)
         (SET *THE-FLONUMS* '()))))

;;; Start in text section, end in data section.

(DEFINE (FINISH-THE-STRINGS)
  (SET *THE-STRINGS* (REVERSE! *THE-STRINGS*))

  ;; Emit string texts.
  (WALK (LAMBDA (S)                     ; (STRING TAG . TEXTTAG)
          (LET ((TEXTTAG (GENTAG 'TXT)))
            (SET (CDDR S) TEXTTAG)
            (OUTPUT-STRING-TEXT TEXTTAG (CAR S))))
        *THE-STRINGS*)

  (SET *THE-SYMBOLS* (REVERSE! *THE-SYMBOLS*))

  (COND (*PRE-COOK?*
         (WALK (LAMBDA (STUFF)
                 (LET ((SYM (CAR STUFF)))
                   (OUTPUT-SYSBUILD-ITEM `(SYMBOL ,SYM))
                   (IF (NOT *DOING-SYSGEN?*)
                       (EMIT `(EXTERNAL ,(MAKE-SPECIAL-TAG 'S SYM))))))
               *THE-SYMBOLS*)
         (SET *THE-SYMBOLS* '()))
        (ELSE
         ;; Emit string texts for symbol pnames.
         (WALK (LAMBDA (STUFF)
                 ;; (SYMBOL TAG FOOTAG PNTAG . TEXTTAG)
                 (LET ((TEXTTAG (GENTAG 'TXT)))
                   (SET (CDDDDR STUFF) TEXTTAG)
                   (OUTPUT-STRING-TEXT TEXTTAG
                                       (SYMBOL-PNAME (CAR STUFF)))))
               *THE-SYMBOLS*)))

  ;; This is the very end of the text section.
  (EMIT '(ALIGN 3))
  (EMITTAG *THE-CODE-END-TAG*)

  ;; String header area.  First random strings, then pnames for symbols.
  (BEGIN-ALIGNED-IMPURE-DATUM)          ; ?
  (EMITTAG *THE-STRING-AREA-TAG*)

  ;; Emit string headers.
  (WALK (LAMBDA (S)                     ; (STRING TAG . TEXTTAG)
          (OUTPUT-STRING-HEADER (CADR S) (CDDR S) (CAR S)))
        *THE-STRINGS*)

  ;; Emit symbol pname headers.
  (WALK (LAMBDA (STUFF)                 ; (SYMBOL TAG FOOTAG PNTAG . TEXTTAG)
          (LET ((PNTAG (GENTAG 'PN)))
            (SET (CADDDR STUFF) PNTAG)
            (OUTPUT-STRING-HEADER PNTAG
                                  (CDDDDR STUFF)
                                  (SYMBOL-PNAME (CAR STUFF)))))
        *THE-SYMBOLS*))

;;; Start and end in data section.  Get alignment at end.

(DEFINE (FINISH-THE-SYMBOLS)
  ;; Scratch area.  Currently this consists of symbols only.
  (DATA-SECTION)
  (EMITTAG *THE-SCRATCH-AREA-TAG*)
  (WALK (LAMBDA (STUFF)
          ;; (SYMBOL TAG FOOTAG PNTAG . TEXTTAG)
          (EMITTAG (CADR STUFF))
          (EMIT-TO-UNIT
           `(PESO (+ (- %%REL-ITEM-TAG %%STRING-TAG)
                     (- ,(CADDDR STUFF) ,*THE-UNIT-0-TAG*)
                     (<< %%SYMBOL-REL-TYPE
                         %%REL-ITEM-TYPE-FIELD-POS))))
          (IF (CADDR STUFF)
              (EMIT `(DEF ,(CADDR STUFF)
                          (+ %%REL-ITEM-TAG
                             (* (- ,(CADR STUFF) ,*THE-UNIT-TAG*) 2)
                             (<< %%FETCH-REL-TYPE
                                 %%REL-ITEM-TYPE-FIELD-POS))))))
        *THE-SYMBOLS*)

  ;; Emit DEF pseudo-ops to fill in vcell pointer slots in linkage area.
  (WALK (LAMBDA (STUFF)
          ;; STUFF is (STATIC REFTYPE VTAG FOOTAG . SYMTAG)
          (LET ((FOOTAG (CADDDR STUFF))
                (SYMTAG (CDDDDR STUFF)))
            (EMIT `(DEF ,FOOTAG
                        (+ %%REL-ITEM-TAG
                           (* (- ,SYMTAG ,*THE-UNIT-TAG*) 2)
                           ;; Eventually move comma inside the LSH.
                           (<< ,(CASE (CADR STUFF)
                                  ((DEF)  TARGET:%%LOCAL-REL-TYPE)
                                  ((LSET) TARGET:%%LSET-REL-TYPE)
                                  ((SETQ) TARGET:%%SET-REL-TYPE)
                                  (ELSE TARGET:%%FREE-REL-TYPE))
                               %%REL-ITEM-TYPE-FIELD-POS)
                           )))))
        *THE-VCELLS*)
  (EMITTAG *THE-UNIT-END-TAG*)
  (BEGIN-ALIGNED-IMPURE-DATUM))


;;; SLINK stuff.  Winning or losing depending on your point of view.

(LSET *NULL-DEMON-TRIGGERED?* NIL)      ;Flag
(LSET *SLINK-OFFSET* 0)                 ;Current virtual offset

;;; At some point, provide an EXTEND pointer into the SLINK for use as
;;; a valid Scheme datum.

(DEFINE (GEN-SLINK SLOTSPECS)
  (BIND ((*SLINK-OFFSET* -124.)
         (*NULL-DEMON-TRIGGERED?* NIL)
         (*GENERATE-QUEUE* (MAKE-QUEUE))
         (*BUFFER-LINKAGE-AREA?* NIL))
    (EMITREMARK "Here follows the SLINK itself.")
    (BEGIN-ALIGNED-IMPURE-DATUM)
    (EMITTAG 'SLINK-BEGIN)
    (EMITREMARK "Byte-offset slots...")
    (WALK EMIT-SLINK-SLOT SLOTSPECS)
    (COND ((NOT *NULL-DEMON-TRIGGERED?*)
           ;; Need to spit out the null-pair at offset 0
           (IF (FX>= *SLINK-OFFSET* -4) (ERROR "Null demon failed to trigger!"))
           (EMIT `(SPACE ,(FX- -4 *SLINK-OFFSET*)))       ;.=.+n
           (SET *SLINK-OFFSET* -4)
           (TRIGGER-NULL-DEMON)))
    (EMITTAG 'SLINK-END)
    ;(LET ((FOO (/ (+ *SLINK-OFFSET* 124) 4)))
      ;(SET *OFFSET* (+ *OFFSET* FOO)))
    (EMIT '(ALIGN 3))
    (SET *OFFSET* -10001)
    (TEXT-SECTION)
    (EMPTY-GENERATE-QUEUE)
    0))                                 ; Value of CEVAL

(DEFINE (EMIT-SLINK-SLOT SLOTSPEC)
  (LET ((NAME (CAR SLOTSPEC))
        (EXPR (CADR SLOTSPEC)))
    (COND (NAME
           (LET ((O (GET NAME 'SLINK-OFFSET)))
             (IF (AND O (NOT (FX= O *SLINK-OFFSET*)))
                 (FORMAT *NOISE-OUTPUT*
                         "~%;Warning!  Changing SLINK offset ~S from ~S to ~S"
                         NAME O *SLINK-OFFSET*)))
           (FORMAT *SUPPORT-OUTPUT* "(PUT '~S 'SLINK-OFFSET ~S)~%"
                   NAME *SLINK-OFFSET*)
           (PUT NAME 'SLINK-OFFSET *SLINK-OFFSET*)))
    (EMIT EXPR)
    
    (LET ((S (GET (CAR EXPR) 'SIZE)))
      (SET *SLINK-OFFSET*
           (FX+ *SLINK-OFFSET*
                (COND (S S)
                      (ELSE
                       (WARN "slink slot with unknown assembly space directive: ~A"
                             "will allocate no space and lose"
                             (CAR EXPR))
                       0))))))

  ;; This test must be at end, not beginning
  (COND ((FX= *SLINK-OFFSET* -6)          ; To make sure 16 bit entries don't 
         (EMIT '(HALFPESO 0))           ; mess up triggering of the
         (SET *SLINK-OFFSET* -4)))     ; NULL-DEMON
  (IF (FX= *SLINK-OFFSET* -4) (TRIGGER-NULL-DEMON))
  (IF (FX= *SLINK-OFFSET* 128.) (EMITREMARK "Word-offset slots...")))

;;; Hack for getting null pair to appear at 0(slp)
;;; Null pair is preceded by a fake template to arrange for SLP to be a valid
;;; pointer - this should simplify things a little.

(DEFINE (TRIGGER-NULL-DEMON)
  (IF (NOT (FX= *SLINK-OFFSET* -4))
      (ERROR "Demon triggerred inappropriately!?"))
  (EMIT '(ADDRESS SLINK-TEMPLATE))
  (SET *SLINK-OFFSET* 0)

  (EMITTAG 'THE-SLINK)
  (EMIT '(GLOBL THE-SLINK))
  (EMIT-SLINK-SLOT '(NULL (ADDRESS NULL)))
  (EMIT-SLINK-SLOT '(() (ADDRESS NULL)))        ; Vax wants this... for MOVQ's
  (PRINT '(PUT 'FALSE 'SLINK-OFFSET 0) *SUPPORT-OUTPUT*)
  (PUT 'FALSE 'SLINK-OFFSET 0)
  (SET *NULL-DEMON-TRIGGERED?* T))
