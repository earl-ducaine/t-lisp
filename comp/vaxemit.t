(HERALD (TCOMP VAXEMIT T 147)
        (ENV TCOMP))

;;; Machine-specific support for EMIT-like things.
;;; This is the Vax/Unix version.

;;; Parameters for TRANSDUCE

(DEFINE *SCRATCH-REG-NAMES* '(R0 R1 R2 R3 R4))

(DEFINE *POINTER-REG-NAMES*
  (MAP (LAMBDA (Z)
         (PUT (CAR Z) 'SYM (CADR Z))
         (*DEFINE-LAP-CONSTANT (CONCATENATE-SYMBOL '%% (CAR Z)) (CADDR Z))
         (CADR Z))
       '((VAL R5 5)
         (XP  R6 6)
         (YP  R7 7)
         (ZP  R8 8)
         (FUN R9 9))))

(BLOCK0 'REGISTER-ALIASES
       (WALK (LAMBDA (X)
               (IF (NEQ? (CAR X) (CADR X))
                   (PUT (CAR X) 'SYM (CADR X)))
               (*DEFINE-LAP-CONSTANT (CONCATENATE-SYMBOL '%% (CAR X)) (CADDR X)))
             '((HP  R10 10)             ; Top-of-heap pointer
               (SLP R11 11)             ; SLINK pointer
               (AP  AP  12)             ; Argument pointer
               (TP  FP  13)             ; Template pointer
               (SP  SP  14)             ; Stack pointer
               (PC  PC  15)             ; Program counter
               ;; The following two are used only for indexing a fault-frame.
               (JF  JF  15)             ; Jump-from
               (SHP SHP 16)             ; Saved heap pointer
               (SAP SAP 17)             ; Saved heap pointer
               )))

(DEFINE *VAL* (GET 'VAL 'SYM))
(DEFINE *CALLEE-VAL* *VAL*)
(DEFINE *MY-VAL* *VAL*)

(DEFINE *FUN* (GET 'FUN 'SYM))
(DEFINE *CALLEE-FUN* *FUN*)
(DEFINE *MY-FUN* *FUN*)

(DEFINE *POSITION-INDEPENDENT-CODE?* NIL)       ; ?
(DEFINE *SEPARATE-ASSEMBLY?* T)


;;; Set this stuff up for NEXTLOC.
(LET ((FUN (LAMBDA (X Y) (PUT X 'NEXT-REGISTER Y))))
  (WALK FUN *SCRATCH-REG-NAMES* (CDR *SCRATCH-REG-NAMES*))
  (WALK FUN *POINTER-REG-NAMES* (CDR *POINTER-REG-NAMES*)))

(DEFINE *POINTER* 4)    ;Number of bytes per pointer
(DEFINE *SHIFT* 3)      ;Number of low 0 bits in fixna

;;; For cross-compilation, fixnum size may change.  Use generic arithmetic.
(DEFINE TARGET:BITS-PER-FIXNUM (FX- 32. *SHIFT*))
(DEFINE TARGET:MAX-FIXNUM (- (EXPT 2 TARGET:BITS-PER-FIXNUM) 1))
(DEFINE TARGET:MIN-FIXNUM (- -1 TARGET:MAX-FIXNUM))

(LET ((U? (CASE *TARGET-SYSTEM* ((UNIX) T) ((VMS) NIL))))

  (DEFINE *LOWERCASIFY-SYLLABLES?* U?)
  (DEFINE *IMMEDIATE-CHARACTER*     (IF U? #\$ #\#))
  (DEFINE *INDIRECTION-CHARACTER*   (IF U? #\* #\@))
  (DEFINE *COMMENT-START-CHARACTER* (IF U? #\# #\;)))

(DEFINE *STRING-CHARACTER*          #\")
;; Non alphanumerics that EMIT-SYLLABLE will not numberify
(DEFINE *OK-ASSEMBLY-SPECIAL-CHARACTERS* '(#\$ #\_ #\.))

;;; This info is used by EMITJ only... on VMS, we lose big...

(BLOCK0 'REVERSED-JOP
       (WALK (LAMBDA (X)
               (PUT (CAR X) 'REVERSED-JOP (CADR X))
               (PUT (CADR X) 'REVERSED-JOP (CAR X))
               (PUT (CADDR X) 'REVERSED-JOP (CADDDR X))
               (PUT (CADDDR X) 'REVERSED-JOP (CADDR X))
               (PUT (CAR X) 'LOSE (CADDDR X))
               (PUT (CADR X) 'LOSE (CADDR X)))
             '((JEQL JNEQ BEQL BNEQ)
               (JLSS JGEQ BLSS BGEQ)
               (JLEQ JGTR BLEQ BGTR)
               (JEQLU JNEQU BEQLU BNEQU)
               (JLSSU JGEQU BLSSU BGEQU)
               (JLEQU JGTRU BLEQU BGTRU))))

;;; When this is called, there is already an appropriate remark pending.
;;; TTAGS is a list of the form created by SETUP-LAMBDA-TEMPLATE:
;;;     (ctag ttag . ptag)

(DEFINE-LAP-CONSTANT %%JUMP-OPCODE #x9f17)

(STAT-COUNTER *TEMPLATE-COUNT* "number of templates/code chunks emitted")

(DEFINE (EMIT-TEMPLATE TTAGS)
  (DECLARE (SPECIAL *THE-CODE-TAG*))
  (COND ((NOT *SUPPRESS-FLINK?*)
         (DESTRUCTURE (((CTAG TTAG . PTAG) TTAGS))
           (BEGIN-ALIGNED-IMPURE-DATUM)
           (EMIT '(BYTE %%REL-ITEM-TAG))
           (EMITTAG TTAG)
           (COND ((AND *ENABLE-LAP-COMMENTARY?*
                       (EQ? *ASSEMBLER-TYPE* 'VAX-UNIX))
                  ;; Globalize tags for better typeout in "adb" debugger.
                  (EMIT `(GLOBL ,CTAG))
                  (EMIT `(GLOBL ,TTAG))))
           (COND (*PRE-COOK?*
                  (EMIT '(WORD %%JUMP-OPCODE))    ; jump absolute
                  (EMIT-CODE-ADDRESS CTAG)
                  (EMIT '(BYTE 0)))
                 (ELSE
                  (EMIT '(WORD %%JUMP-OPCODE))
                  (EMIT `(BYTE ,(LSH TARGET:%%TEMPLATE-REL-TYPE
                                     (FX- TARGET:%%REL-ITEM-TYPE-FIELD-POS 24.))))
                  (EMIT-CODE-ADDRESS CTAG)))
           (SET *OFFSET* (FX+ *OFFSET* 2))
           (COND (PTAG (EMITREMARK "Tproc")
                       (EMIT-VANILLA TTAG)
                       (EMITTAG PTAG)))
           (TEXT-SECTION)
           (INCR *TEMPLATE-COUNT*)
           ))))

(DEFINE (EMIT-CODE-ADDRESS TAG)
  (COND (*LISP-ASSEMBLY-SYNTAX?*
         (EMIT `(CODE-ADDRESS ,TAG)))
        ((OR *PRE-COOK?*
             (EQ? *ASSEMBLER-TYPE* 'VAX-UNIX))  ; Unix assembler can't cope.
         (EMIT `(ADDRESS ,TAG)))
        (ELSE
         (EMIT `(LONG (- ,TAG ,*THE-CODE-TAG*))))))

;;; See RELOC
(DEFINE-LAP-CONSTANT %%TEMPLATE-LOW-PESO (FX+ #x+9f1700 TARGET:%%REL-ITEM-TAG))

;;; This is for data which wants to be aligned and pure but not position-
;;; independent.  Thus this can be .text on the Vax but must be data on
;;; the 68K.  Think of a better name for this than FOO!

(DEFINE (BEGIN-ALIGNED-FOO-DATUM)
  (BEGIN-ALIGNED-PURE-DATUM))

(DEFINE (EMIT-OPCODE OP)
  (SET-HPOS *ASSEMBLY-OUTPUT* (OR *TAB-STOP-INTERVAL* 1))
  (EMIT-SYLLABLE (OR (GET OP 'OP-ALIAS) OP))    ;Fix this for pesudo-ops later
  (WRITEC *ASSEMBLY-OUTPUT* #\SPACE))

(DEFINE (EMIT-OPERAND X)
  (COND ((STRING? X) (WRITES *ASSEMBLY-OUTPUT* X))
        ((SYMBOL? X)
         (LET ((PROBE (GET X 'SYM)))
           (IF PROBE (EMIT-OPERAND PROBE) (EMIT-SYLLABLE X))))
        ((NUMBER? X) (EMIT-OFFSET X NIL))       ;For weird pseudo-ops
        ((ATOM? X)
         (BUGLET ((*OPERAND* X))
                 "~S weird operand" "will ignore it" X))
        (ELSE (LET ((X (EXPAND-OPERAND X)))
             (CASE (CAR X)
               ((REG -REG REG+)
                (IF (AND (CADDR X) (NOT (ALIKEV? (CADDR X) 0))) ; EQUIV?
                    (EMIT-OFFSET (CADDR X) NIL))
                (IF (EQ? (CAR X) '-REG) (WRITEC *ASSEMBLY-OUTPUT* #\-))
                (EMIT-REG-NAME-WITH-PARENS (CADR X))
                (IF (EQ? (CAR X) 'REG+) (WRITEC *ASSEMBLY-OUTPUT* #\+)))
               ((REL)                   ;PC-relative, indirectable
                (EMIT-OFFSET (CADR X) NIL))
               ((DATUM)
                (COND (*PRE-COOK?* (EMIT-OFFSET (CADR X) NIL))
                      (ELSE (BUG "can't have a DATUM operand here: ~S"
                              "will fail to put out any operand"
                              X))))
               ((LIT)
                (WRITEC *ASSEMBLY-OUTPUT* *IMMEDIATE-CHARACTER*)
                (EMIT-OFFSET (CADR X) NIL))
               ((@)
                (WRITEC *ASSEMBLY-OUTPUT* *INDIRECTION-CHARACTER*)
                (EMIT-OPERAND (CADR X)))
               ((IDX)
                (EMIT-OPERAND (CADDR X))
                (WRITEC *ASSEMBLY-OUTPUT* #\[)
                (EMIT-OPERAND (CADR X))
                (WRITEC *ASSEMBLY-OUTPUT* #\]))
               ((ABS)
                (WRITEC *ASSEMBLY-OUTPUT* *INDIRECTION-CHARACTER*)
                (WRITEC *ASSEMBLY-OUTPUT* *IMMEDIATE-CHARACTER*)
                (EMIT-OPERAND (CADR X)))
               ((ADDR) (EMIT-OPERAND (ADDR-LOC (CADR X))))
               ((UADDR)
                (COND (*PRE-COOK?* (EMIT-OPERAND `(LIT ,(CADR X))))
                      (ELSE (BUG "what's this ~S doing here?"
                              "will fail to emit any operand"
                              X))))
               (ELSE (EMIT-OFFSET X NIL))  ;Allow (+ ...) to fall through
               )))))

;;; Is this adequate for the 68000?  Do we ever want to generate an
;;; out-of-line literal?  ... well, let PRODUCE and friends figure that out.

(DEFINE (ADDR-LOC THING)
  (COND ((OR (ATOM? THING)
             (NOT (MEMQ (CAR THING) '(REL DATUM))))
         (BUGLET ((*OPERAND* THING))
                 "cannot address ~S's address"
                 "use 0 instead"
                 THING)
         '(LIT 0))
        (ELSE `(LIT ,(CADR THING)))))

;;; Write a flonum.

(DEFINE (OUTPUT-FLONUM N)
  (EMIT `(\.DOUBLE ,N)))

(DEFINE (OUTPUT-STRING-TEXT TAG S)
  (LET ((L (FX+ (STRING-LENGTH S) 1)))
    (TEXT-SECTION)
    (EMIT '(ALIGN 1))
    (EMIT `(HALFPESO ,L))
    (EMITTAG TAG)
    (COND (*LISP-ASSEMBLY-SYNTAX?*
           (EMIT `(ASCIZ ,S)))
          (ELSE
           (OUTPUT-ASCIZ S)))
    ;; Avoid weird GC screw.  This is really a hack.  I bet you don't
    ;; understand it.
    (COND ((AND (NOT *PRE-COOK?*)
                (FX< L 5))
           (EMIT `(SPACE ,(FX- 6 L)))))))
