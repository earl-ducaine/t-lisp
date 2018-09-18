(HERALD M68EMIT
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University


;;;; Machine-specific support for code emitters
;;; This is the 68000/Apollo version.


;;; Parameters for TARGETIZE and PACK-TNS

(DEFINE *SCRATCH-REG-NAMES* '(D0 D1 D2 D3 D4 D5))
(DEFINE *POINTER-REG-NAMES* '(VAL XP FUN));Poor old YP. His day will come again.

;;; Hackery for code gen routines that need to know address for data reg

(DEFINE *ADDRESS-REGISTERS* '(VAL XP YP FUN TP AP HP SLP SP))
(DEFINE *DATA-REGISTERS* '(D0 D1 D2 D3 D4 D5 D6 D7))

;;; In Apollo T,  A0, A1,A2,A3;  A4,A5,A6, A7
;;;           are VAL,XP,YP,FUN; TP,HP,SLP,SP respectively.
;;;
;;; This correspondence is determined by equates that appear at the head 
;;; of each assembly file.
;;;
;;; On the Apollo,                  A5,A6, A7
;;;           are                   DB,SB, SP respectively.
;;; 
;;; SB in the frame pointer
;;; DB is the data block pointer (base reg for data area)
;;; but those don't concern us much, except in the kernel.

(PROG1 'REGISTER-ALIASES
       (WALK (LAMBDA (X)
               (*DEFINE-LAP-CONSTANT (CONCATENATE-SYMBOL '%% (CAR X))
                                     (CADR X)))
               ;;Register numbers
             '((D0   0)
               (D1   1)
               (D2   2)
               (D3   3)
               (D4   4)
               (D5   5)
               (D6   6)
               (D7   7)
               (VAL  8)
               (XP   9)
               (YP   10)
               (FUN  10)
               (AP   11)
               (TP   12)
               (HP   13)
               (SLP  14)
               (SP   15)
               ;;Longword offsets into non-apollo part of the fault frame
               (SHP  3)
               (SAP  4)
               (JF   5)
               ;;Byte offsets in the not-apollo part of the fault frame
               (BV-LENGTH      0)
               (BV-TEMPLATE    4)
               (BV-DFD0-INDEX  8)
               (MACHINE-FAULT? 10)
               (OLD-SP         24)
               )))

;%%FAULT-LEADER-SIZE 26
;%%DIAG-DFD0-OFFSET 6

(DEFINE *VAL* 'VAL)
(DEFINE *CALLEE-VAL* *VAL*)
(DEFINE *MY-VAL* *VAL*)

(DEFINE *FUN* 'FUN)
(DEFINE *CALLEE-FUN* *FUN*)
(DEFINE *MY-FUN* *FUN*)


;;; This wonderful horror is the first (low) 4 bytes of a template after it has
;;; been relocated by RELOC.  This value can be recognized by the GC if we
;;; restrict data with type tag = 1 (now flonums) from occurring in the highest
;;; 64K of the 32 bit address space.  The low 16 bits form a jump absolute
;;; opcode.  The top 3 bits are now 0 so that whoever is asserting that
;;; this is a fixnum won't crap out.  This problem will go away when we have
;;; 32 bit bignums

(DEFINE-LAP-CONSTANT %%TEMPLATE-LOW-PESO #X+1FFF4EF9)

;;; Parameters for EMIT and KERNEL

(DEFINE *POINTER* 4)    ;Number of bytes per pointer
(DEFINE *SHIFT* 3)      ;Number of low 0 bits in fixna

;;; For cross-compilation, fixnum size may change.  Use generic arithmetic.

(DEFINE TARGET:BITS-PER-FIXNUM (FX- 32. *SHIFT*))
(DEFINE TARGET:MAX-FIXNUM *MAX-FIXNUM*)
(DEFINE TARGET:MIN-FIXNUM *MIN-FIXNUM*)

;;; Parameters for the assembler

(DEFINE *LOWERCASIFY-SYLLABLES?* NIL)
(DEFINE *IMMEDIATE-CHARACTER*     #\#)
(DEFINE *INDIRECTION-CHARACTER*   '**YOU-ARE-LOSING-INDIRECTLY**)
(DEFINE *STRING-CHARACTER*        #\')
(DEFINE *OPEN-SUBEXPRESSION*      #\LEFT-PAREN)
(DEFINE *CLOSE-SUBEXPRESSION*     #\RIGHT-PAREN)
(DEFINE *COMMENT-START-CHARACTER* #\*)
;; Non alphanumerics that EMIT-SYLLABLE will not numberify
(DEFINE *OK-ASSEMBLY-SPECIAL-CHARACTERS* '(#\$ #\_))

;; This info is used by EMITJ only

(PROG1 'REVERSED-JOP
       (WALK (LAMBDA (X)
               (PUT (CAR X) 'REVERSED-JOP (CADR X))
               (PUT (CADR X) 'REVERSED-JOP (CAR X)))
             '((BEQ BNE)
               (BLT BGE)                ; 2's complement comparisons
               (BLE BGT)                ; - ditto -
               (BLS BHI)                ; unsigned comparisons
               (BHS BLO))))             ; unsigned comparisons

;; This info is used by EMITJ only

(PROG1 'REVERSED-COP
       (WALK (LAMBDA (X)
               (PUT (CAR X) 'REVERSED-COP (CADR X))
               (PUT (CADR X) 'REVERSED-COP (CAR X)))
             '((BEQ BEQ)                ; 2's complement comparisons
               (BNE BNE)                ; - ditto -
               (BGT BLT)                ; - ditto -
               (BLE BGE)                ; - ditto -
               (BHI BLO)                ; unsigned comparisons
               (BLS BHS))))             ; unsigned comparisons


;; General OP-ALIASes

(WALK (LAMBDA (X)
        (DESTRUCTURE (((OP V) X))
          (PUT OP 'OP-ALIAS V)))
      '((BYTE     DC.B)
        (HALFPESO DC.W)
        (PESO     DC.L)
        (SPACE    DS.B)
        (WSPACE   DS.W)
        (LSPACE   DS.L)
        (TEXT     PROC)
        (ADDRESS  AC)
        (BHS      BCC)
        (BHS.S    BCC.S)
        (BLO      BCS)
        (BLO.S    BCS.S)
        ))

;;; Called from file transducer at beginning of file compilation. 

(DEFINE (BEGIN-ASSEMBLY-FILE-1 ID)
  ;; Put DEFs into assembly file for readablility
  (EMIT `(MODULE ,ID))
  (COND ((NEQ? ID 'M68KERNEL)   ; FOO
         (EMIT '(EXTERNAL NULL))
         (EMIT '(EXTERNAL UNIT-TEMPLATE))
         (EMIT '(EXTERNAL VECTOR-TEMPLATE))))
  (WALK (LAMBDA (X) (EMIT `(DEF ,(CAR X) ,(CADR X))))
        '((VAL A0)
          (XP  A1)             
          (YP  A2)
          (FUN A2)
          (AP  A3)
          (TP  A4)
          (HP  A5)
          (SLP A6))))

(DEFINE (END-ASSEMBLY-FILE) T)          ; No END required

(DEFINE (MAKE-SPECIAL-TAG PREFIX SYMBOL)
  (CONCATENATE-SYMBOL PREFIX '$ SYMBOL))


;;; When this is called, there is already an appropriate remark pending.
;;; TTAGS is a list of the form created by SETUP-LAMBDA-TEMPLATE:
;;;     (ctag ttag . ptag)

(DEFINE-LAP-CONSTANT %%JUMP-OPCODE #x+4EF9)

(DEFINE (EMIT-TEMPLATE TTAGS)
  (COND ((NOT *SUPPRESS-FLINK?*)
         (PUSH *THE-LINKAGE-AREA*
               (LAMBDA ()
                 (DESTRUCTURE (((CTAG TTAG . PTAG) TTAGS))
                   (BEGIN-ALIGNED-IMPURE-DATUM)
                   (COND (*PRE-COOK?*
                          (EMIT '(DC.W #x+1FFF))
                          (EMITTAG TTAG)
                          (EMIT '(DC.W %%JUMP-OPCODE))
                          (EMIT-CODE-ADDRESS CTAG))
                         (ELSE
                          (EMIT '(DC.W (<< %%TEMPLATE-REL-TYPE
                                           (- %%REL-ITEM-TYPE-FIELD-POS
                                              16.))))
                          (EMITTAG TTAG)
                          (EMIT '(DC.W %%REL-ITEM-TAG))
                          (EMIT-CODE-ADDRESS CTAG)))
                   (SETQ *OFFSET* (FX+ *OFFSET* 2))
                   (COND (PTAG (EMITREMARK "Tproc")
                               (EMIT-VANILLA TTAG)
                               (EMITTAG PTAG)))
                   ))))))

;;; The following OUGHT to depend on *PRE-COOK?*, and if it's nil, should
;;; emit `(DC.L (- ,CTAG ,*THE-CODE-BEGIN-TAG*)).  RELOC should be hacked
;;; accordingly.
;;; Compare with code in VAXEMIT.

(DEFINE (EMIT-CODE-ADDRESS TAG)
  (EMIT `(AC ,TAG)))

;;; This is for data which wants to be aligned and pure but not position-
;;; independent.  Thus this can be .text on the Vax but must be data on
;;; the 68K.  Think of a better name for this than FOO!

(DEFINE (BEGIN-ALIGNED-FOO-DATUM)
  (BEGIN-ALIGNED-IMPURE-DATUM))

(DEFINE (EMIT-OPCODE OP)
  (SET-HPOS *ASSEMBLY-OUTPUT* 1)
  (DISPLAY (OR (GET OP 'OP-ALIAS) OP) *ASSEMBLY-OUTPUT*)
  (WRITEC *ASSEMBLY-OUTPUT* #\SPACE))

(DEFINE (EMIT-OPERAND X)
  (COND ((SYMBOL? X)
         (LET ((PROBE (GET X 'SYM)))
           (IF PROBE (EMIT-OPERAND PROBE) (EMIT-SYLLABLE X))))
        ((NUMBER? X) (EMIT-OFFSET X NIL))       ;For weird pseudo-ops
        ((ATOM? X) (BUG "~S weird operand" "will ignore it" X))
        (ELSE
         (LET ((X (EXPAND-OPERAND X)))
           (CASE (CAR X)
             ((REG -REG REG+)
              (IF (AND (CADDR X) (NOT (ALIKEV? (CADDR X) 0)))
                  (EMIT-OFFSET (CADDR X) NIL))
              (IF (EQ? (CAR X) '-REG) (WRITEC *ASSEMBLY-OUTPUT* #\-))
              (EMIT-REG-NAME-WITH-PARENS (CADR X))
              (IF (EQ? (CAR X) 'REG+) (WRITEC *ASSEMBLY-OUTPUT* #\+)))
             ((REL)                   ;PC-relative
              (EMIT-OFFSET (CADR X) NIL))
             ((LIT)
              (WRITEC *ASSEMBLY-OUTPUT* *IMMEDIATE-CHARACTER*)
              (EMIT-OFFSET (CADR X) NIL))
             ((VERBATIM) (DISPLAY (CADR X) *ASSEMBLY-OUTPUT*))
             ((IDX IDX.W IDX.L)       ;(IDX INDEX BASE OFFSET), e.g.
              (EMIT-OFFSET (CADDDR X) NIL)
              (WRITEC *ASSEMBLY-OUTPUT* #\LEFT-PAREN)
              (EMIT-OPERAND (CADDR X))        ; The "base" reg
              (WRITEC *ASSEMBLY-OUTPUT* #\,)
              (EMIT-OPERAND (CADR X)) ; The index reg
              (WRITES *ASSEMBLY-OUTPUT*
                      (IF (EQ? (CAR X) 'IDX.W) ".W)" ".L)")))
             (ELSE (EMIT-OFFSET X NIL))  ;Allow (+ ...) to fall through
             )))))

(DEFINE (EMIT-OFFSET X PARENTHESIZE?)
  ;; X is an assembler expression, like (+ FOO 2) or whatever
  ;; We assume that most assemblers accept expressions like FOO+(BAR*8).
  (COND ((NULL? X)
         (IF PARENTHESIZE? (BUG "expression is () in EMIT-OFFSET" "none")))
        ((INTEGER? X)
         (COND ((< X 10.)
                (PRIN1 X *ASSEMBLY-OUTPUT*))
               (ELSE
                (WRITEC *ASSEMBLY-OUTPUT* #\$)
                (RADIX-16-PRINT X *ASSEMBLY-OUTPUT*))))
        ((STRING? X) (WRITES *ASSEMBLY-OUTPUT* X))
        ((SYMBOL? X)
         (COND ((GET X 'SYM) (EMIT-OFFSET (GET X 'SYM) PARENTHESIZE?))
               (ELSE (EMIT-SYLLABLE X))))
        ((CHAR? X)
         (PRIN1 (CHAR->ASCII X) *ASSEMBLY-OUTPUT*))
        ((NOT (PAIR? X))
         (BUG "illegal object ~S in EMIT-OFFSET" "will ignore it" X))
        (ELSE
         (XCASE (CAR X)
                ((~)
                 (WRITEC *ASSEMBLY-OUTPUT* #\-)
                 (EMIT-OFFSET (CADR X) T))
                ((+ - * / << >>)
                 (COND ((NULL? (CDDR X))
                        (EMIT-OFFSET (CADR X) NIL))
                       (ELSE
                        (IF PARENTHESIZE?
                            (WRITEC *ASSEMBLY-OUTPUT* #\LEFT-PAREN))
                        (EMIT-OFFSET (CADR X) T)
                        (DO ((Z (CDDR X) (CDR Z)))
                            ((NULL? Z))
                          (DISPLAY (CAR X) *ASSEMBLY-OUTPUT*)
                          (EMIT-OFFSET (CAR Z) T))
                        (IF PARENTHESIZE?
                            (WRITEC *ASSEMBLY-OUTPUT* #\RIGHT-PAREN)))))
                ((LABEL) (OUTPUT-TAG (CDR X)))
                ((COMMA)
                 (EMIT-OFFSET (EVAL (CADR X) *TC-ENV*) PARENTHESIZE?))
                ((VERBATIM)
                 (DISPLAY (CADR X) *ASSEMBLY-OUTPUT*))
                ))))

(DEFUN (ADDR OPERAND-MACRO) (X)
  (ADDR-LOC (CADR X)))

(DEFINE (ADDR-LOC THING)
  (COND ((OR (ATOM? THING)
             (NOT (MEMQ (CAR THING) '(UREF DATUM REL))))
         (BUGLET ((*OPERAND* THING))
                 "cannot address ~S's address"
                 "use 0 instead"
                 THING)
         '(LIT 0))
        (ELSE
         (LET ((TAG (GENTAG 'ADDR)))
           (ENQUEUE *GENERATE-QUEUE*
                    (LAMBDA ()
                      (DATA-SECTION)
                      (EMITTAG TAG)
                      (EMITREMARK "ADDR operand")
                      (EMIT-VANILLA (CADR THING))
                      (TEXT-SECTION)))
           (IF (EQ? (CAR THING) 'UREF)
               `(UREF ,TAG ,(CADDR THING))
             `(DATUM ,TAG))))))

(DEFUN (DATUM OPERAND-MACRO) (X)
  (LET ((SYMBOL (CADR X)))
    `(UREF ,SYMBOL ,*TP-LOC*)))

(DEFUN (VCON OPERAND-MACRO)  (X)
  (LET* ((SYSCALL (CADR X))
         (SYSCALL-TAG (GENTAG (IF (PAIR? SYSCALL) (CADR SYSCALL) SYSCALL))))
    (DATA-SECTION)
    (EMITTAG SYSCALL-TAG)
    (EMITREMARK "VCON")
    (EMIT-TO-UNIT `(ADDRESS ,SYSCALL))
    (TEXT-SECTION)
    (UNIT-REF SYSCALL-TAG)))

;;; Arg must be a symbol.
;;; We assume that $, _, are valid alphabetic characters

(DEFINE (EMIT-SYLLABLE R)
  (COND ((EQ? R '*) (DISPLAY R *ASSEMBLY-OUTPUT*))
        (ELSE
         (WALK-STRING (LAMBDA (C)
                        (COND ((UPPERCASE? C)
                               (WRITEC *ASSEMBLY-OUTPUT*
                                       (IF *LOWERCASIFY-SYLLABLES?*
                                           (CHAR-DOWNCASE C)
                                         C)))
                              ((OR (DIGIT? C 10)
                                   (MEMQ C *OK-ASSEMBLY-SPECIAL-CHARACTERS*))
                               (WRITEC *ASSEMBLY-OUTPUT* C))
                              ((CHAR= C #\-)
                               (WRITEC *ASSEMBLY-OUTPUT* #\_))
                              (ELSE
                               (FORMAT *ASSEMBLY-OUTPUT* "~S$"
                                       (CHAR->ASCII C)))))
                      (SYMBOL-PNAME R)))))

(DEFINE (OUTPUT-ASCIZ S)
  (EMIT-OPCODE 'DA.B)
  (WRITEC *ASSEMBLY-OUTPUT* *STRING-CHARACTER*)
  (WALK-STRING (LAMBDA (C)
                 (DECLARE (FIXNUM C))
                 (COND ((CHAR= C #\RETURN) (WRITEC *ASSEMBLY-OUTPUT* #\SPACE))
                       ((OR (CHAR< C #\SPACE) (CHAR= C #\RUBOUT))
                        (FORMAT *ASSEMBLY-OUTPUT*
                                "~C~%  DC.B ~D~%  DA.B ~C"
                                *STRING-CHARACTER*
                                (CHAR->ASCII C)
                                *STRING-CHARACTER*))
                       ((CHAR= C *STRING-CHARACTER*)
                        (WRITEC *ASSEMBLY-OUTPUT* C)  ; ?
                        (WRITEC *ASSEMBLY-OUTPUT* C))
                       (ELSE
                        (WRITEC *ASSEMBLY-OUTPUT* C)))
                 T)
               S)
  (WRITEC *ASSEMBLY-OUTPUT* *STRING-CHARACTER*)
  (EMIT '(BYTE 0))                      ; ASCIZ like it says
  (EMIT '(WSPACE 0))  ; force word alignment for instructions
  )

;;; Whomever calls this must make sure that it goes in the right section
;;; (probably DATA), and that the section gets restored if necessary.

(DEFINE (OUTPUT-STRING-HEADER TAG TEXTTAG STRING)
  (IF (NOT (FX= TARGET:%%STRING-TAG 6))
      (BUG "something's wrong with %%STRING-TAG" "none"))
  (IF (NEQ? *ASSEMBLY-SECTION* 'DATA)
      (BUG "in wrong section for string header" "none"))
  (EMITREMARK "String")
  (EMIT `(HALFPESO ,(STRING-LENGTH STRING)))    ;Length
  (EMIT `(ADDRESS ,TEXTTAG))            ;Text pointer
  (EMITTAG TAG)
  (EMIT '(HALFPESO 0))          ;Index (base)
  (SETQ *OFFSET* (FX+ *OFFSET* 2)))

;;; Note that the text pointer points to the first character in the string
;;; and that this character is quadword aligned. (? - may want to give it
;;; type TEMPLATE or something...)  The length of the 
;;; real stored string preceeds the string and is only word aligned

(DEFINE (OUTPUT-STRING-TEXT TAG S)
  (TEXT-SECTION)                        ; Pure sybol names to share (e.g.)
  (EMIT '(ALIGN 3))                     ; Align to quadword
  ;; Add 3, 1 for the null byte, 2 for the text-length
  ;; then subtract out the text length
  (EMIT `(HALFPESO ,(FX- (CEILING (FX+ (STRING-LENGTH S) 3) 8) 2)))
  (EMITTAG TAG)
  (COND (*LISP-ASSEMBLY-SYNTAX?*
         (EMIT `(ASCIZ ,S)))
        (ELSE
         (OUTPUT-ASCIZ S))))

;;; Convert PDP-10 flonum to IEEE flonum by shuffling bits around.
;;;
;;;                    sign      exponent   MSB       fraction
;;; PDP-10 flonum      <35,1>    <27,8>     <26,1>    <6,20>+< 0,6>
;;; IEEE flonum        <31,1>    <20,11>    --        <0,20>+<26,6>

(DEFINE (OUTPUT-FLONUM X)
  (COND (*LISP-ASSEMBLY-SYNTAX?*
         (EMIT `(FLONUM ,X)))
        ((FL= X 0.0)
         (EMIT '(PESO 0))
         (EMITREMARK "Flonum 0.0")
         (EMIT '(PESO 0)))
        (ELSE
         (WALK (LAMBDA (W) (EMIT `(DC.W ,W)))
               (FLONUM-GUTS X LIST)))))

;;; LAP-MAGIC's.

;;; Label and EQU pseudo MUST be on the same line for Apollo assembler.

(DEFUN (DEF LAP-MAGIC) (X)
  (DESTRUCTURE (((() SYM VAL) X))
    (EMITTAG SYM)
    (WRITEC *ASSEMBLY-OUTPUT* #\SPACE)
    (EMIT-SYLLABLE 'EQU)
    (WRITEC *ASSEMBLY-OUTPUT* #\SPACE)
    (EMIT-OFFSET VAL NIL)))

(DEFUN (INCLUDE LAP-MAGIC) (X)          ; need to flushtag or flushcomment?
  (LET ((FILE (CADR X)))
    (FORMAT *ASSEMBLY-OUTPUT* "~&%INCLUDE '~A'" FILE)))

;; (EXTERNAL EXTERN.P)  was OP alias but have to flushtag
(DEFUN (EXTERNAL LAP-MAGIC) (X)
  (FLUSHTAG)
  (EMIT `(EXTERN.P ,(CADR X))))

;; (GLOBL    ENTRY.P)  was OP alias but have to flushtag
(DEFUN (GLOBL LAP-MAGIC) (X)
  (FLUSHTAG)
  (EMIT `(ENTRY.P ,(CADR X))))

(DEFUN (BPT LAP-MAGIC) (X)
  (IGNORE X)
  (EMIT '(TRAP (LIT 9))))

(DEFUN (ALIGN LAP-MAGIC) (X)
  (DESTRUCTURE (( (() N OFFSET)  X))
    (COND ((FX= N 1) (EMIT `(DS.W 0)))
          (ELSE (LET ((M (FIXNUM-ASHL 1 N)))
                  (EMIT-OPCODE 'DS.B)
                  (IF (OR (EQ? OFFSET NIL) (ALIKEV? OFFSET 0))
                      (EMIT-OFFSET `(* -1
                                       (- *
                                          (* ,M
                                             (/ (+ * ,(FX- M 1))
                                                ,M))))
                               NIL)
                      (EMIT-OFFSET `(* -1
                                       (- (+ * ,OFFSET)
                                          (* ,M
                                             (/ (+ (+ * ,OFFSET) ,(FX- M 1))
                                                ,M))))
                                   NIL)))))))
