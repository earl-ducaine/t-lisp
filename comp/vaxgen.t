(HERALD (TCOMP VAXGEN T 322)
        (ENV TCOMP))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Code generation for the VAX

;;; Code generation utilities

;;; Some day we should figure out some reasonable way to organize all this shit.

(DEFINE (ADJUST-TP FROM TO)
; (EMITREMARK "Adjust TP")  ;Remark already pending
  (IF (NEQ? TO FROM)
      (EMIT `(MOVAL (REG TP (- ,TO ,FROM)) TP))))

(DEFINE (GEN-LOSSAGE)
  (EMIT '(JSB (@ (SLINK COMPILER-LOSSAGE)))))

(DEFINE (GEN-INAPPLICABLE)
  (EMIT '(JMP (@ (SLINK INAPPLICABLE)))))

(DEFINE (UNCONDITIONAL-JUMP TAG)
  (XCASE *ASSEMBLER-TYPE*
    ((VAX-UNIX) (EMITJ 'JBR TAG NIL))
    ((VAX-VMS)  (EMITJ 'BRW TAG NIL)))
  'GONE)

;;; Compare LOC1 with LOC2.  Jump to EQTAG iff they're EQ? and not REVERSEP.

(DEFINE (COMPARE LOC1 LOC2 EQTAG REVERSEP)
  (EMIT `(CMPL ,LOC1 ,LOC2))
  (EMITJ 'JEQL EQTAG REVERSEP))

;;; Cons &REST arg and return a pointer to it.

(DEFINE (GET-REST-ARG NARGS REST-ARG)
  (COND (REST-ARG
         (EMITREMARK (FORMAT NIL "~D required argument~P" NARGS NARGS))
         (MOVE `(LIT ,NARGS) 'R0)
         (EMITREMARK "Get list of remaining args")
         (EMIT '(JSB (@ (SLINK LEXPR-SETUP))))
         *VAL*)
        (ELSE
         (MOVE-ADDRESS `(REG AP ,(FX* NARGS -4)) 'SP))))

;;; The heap pointer lives in register HP.  Its type tag is a parameter,
;;; whose proper place of definition would be here except that it has to
;;; be initialized AFTER the other type tags are set; see DEFINE-PRIMITIVE-TYPE.

;;; Utility used by consers.

(DEFINE (ADJUST-TAG SOURCE DEST SYM)
  (LET ((HEAPTAG (MEVAL-LAP-OFFSET '%%HEAP-TAG))
        (LOCTAG  (MEVAL-LAP-OFFSET SYM)))
    (COND ((FX= HEAPTAG LOCTAG)
           (MOVE SOURCE DEST))
          (ELSE
           (EMITREMARK "Adjust type tag")
           ;; Force short (6-bit) literal.  The hair here is due to the fact 
           ;; that short literals can't be negative.
           (GEN-TOP (COND ((FX< HEAPTAG LOCTAG) '(ADDL3 ADDL2))
                          (ELSE                 '(SUBL3 SUBL2)))
                    `(LIT ,(FIXNUM-ABS (FX- LOCTAG HEAPTAG)))
                    SOURCE
                    DEST)))))

;;; Generalized extend conser.  SIZE is in quadwords, not bytes.

(DEFINE (GEN-MAKE-EXTEND TEMPLATE SIZE DEST)
  (COND ((NOT (REGISTER? DEST))
         (BUG "destination not a register in GEN-MAKE-EXTEND"
              "will generate no code"))
        ((FX< SIZE 1)
         (BUG "requested extend size is too small"
              "will generate no code"))
        ((FX= SIZE 1)
         (EMIT `(MOVAQ (REG+ HP) ,DEST))
         (EMITREMARK "Store template")
         (IF (NOT (ALIKEV? TEMPLATE '(LIT 0)))
             (MOVE TEMPLATE `(REG ,DEST (~ %%HEAP-TAG))))
         (ADJUST-TAG DEST DEST '%%EXTEND-TAG))
        (ELSE
         (IF (NOT (ALIKEV? TEMPLATE '(LIT 0)))
             (MOVE TEMPLATE '(REG HP (~ %%HEAP-TAG))))
         (ADJUST-TAG 'HP DEST '%%EXTEND-TAG)
         (EMITREMARK "Bump heap pointer")
         (EMIT `(ADDL2 (LIT ,(LSH SIZE *SHIFT*)) HP))))
  DEST)

(DEFINE (USE-ALIAS? SOURCE ALIASP)
  (CASE ALIASP
    ((POINTER-REG) (MEMQ SOURCE *POINTER-REG-NAMES*))
    ((SCRATCH-REG) (MEMQ SOURCE *SCRATCH-REG-NAMES*))
    ((INDIRECTABLE)
     (OR (ATOM? SOURCE)
         (MEMQ (CAR SOURCE) '(REG REG+ SLINK REL DATUM ADDR UADDR))))
    (ELSE ALIASP)))

(DEFUN (DWFLO MOVE) (SOURCE DEST)
  (COND ((ALIKEV? SOURCE '(LIT 0.0))
         (EMIT `(CLRD ,DEST)))
        (ELSE
         (EMIT `(MOVD ,SOURCE ,DEST)))))

(DEFUN (HALFPESO MOVE) (SOURCE DEST)
  (COND ((ALIKEV? SOURCE '(LIT 0))
         (EMIT `(CLRW ,DEST)))
        (ELSE
         (EMIT `(MOVW ,SOURCE ,DEST)))))

(DEFUN (BYTE MOVE) (SOURCE DEST)
  (COND ((ALIKEV? SOURCE '(LIT 0))
         (EMIT `(CLRB ,DEST)))
        (ELSE
         (EMIT `(MOVB ,SOURCE ,DEST)))))

;;; Someday hack MNEGL, CVTBL, CVTWL, MOVZBL, MOVZWL for literal fixnums
;;; in various ranges; when the 68000 gets more VAX instructions

(DEFINE (MOVE SOURCE DEST)
  (COND ((ALIKEV? SOURCE DEST))
        ((ALIKEV? SOURCE '(LIT 0)) (EMIT `(CLRL ,DEST)))
        ((AND (PAIR? SOURCE) (EQ? (CAR SOURCE) 'ADDR))  ;Pseudo-type
         (SETQ DEST (MOVE-ADDRESS (CADR SOURCE) DEST)))
        ((AND (PAIR? SOURCE) (EQ? (CAR SOURCE) 'UADDR))
         (SETQ DEST (MOVE-ADDRESS (UNIT-REF (CADR SOURCE)) DEST)))
        ((ALIKEV? DEST *PUSH*) (EMIT `(PUSHL ,SOURCE)))
        (ELSE (EMIT `(MOVL ,SOURCE ,DEST))))
  (MAYBE-INCREMENT-STACKNUM DEST))

(DEFINE (MOVE-ADDRESS LOC DEST)
  (COND ((AND (PAIR? LOC)
              (EQ? (CAR LOC) 'REG)
              (OR (NULL? (CDDR LOC)) (ALIKEV? (CADDR LOC) 0)))
         (MOVE (CADR LOC) DEST))
        ((ALIKEV? DEST *PUSH*)
         (EMIT `(PUSHAL ,LOC))
         (INCR *STACKNUM*)
         '(REG SP))
        (ELSE
         (EMIT `(MOVAL ,LOC ,DEST))
         DEST)))

(DEFINE-REP-CONVERTER ((POINTER POINTER)) (NODE SOURCE DEST ALIASP)
  (IGNORE NODE)
  (IF ALIASP SOURCE (MOVE SOURCE DEST)))

;;; Generate CVT or MOVZ instruction.

(WALK (LAMBDA (Z)
        (DESTRUCTURE (((FROM TO OP) Z))
          (SET-REP-CONVERTER FROM TO
            (LAMBDA (NODE SOURCE DEST ALIASP)
              (IGNORE NODE ALIASP)
              (EMIT `(,OP ,SOURCE ,DEST))
              (MAYBE-INCREMENT-STACKNUM DEST))
            NIL)))
      '((MFIX     HALFPESO CVTLW)
        (MFIX     BYTE     CVTLB)
        (HALFPESO BYTE     CVTWB)
        (HALFPESO MFIX     CVTWL)
        (BYTE     MFIX     MOVZBL)
        (BYTE     HALFPESO MOVZBW)
        ))

;;; We assume in the case of HALFPESO's and BYTE's that the
;;; high 16 or 24 bits of the dest, resp., are clobberable.

(DEFINE-REP-CONVERTER ((POINTER MFIX)
                       (POINTER HALFPESO)
                       (POINTER BYTE)
                       (POINTER CHAR))    (NODE SOURCE DEST ALIASP)
  (IGNORE NODE ALIASP)
  (EMIT `(ASHL (LIT -3.) ,SOURCE ,DEST))
  (MAYBE-INCREMENT-STACKNUM DEST))

(DEFINE-REP-CONVERTER ((POINTER QFIX)) (NODE SOURCE DEST ALIASP)
  (IGNORE NODE ALIASP)
  (EMIT `(ASHQ (LIT -35.) ,SOURCE ,DEST))
  DEST)

;;; LIFETIME SCREWS!

(DEFINE-REP-CONVERTER ((POINTER DWFLO)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC POINTER-REG)
  (LET ((FOO (SIMPLEREF SOURCE '(~ %%FLONUM-TAG)
                        (IF (NOT (REGISTER? SOURCE))
                            (CONVERT-REP-SPARELOC NODE)))))
    (COND (ALIASP FOO)
          (ELSE (EMIT `(MOVD ,FOO ,DEST))
                (MAYBE-INCREMENT-STACKNUM DEST)))))  ;unlikely

(DEFINE-REP-CONVERTER ((MFIX POINTER)) (NODE SOURCE DEST ALIASP)
  (IGNORE NODE ALIASP)
  (COND ((AND (PAIR? SOURCE) (EQ? (CAR SOURCE) 'ADDR))
         (LET ((NEWDEST (MOVE SOURCE DEST)))
           (EMIT `(ASHL (LIT 3) ,NEWDEST ,NEWDEST))))
        (ELSE
         (EMIT `(ASHL (LIT 3) ,SOURCE ,DEST))
         (MAYBE-INCREMENT-STACKNUM DEST))))

(DEFINE-REP-CONVERTER ((MFIX QFIX)) (NODE SOURCE DEST ALIASP)
  (IGNORE NODE ALIASP)
  (EMIT `(ASHQ (LIT -32.) ,SOURCE ,DEST))
  (MOVE SOURCE DEST))

(DEFINE-REP-CONVERTER ((HALFPESO POINTER))   (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE NODE ALIASP)
  (LET ((LOC (CONVERT-REP-SPARELOC NODE)))
    (EMIT `(CVTWL ,SOURCE ,LOC))
    (EMIT `(ASHL (LIT 3.) ,LOC ,DEST))
    (MAYBE-INCREMENT-STACKNUM DEST)))

(DEFINE-REP-CONVERTER ((BYTE POINTER))   (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE NODE ALIASP)
  (LET ((LOC (CONVERT-REP-SPARELOC NODE)))
    (EMIT `(MOVZBL ,SOURCE ,LOC))
    (EMIT `(ASHL (LIT 3.) ,LOC ,DEST))
    (MAYBE-INCREMENT-STACKNUM DEST)))

;;; Cons a flonum.

(DEFINE-REP-CONVERTER ((DWFLO POINTER)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC POINTER-REG)
  (LET ((TEMP (IF (REGISTER? DEST) DEST (CONVERT-REP-SPARELOC NODE))))
    (EMITREMARK "Cons a flonum")
    (EMIT `(MOVAQ (REG+ HP) ,TEMP))
    (ADJUST-TAG TEMP TEMP '%%FLONUM-TAG)
    (EMIT `(MOVD ,SOURCE (REG ,TEMP (~ %%FLONUM-TAG))))
    (COND ((EQ? ALIASP T) TEMP)
          (ELSE (MOVE TEMP DEST)))))

;;; Maybe we need a sparetn here, too.

(DEFINE-REP-CONVERTER ((CHAR POINTER)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (LET ((NEWSOURCE (IF (REGISTER? SOURCE) SOURCE
                     (MOVE SOURCE (CONVERT-REP-SPARELOC NODE)))))
    ;; Maybe want to use (ABS NULL-CHARACTER) here?  3 more bytes in
    ;; in instruction stream, but one fewer memory reference.
    (EMIT `(MOVAQ (IDX ,NEWSOURCE (@ (SLINK NULL-CHARACTER))) ,DEST))
    (MAYBE-INCREMENT-STACKNUM DEST)))


;;; Type-code properties, SIMPLEREF, INDIRECT

(DEFINE (SREF BASEPTR OFFSET) (SIMPLEREF BASEPTR OFFSET NIL))

;;; BASEPTR is an operand, and OFFSET is a numeric offset in bytes.
;;;  Use SPARELOC if necessary.  Returns an operand.

(DEFINE (SIMPLEREF BASEPTR OFFSET SPARELOC)
  (COND ((ALIKEV? (MEVAL-LAP-OFFSET OFFSET) 0)
         (INDIRECT BASEPTR SPARELOC))
        ((REGISTER? BASEPTR) `(REG ,BASEPTR ,OFFSET))
        ((OR (NULL? SPARELOC) (NOT (REGISTER? SPARELOC)))
         (BUG
     "can't generate a SIMPLEREF with BASEPTR=~S OFFSET=~S without a spare reg"
            "I'll use location \"SIMPLEREF-LOSER\", which of course won't work"
              BASEPTR OFFSET)
         (SIMPLEREF (MOVE BASEPTR 'SIMPLEREF-LOSER) OFFSET NIL))
        ((REGISTER? SPARELOC)
         (SIMPLEREF (MOVE BASEPTR SPARELOC) OFFSET NIL))))

;;; Construct an operand which indirects through the given source.
;;; Use SPARELOC if necessary.
;;; Note that operand which are symbols are assumed to be registers.
;;; PC-relative or absolute references should be specified as
;;; (REL FOO), not FOO.

(DEFINE (INDIRECT SOURCE SPARELOC)
  (COND ((REGISTER? SOURCE) `(REG ,SOURCE))
        ((MEMQ (CAR SOURCE) '(REG REG+ UREF SLINK REL)) `(@ ,SOURCE))
        ((MEMQ (CAR SOURCE) '(ABS DATUM)) `(@ (REL ,(CADR SOURCE))))  ;?
        ((EQ? (CAR SOURCE) 'ADDR) (CADR SOURCE))
        ((NULL? SPARELOC)
         (BUG "can't indirect through ~S without a SPARELOC"
              "will use INDIRECT-LOSER instead"
              SOURCE)
         (INDIRECT (MOVE SOURCE 'INDIRECT-LOSER) NIL))
        (ELSE (INDIRECT (MOVE SOURCE SPARELOC) NIL))))

;;; NEXTLOC

;;; Returns the next higher memory location following the given location.
;;; This is used by the MOD instruction.

(DEFINE (NEXTLOC LOC)
  (COND ((AND (PAIR? LOC) (EQ? (CAR LOC) 'REG))
         `(,(CAR LOC)
           ,(CADR LOC)
           ,(COND ((NULL? (CADDR LOC)) *POINTER*)
                  (ELSE `(+ ,(CADDR LOC) ,*POINTER*)))))
        ((AND (REGISTER? LOC) (GET LOC 'NEXT-REGISTER)))
        (ELSE (BUG "can't get next memory location following ~S"
                   "will use NEXTLOC-LOSER instead, which obviously won't work"
                   LOC)
              'NEXTLOC-LOSER)))

;;; Predicate: returns true iff operand LOC employs register REG.
;;; No one uses this any more, so far as I know...

(DEFINE (INVOLVES LOC REG)
  (COND ((REGISTER? LOC) (EQ? LOC REG))
        (ELSE (XCASE (CAR LOC)
                ((LIT SHORTLIT REL SLINK STATIC TRIVPROC) NIL)
                ((REG -REG REG+) (EQ? (CADR LOC) REG))
                ((IDX) (OR (EQ? (CADR LOC) REG)
                           (INVOLVES (CADDR LOC) REG)))
                ((@) (INVOLVES (CADR LOC) REG))))))

;;; TNLOC and friends

;;; Return a valid operand which addresses the TN.

(DEFINE (TNLOC TN)
  (COND ((NULL? TN)
         (BUG "can't compute the location of a null TN"
              "I'll return a completely hokey operand instead")
         '(BAD-OPERAND TNLOC))
        (ELSE
         (XCASE (TN-WANTLOC TN)
           ((SCRATCH-REG POINTER-REG) (TN-ISLOC TN))
           ((STACK)
            (STACKLOC (TN-ISLOC TN)))
           ((POINTER-MEM)
            (FRAMELOC (TN-ISLOC TN)))
           ((SCRATCH-MEM)
            ;; The following horror deals with the interaction of
            ;;  floating point numbers and negative-growing stacks.
            (FRAMELOC (FX+ (FX- (FX- (REGION-SCRATCH-MEM-SIZE *REGION*) 1)
                                (TN-ISLOC TN))
                           (REGION-POINTER-MEM-SIZE *REGION*))))))))

;;; Compute an operand to reference the "stack" area.  0-based; increasing
;;; indices are higher (more-recently-pushed) locations in the stack.
;;; (STACKLOC *STACKNUM*) yields a PUSH-type operand.

(DEFINE (STACKLOC INDEX)
  (LET ((OFFSET (FX- *STACKNUM* INDEX)))
    (IF (FX< OFFSET -1)
        (BUGLET ((*INDEX* INDEX))
                "index refers to a non-existent stack location"
                "will generate code that accesses above SP"))
    (COND ((FX= OFFSET 0) *PUSH*)
          ((FX= OFFSET 1) '(REG SP))
          (ELSE `(REG SP ,(FX* (FX- OFFSET 1) *POINTER*))))))

;;; OFFSET is a longword index into the argument frame (i.e. continuation);
;;; thus OFFSET = 0 addresses the continuation's template, OFFSET = 1
;;; addresses the first argument, etc.
;;; The sign is backwards from what the machine really does; this may be
;;; confusing.

(DEFINE (FRAMELOC OFFSET)
  (STACKLOC (FX- OFFSET *FRAME-SIZE*)))   ; cheat

;;; EMITPUSH, EMITJ

(DEFINE (EMITPUSH SOURCE)               ; MOVE?  -- fix this sometime!
  (EMIT `(PUSHL ,SOURCE))
  (INCR *STACKNUM*))

;;; Push N zeroes onto the stack.

(DEFINE (ZPUSH N)
  (DO ((N N (FX- N 2)))
      ((NOT (FX> N 0)))
    (COND ((FX= N 1) (EMIT '(CLRL (-REG SP))))
          (ELSE (EMIT '(CLRQ (-REG SP)))))))

(COMMENT
(DEFINE (ZPUSH-METHOD-2 N)
  (EMITREMARK "Lock out interrupts")
  (EMIT '(BISL2 (LIT 1) (SLINK INTERRUPT-FROB)))
  (EMIT `(MOVAL (REG SP ,(FX- 0 (FX* N *POINTER*))) SP))      ;Bump SP
  (EMIT `(MOVC5 (LIT 0)                 ;source length
                (REG SP)                ;source
                (LIT 0)                 ;fill byte
                (LIT ,(FX* N *POINTER*))  ;dest length
                (REG SP) ))
  (LET ((TAG (GENTAG 'ZPSH)))
    (EMITREMARK "Unlock")
    (EMIT `(BLBSC (SLINK INTERRUPT-FROB) ,TAG))
    (EMIT '(JSB (@ (SLINK HANDLE-INTERRUPT))))
    (EMITTAG TAG)))
)

(DEFINE (ADJUST-STACK STACKNUM)
  (LET ((OFFSET (FX- *STACKNUM* STACKNUM)))
    (COND ((FX< OFFSET 0)
           (BUG "arg ~S exceeds *STACKNUM* in ADJUST-STACK"
                "will make no attempt to adjust the stack"
                STACKNUM))
          ((FX= OFFSET 0))
          ((FX= OFFSET 1) (EMIT '(TSTL (REG+ SP))))
;         ((FX= OFFSET 2) (EMIT '(CMPL (REG+ SP) (REG+ SP))))
          (ELSE
           (EMIT `(MOVAL (REG SP ,(FX* *POINTER* OFFSET))
                         SP)))))
  (SETQ *STACKNUM* STACKNUM))


;;; Utilities for stack continuations, function application, and the like.

;;; Leave a procedure by invoking its continuation.

(DEFINE (GEN-IRETURN)
  (COND ((NOT (FX= (FX+ *STACKNUM* *FRAME-SIZE*) 0))
         (EMITREMARK "Get return point address")
         (MOVE-ADDRESS (FRAMELOC -1) 'SP)))
  (EMITREMARK "Return from procedure")
; (EMIT '(MOVL (REG+ SP) TP))           ; This is all that's happening, btw
; (EMIT '(JMP (REG TP)))
  (EMIT '(JMP (@ (SLINK IRETURN)))))

;;; This is just a very rough approximation... the theory doesn't really
;;;  hold water yet, but the code here is supposed to be suggestive.
;;; A major possibility not indicated by this code is that instead of
;;;  an indirect jump through an EXTEND, we might be able to do a PC-relative
;;;  transfer, if the function's template belongs to the same assembly as 
;;;  this one.  Think about this.

(DEFINE *ICALL-SAFETY* 'NORMAL)

(DEFINE (ICALL-SAFETY NODE) (IGNORE NODE) *ICALL-SAFETY*)

(DEFINE (GEN-ICALL NODE)
  (EMITREMARK "Call procedure")
  (EMIT `(JMP (@ ,(XCASE (ICALL-SAFETY NODE)
                    ((PARANOID)  '(SLINK PARANOID-ICALL))
                    ((NORMAL)    '(SLINK ICALL))
                    ((CONFIDENT) '(SLINK CONFIDENT-ICALL))
;;                  ((SECURE)    '(REG FUN %%EXTEND-TEMPLATE-OFFSET))
;;                  ^^^ no longer applicable now that TP is mandated
                    )))))

(DEFINE (GEN-DIRECT-CALL PTAGS)
  (EMITREMARK "Call known procedure")
  (ADJUST-TP *TP-LOC* (CADR PTAGS))
; (EMIT '(JMP (REG TP)))
  (UNCONDITIONAL-JUMP (CAR PTAGS)))

(DEFINE (SETUP-CONTINUATION NARGS)
  (EMIT (IF (FX= NARGS 0)
            '(MOVL SP AP)
          `(MOVAL (REG SP ,(FX* NARGS *POINTER*)) AP))))

;;; Push template for a STACK strategy procedure onto the stack.

(comment
 (DEFINE (PUSH-STACK-LAMBDA NODE)
  (LET ((FM (NODE-FORM NODE))
        (TTAG (GENTAG 'STACK)))
    (SETF (LAMBDA-GENTAG FM) TTAG)
    (IF (NOT (EQ? (LAMBDA-STACKLOC FM) *STACKNUM*))
        (BUG "a STACK LAMBDA's STACKLOC is screwed up"
             "I don't care, but there's definitely something wrong!"))
    (MOVE-ADDRESS `(REL ,(LAMBDA-GENTAG FM)) *PUSH*)
    (ZPUSH 1)))
)
;;; Primitive types:

(DEFINE (GENERATE-TAG-PREDICATE-0 NODE JUMPTAG REVERSEP LOC1)
  (EMIT `(BITL (LIT ,(FX- (LSH 1 *SHIFT*) 1)) ,LOC1))
  (EMITJ 'JEQL JUMPTAG REVERSEP))

;;; ... GENERATE-TAG-PREDICATE
;;; Could do without the SPARELOC if we did
;;;   `(CMPZV (LIT 0) (LIT 3) ,SOURCE (LIT ,SYM))
;;; This is 7 bytes instead of 5 (if SOURCE is a register).  
;;; Should do timings!

(DEFINE (GENERATE-TAG-PREDICATE NODE JUMPTAG REVERSEP SOURCE)
  (LET* ((FM (NODE-FORM NODE))
         (SPARELOC (TNLOC (GETSPARETN NODE 'FOO)))
         (SYM (CGET (NODE-FORM (CALL-FUNCTION FM)) 'TAG-PREDICATE-INFO)))
    (BRANCHCOMMENT NODE REVERSEP)
    (EMIT `(BICB3 (LIT #b11111000) ,SOURCE ,SPARELOC))
    (EMIT `(CMPB ,SPARELOC (LIT ,SYM)))
    (EMITJ 'JEQL JUMPTAG REVERSEP)))

;;; Establish values for primitive pointer type-tags

(DEFINE-PRIMITIVE-TYPE FIXNUM     0)
(DEFINE-PRIMITIVE-TYPE TEMPLATE   1)
(DEFINE-PRIMITIVE-TYPE STRING     2)
(DEFINE-PRIMITIVE-TYPE REL-ITEM   3)
(DEFINE-PRIMITIVE-TYPE EXTEND     4)
(DEFINE-PRIMITIVE-TYPE PAIR       5)
(DEFINE-PRIMITIVE-TYPE FLONUM     6)
(DEFINE-PRIMITIVE-TYPE MISC       7)

(DEFINE-LAP-CONSTANT %%CHAR-TAG      TARGET:%%MISC-TAG)
(DEFINE-LAP-CONSTANT %%NULL-TAG      TARGET:%%MISC-TAG)
(DEFINE-LAP-CONSTANT %%UNBOUND-TAG   TARGET:%%MISC-TAG)
(DEFINE-LAP-CONSTANT %%HEAP-TAG      TARGET:%%PAIR-TAG)


;;; ... Binary comparisons: things like EQ?, FIXNUM-LESS?, CHAR>=.

(DEFINE (GENERATE-BINARY-COMPARISON NODE JUMPTAG REVERSEP LOC1 LOC2)
  (LET* ((FM (NODE-FORM NODE))
         (INFO (CGET (NODE-FORM (CALL-FUNCTION FM))
                     'PRIMOP-BINARY-COMPARISON-INFO))
         (JOP (CDR INFO))
         (COP (CAR INFO)))
    ;; The following may be redundant if the condition codes are already
    ;; correct, e. g. in (COND ((-P (SETQ FOO X)) ...)).
    ;; Fix this sometime.
;   (EMITCOMMENT NIL NODE)
    (EMIT `(,COP ,LOC1 ,LOC2))
    (BRANCHCOMMENT NODE REVERSEP)
    (EMITJ JOP JUMPTAG REVERSEP)))

(BLOCK0 'PRIMOP-BINARY-COMPARISON-INFO
       (WALK (LAMBDA (GROUP)
               (DESTRUCTURE (((WANT INSTR) (CAR GROUP)))
                 (WALK (LAMBDA (FOO)
                         (CPUT (CAR FOO) 'PRIMOP-BINARY-COMPARISON-INFO
                               (CONS INSTR (CADR FOO)))
                         (CPUT (CAR FOO) 'PRIMOP-PREDICATE
                               GENERATE-BINARY-COMPARISON)
                         (CPUT (CAR FOO) 'PRIMOP T)
                         (CPUT (CAR FOO) 'PRIMOP-NUMBER-OF-ARGS '(2 . 2))
                         (CPUT (CAR FOO) 'ISREP 'NONE)
                         (CPUT (CAR FOO) 'EFFECTS 'NONE)
                         (CPUT (CAR FOO) 'AFFECTED 'NONE)
                         (IF WANT
                             (CPUT (CAR FOO) 'WANTREP WANT)))
                       (CDR GROUP))))
             '(((() CMPL)
                (EQ?    JEQL)
                (NEQ?   JNEQ)
                (CHAR=  JEQLU)
                (CHAR<  JLSSU)
                (CHAR>  JGTRU)
                (CHARN= JNEQU)
                (CHAR>= JGEQU)
                (CHAR<= JLEQU)
                (POINTER-EQUAL?    JEQLU)
                (POINTER-LESS?     JLSSU)
                (POINTER-GREATER?  JGTRU)
                (POINTER-NOT-EQUAL?   JNEQU)
                (POINTER-NOT-LESS?    JGEQU)
                (POINTER-NOT-GREATER? JLEQU))
               ((SWFIX CMPL)
                (FIXNUM-EQUAL?    JEQL)
                (FIXNUM-LESS?     JLSS)
                (FIXNUM-GREATER?  JGTR)
                (FIXNUM-NOT-EQUAL?   JNEQ)
                (FIXNUM-NOT-LESS?    JGEQ)
                (FIXNUM-NOT-GREATER? JLEQ))
               ((DWFLO CMPD)
                (FLONUM-EQUAL?    JEQL)
                (FLONUM-LESS?     JLSS)
                (FLONUM-GREATER?  JGTR)
                (FLONUM-NOT-EQUAL?   JNEQ)
                (FLONUM-NOT-LESS?    JGEQ)
                (FLONUM-NOT-GREATER? JLEQ))
               )))


;;; Sign predicates should go here!!!

;(FUNREP JUMP SWFIX
;       FIXNUM-ZERO? FIXNUM-POSITIVE? FIXNUM-NEGATIVE?)

;(FUNREP JUMP DWFLO                     ;What's this doing here?
;       FLONUM-ZERO? FLONUM-POSITIVE? FLONUM-NEGATIVE?)

(DEFINE (GEN-JBS BIT JUMPTAG REVERSEP LOC)
  (XCASE *ASSEMBLER-TYPE*
    ((VAX-UNIX)
     (EMIT `(,(IF REVERSEP 'JBC 'JBS) ,BIT ,LOC ,JUMPTAG)))
    ((VAX-VMS)
     (LET ((TAG (GENTAG 'JBS)))
       (EMIT `(,(IF REVERSEP 'BBS 'BBC) ,BIT ,LOC ,TAG))
       (UNCONDITIONAL-JUMP JUMPTAG)
       (EMITTAG TAG)))))

(DEFINE-PREDICATE-GENERATOR FIXNUM-ODD? (NODE JUMPTAG REVERSEP LOC)
  (BRANCHCOMMENT NODE REVERSEP)
  (GEN-JBS '(LIT 3) JUMPTAG REVERSEP LOC))
(CPUT 'FIXNUM-ODD? 'WANTREP 'SWFIX)

;;; Think of a reasonable name for this.

(DEFINE-PREDICATE-GENERATOR FIXNUM-BIT? (NODE JUMPTAG REVERSEP LOC1 LOC2)
  (BRANCHCOMMENT NODE REVERSEP)
  (GEN-JBS LOC2 JUMPTAG REVERSEP LOC1))

(CPUT 'FIXNUM-BIT? 'WANTREP 'MFIX)


;;; An "XOP" is a standard two-operand instruction.

(DEFINE (GENERATE-STANDARD-XOP NODE FALL-THROUGH? LOC)
  (LET ((FM (NODE-FORM NODE)))
    (LET ((INFO (CGET (NODE-FORM (CALL-FUNCTION FM)) 'PRIMOP-XOP-INFO))
          (DEST (ISTNLOC NODE)))
      (EMITCOMMENT NIL NODE)
      (EMIT `(,INFO ,LOC ,DEST))
      (YIELD NODE DEST FALL-THROUGH?))))

(BLOCK 'PRIMOP-XOP-INFO
       (WALK (LAMBDA (FOO)
               (DESTRUCTURE (((FN WANTREP ISREP INFO) FOO))
                 (CPUT FN 'PRIMOP-XOP-INFO INFO)
                 (CPUT FN 'PRIMOP-GENERATE GENERATE-STANDARD-XOP)
                 (CPUT FN 'PRIMOP T)
                 (CPUT FN 'PRIMOP-NUMBER-OF-ARGS '(1 . 1))
                 (CPUT FN 'EFFECTS 'NONE)
                 (CPUT FN 'AFFECTED 'NONE)
                 (IF WANTREP (CPUT FN 'WANTREP WANTREP))
                 (IF ISREP   (CPUT FN 'ISREP ISREP))
                 ))
             '((FLONUM->FIXNUM DWFLO MFIX  CVTDL)
               (FIXNUM->FLONUM MFIX  DWFLO CVTLD)
               (FIXNUM-NEGATE  SWFIX SWFIX MNEGL)
               (FIXNUM-LOGNOT  MFIX  MFIX  MCOML)
               (POINTER-LOGNOT ()    ()    MCOML)
               )))

;;; A "TOP" is a standard three-operand instruction.

(DEFINE (GENERATE-STANDARD-TOP NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((FM (NODE-FORM NODE)))
    (LET ((INFO (CGET (NODE-FORM (CALL-FUNCTION FM)) 'PRIMOP-TOP-INFO))
          (DEST (COND ((AND (TRIVIALLY-COERCIBLE? (NODE-ISREP NODE)
                                                  (NODE-WANTREP NODE))
                            (NODE-WANTTN NODE))
                       (WANTTNLOC NODE))
                      (ELSE (ISTNLOC NODE)))))
      (COND ((NULL? INFO)
             (BUGLET ((*NODE* NODE))
                     "can't find PRIMOP-TOP-INFO for this node"
                     "will emit a BPT instruction with operands (!)")
             (SETQ INFO '(BPT BPT))))
      (EMITCOMMENT NIL NODE)
      (YIELD NODE
             (GEN-TOP INFO LOC2 LOC1 DEST)
             FALL-THROUGH?))))

(DEFINE (GEN-TOP INFO LOC1 LOC2 DEST)
  (COND ((OR (NOT (ALIKEV? LOC2 DEST)) (NULL? (CADR INFO)))
         (EMIT `(,(CAR INFO) ,LOC1 ,LOC2 ,DEST)))
        ((AND (EQ? (CADR INFO) 'ADDL2)  ; Peephole pessimization
              (ALIKEV? LOC1 '(LIT 1)))
         (EMIT `(INCL ,DEST)))
        ((AND (EQ? (CADR INFO) 'SUBL2)
              (ALIKEV? LOC1 '(LIT 1)))
         (EMIT `(DECL ,DEST)))
        (ELSE
         (EMIT `(,(CADR INFO) ,LOC1 ,DEST))))
  (MAYBE-INCREMENT-STACKNUM DEST))

(BLOCK0 'PRIMOP-TOP-INFO
       (WALK (LAMBDA (FOO)
               (CPUT (CAR FOO) 'PRIMOP-TOP-INFO (CDR FOO))
               (CPUT (CAR FOO) 'PRIMOP-GENERATE
                     GENERATE-STANDARD-TOP)
               (CPUT (CAR FOO) 'TN-SCRIPT
                     '((ARG 2) (PREF (ARG 1) (RESULT))))
               (CPUT (CAR FOO) 'PRIMOP T)
               (CPUT (CAR FOO) 'PRIMOP-NUMBER-OF-ARGS '(2 . 2))
               (CPUT (CAR FOO) 'EFFECTS 'NONE)
               (CPUT (CAR FOO) 'AFFECTED 'NONE)
               )
             '((FIXNUM-ADD      ADDL3 ADDL2)
               (FIXNUM-SUBTRACT SUBL3 SUBL2)
               (FIXNUM-MULTIPLY MULL3 MULL2)
               (FIXNUM-DIVIDE   DIVL3 DIVL2)
               (FIXNUM-LOGIOR   BISL3 BISL2)
               (FIXNUM-LOGANDC2 BICL3 BICL2)
               (FIXNUM-LOGXOR   XORL3 XORL2)

               (FLONUM-ADD      ADDD3 ADDD2)
               (FLONUM-SUBTRACT SUBD3 SUBD2)
               (FLONUM-MULTIPLY MULD3 MULD2)
               (FLONUM-DIVIDE   DIVD3 DIVD2)

               (POINTER-ADD      ADDL3 ADDL2)
               (POINTER-SUBTRACT SUBL3 SUBL2)
               (POINTER-MULTIPLY MULL3 MULL2)   ; ?
               (POINTER-DIVIDE   DIVL3 DIVL2)   ; ?
               (POINTER-LOGIOR   BISL3 BISL2)
               (POINTER-LOGANDC2 BICL3 BICL2)
               (POINTER-LOGXOR   XORL3 XORL2)   ; ?
               (POINTER-ASH      ASHL ())
               (POINTER-ASHL     ASHL ())       ; ?
               (FIXNUM-ASH       ASHL ())       ; ?
               (FIXNUM-ASHL      ASHL ())       ; ?
               
               (MAKE-POINTER     BISL3 BISL2)
               )))
(FUNREP SWFIX SWFIX
        FIXNUM-ADD  FIXNUM-SUBTRACT
        FIXNUM-LOGIOR  FIXNUM-LOGANDC2)
(FUNREP SWFIX (MFIX SWFIX) FIXNUM-MULTIPLY)
(FUNREP MFIX SWFIX FIXNUM-DIVIDE)

(FUNREP DWFLO DWFLO
        FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE)

(FUNREP POINTER (SWFIX MFIX) MAKE-POINTER FIXNUM-ASHL)
(FUNREP POINTER (POINTER MFIX) POINTER-ASH POINTER-ASHL)
(FUNREP MFIX    MFIX           FIXNUM-ASH   FIXNUM-LOGXOR)

(COMMENT
;;; (FIXNUM-DIV2 DIVISOR DIVIDEND (LAMBDA (QUOTIENT REMAINDER) RESULT))

(DEFINE-GENERATOR FIXNUM-DIV2 (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (IGNORE LOC3)
  (LET* ((FM (NODE-FORM NODE))
         (LNODE (CADDR (CALL-ARGS FM)))
         (LM (NODE-FORM LNODE))
         (DEST1 (TNLOC (GETSPARETN NODE 'QUOTIENT)))
         (VAR1 (CAR (LAMBDA-VARS LM)))
         (DEST2 (TNLOC (GETSPARETN NODE 'REMAINDER)))
         (VAR2 (CADR (LAMBDA-VARS LM))))
    ;; LOC1 should be a quadword fixnum! 
    (EMIT `(EDIV ,LOC1 ,LOC2 ,DEST1 ,DEST2))
    (TN-MOVE (VARIABLE-TN VAR1) (VARIABLE-REP VAR1) 'MFIX DEST1 NIL)
    (TN-MOVE (VARIABLE-TN VAR2) (VARIABLE-REP VAR2) 'MFIX DEST2 NIL)
    (GENERATE (LAMBDA-BODY LM) FALL-THROUGH?)))

(FUNREP POINTER (QFIX MFIX (CONT MFIX MFIX)) FIXNUM-DIV2)
(CPUT 'FIXNUM-DIV2 'BIND-ANNOTATE-PRIMOP-INFO '(() () JUMP))
)


;;; String primops

;;; (STRING-HEAD string) gets the first character from a string.

(DEFINE-GENERATOR STRING-HEAD (NODE FALL-THROUGH? LOC)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (EMIT `(MOVZBL (@ (REG ,LOC)) ,DEST))
    ;; The MAYBE-INCREMENT-STACKNUM isn't really necessary.
    (YIELD NODE (MAYBE-INCREMENT-STACKNUM DEST) FALL-THROUGH?)))

(CPUT 'STRING-HEAD 'ISREP 'CHAR)
(CPUT 'STRING-HEAD 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))     ; Hack
(CPUT 'STRING-HEAD 'EFFECTS 'NONE)
(CPUT 'STRING-HEAD 'AFFECTED '(SET-STRING-ELT STRING-TAIL!))
(CPUT 'STRING-HEAD 'SETTER (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-STRING-HEAD T))

;;; (SET-STRING-HEAD string character) sets a string's first character.

(DEFINE-GENERATOR SET-STRING-HEAD (NODE FALL-THROUGH? LOC1 LOC2)
  (EMITCOMMENT NIL NODE)
  (EMIT `(CVTLB ,LOC2 (@ (REG ,LOC1)))) ; Get fixnum overflow on err
  ;; Should special case constant character at some point.
  (YIELD NODE LOC2 FALL-THROUGH?))

(CPUT 'SET-STRING-HEAD 'WANTREP '(POINTER CHAR))
(CPUT 'SET-STRING-HEAD 'ISREP 'CHAR)
(CPUT 'SET-STRING-HEAD 'TN-SCRIPT '((ARG 1) (PREF (ARG 2) (RESULT))))
(CPUT 'SET-STRING-HEAD 'ARGUMENT-RESTRICTIONS '((POINTER-REG) ()))
(CPUT 'SET-STRING-HEAD 'RESULT-RESTRICTION '(POINTER-REG))      ; temporary hack
(CPUT 'SET-STRING-HEAD 'EFFECTS '(SET-STRING-ELT))
(CPUT 'SET-STRING-HEAD 'AFFECTED '(STRING-TAIL!))

;;; (STRING-ELT string index) gets some character from a string.
;;; Should optimize the case of constant second arg.

(DEFINE-GENERATOR STRING-ELT (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (EMIT `(MOVZBL (IDX ,LOC2 (@ (REG ,LOC1))) ,DEST))
    (YIELD NODE (MAYBE-INCREMENT-STACKNUM DEST) FALL-THROUGH?)))

(CPUT 'STRING-ELT 'WANTREP '(POINTER MFIX))
(CPUT 'STRING-ELT 'ISREP 'CHAR)
(CPUT 'STRING-ELT 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (SCRATCH-REG)))
(CPUT 'STRING-ELT 'EFFECTS 'NONE)
(CPUT 'STRING-ELT 'AFFECTED '(SET-CHAR STRING-TAIL!))
(CPUT 'STRING-ELT 'SETTER (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-STRING-ELT T))

;;; (SET-STRING-ELT string index char) sets a character in a string.

(DEFINE-GENERATOR SET-STRING-ELT (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (EMITCOMMENT NIL NODE)
  (EMIT `(CVTLB ,LOC3 (IDX ,LOC2 (@ (REG ,LOC1)))))
  (YIELD NODE LOC3 FALL-THROUGH?))      ; Careful about lifetimes!

(CPUT 'SET-STRING-ELT 'WANTREP '(POINTER MFIX CHAR))
(CPUT 'SET-STRING-ELT 'ISREP 'CHAR)
(CPUT 'SET-STRING-ELT 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (SCRATCH-REG) ()))
(CPUT 'SET-STRING-ELT 'TN-SCRIPT '((ARG 1) (ARG 2) (PREF (ARG 3) (RESULT))))
(CPUT 'SET-STRING-ELT 'EFFECTS '(SET-CHAR))
(CPUT 'SET-STRING-ELT 'AFFECTED '(STRING-TAIL!))

;;; (CHOPY string) copies a string header.

(DEFINE-GENERATOR CHOPY (NODE FALL-THROUGH? LOC)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (EMIT `(MOVAQ (REG+ HP) ,DEST))
    (ADJUST-TAG DEST DEST '%%STRING-TAG)
    (EMIT `(MOVQ (REG ,LOC (~ %%STRING-TAG)) (REG ,DEST (~ %%STRING-TAG))))
    (YIELD NODE DEST FALL-THROUGH?)))

(CPUT 'CHOPY 'TN-SCRIPT '((RESULT) (ARG 1)))
(CPUT 'CHOPY 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'CHOPY 'RESULT-RESTRICTION '(POINTER-REG))

;;; (CHOPY! dest source) copies one string header into another.

(DEFINE-GENERATOR CHOPY! (NODE FALL-THROUGH? LOC1 LOC2)
  (EMITCOMMENT NIL NODE)
  (EMIT `(MOVQ (REG ,LOC2 (~ %%STRING-TAG)) (REG ,LOC1 (~ %%STRING-TAG))))
  (YIELD NODE LOC1 FALL-THROUGH?))

(CPUT 'CHOPY! 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (POINTER-REG)))

;;; Destructively "chdr" (increment) a string header.

(DEFINE-GENERATOR STRING-TAIL! (NODE FALL-THROUGH? LOC)
  (EMITCOMMENT NIL NODE)
  (EMIT `(DECW (REG ,LOC %%STRING-LENGTH-OFFSET)))
  (EMIT `(INCL (REG ,LOC %%STRING-POINTER-OFFSET)))
  (EMIT `(INCW (REG ,LOC %%STRING-BASE-OFFSET)))
  (YIELD NODE LOC FALL-THROUGH?))

(CPUT 'STRING-TAIL! 'TN-SCRIPT '((PREF (ARG 1) (RESULT))))
(CPUT 'STRING-TAIL! 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'STRING-TAIL! 'AFFECTED '(STRING-TAIL!))
(CPUT 'STRING-TAIL! 'EFFECTS '(STRING-TAIL!))

(DEFINE-GENERATOR STRING-NTHTAIL! (NODE FALL-THROUGH? LOC1 LOC2)
  (EMIT `(SUBW2 ,LOC2 (REG ,LOC1 %%STRING-LENGTH-OFFSET)))
  (EMIT `(ADDL2 ,LOC2 (REG ,LOC1 %%STRING-POINTER-OFFSET)))
  (EMIT `(ADDW2 ,LOC2 (REG ,LOC1 %%STRING-BASE-OFFSET)))
  (YIELD NODE LOC1 FALL-THROUGH?))

(CPUT 'STRING-NTHTAIL! 'TN-SCRIPT  '((ARG 2) (PREF (ARG 1) (RESULT))))
(CPUT 'STRING-NTHTAIL! 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'STRING-NTHTAIL! 'AFFECTED '(STRING-TAIL!))
(CPUT 'STRING-NTHTAIL! 'EFFECTS '(STRING-TAIL!))
(CPUT 'STRING-NTHTAIL! 'WANTREP '(POINTER MFIX))

;;; Test a string for nullity.

(DEFINE-PREDICATE-GENERATOR STRING-EMPTY? (NODE JUMPTAG REVERSEP LOC)
  (EMIT `(TSTW (REG ,LOC %%STRING-LENGTH-OFFSET)))
  (BRANCHCOMMENT NODE REVERSEP)
  (EMITJ 'JLEQU JUMPTAG REVERSEP))

(CPUT 'STRING-EMPTY? 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'STRING-EMPTY? 'AFFECTED '(STRING-TAIL! SET-STRING-LENGTH))

;;; (EXTEND-ELT extend index): gets the index'th element from an extend.

(DEFINE-GENERATOR EXTEND-ELT (NODE FALL-THROUGH? LOC1 LOC2)
  (YIELD NODE
         `(IDX ,LOC2 ,(INDIRECT LOC1 NIL))
         FALL-THROUGH?))
(CPUT 'EXTEND-ELT 'WANTREP '(POINTER MFIX))
(CPUT 'EXTEND-ELT 'ARGUMENT-RESTRICTIONS '((INDIRECTABLE) (SCRATCH-REG)))
(CPUT 'EXTEND-ELT 'EFFECTS 'NONE)
(CPUT 'EXTEND-ELT 'AFFECTED '(SET-EXTEND-ELT))
(CPUT 'EXTEND-ELT 'SETTER
      (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-EXTEND-ELT T))

;;; (EXTEND-ELT-FIXED extend constant-index)
;;; Same as above, but second arg is known to be a constant fixnum.
;;; There should be another case of this, where offset is exactly 0.

(DEFINE-GENERATOR EXTEND-ELT-FIXED (NODE FALL-THROUGH? LOC1 LOC2)
  (IGNORE LOC2)
  (LET ((ARG2 (CADR (CALL-ARGS (NODE-FORM NODE)))))
    (COND ((NOT (CONSTANT-NODE? ARG2))
           (BUGLET ((*NODE* NODE))
                   "second arg to EXTEND-ELT-FIXED isn't a CONSTANT"
                   "generate no code for the call")
           '(BAD-OPERAND EXTEND-ELT-FIXED))
          (ELSE
           (YIELD NODE
                  (SREF LOC1 (FX* (CONSTANT-VALUE (NODE-FORM ARG2)) *POINTER*))
                  FALL-THROUGH?)))))
(CPUT 'EXTEND-ELT-FIXED 'WANTREP '(POINTER NONE))
(CPUT 'EXTEND-ELT-FIXED 'ARGUMENT-RESTRICTIONS '((POINTER-REG) ()))
(CPUT 'EXTEND-ELT-FIXED 'EFFECTS 'NONE)
(CPUT 'EXTEND-ELT-FIXED 'AFFECTED '(SET-EXTEND-ELT))
(CPUT 'EXTEND-ELT-FIXED 'SETTER
      (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-EXTEND-ELT-FIXED T))

;;; (SET-EXTEND-ELT extend index value): sets the index'th elt of an extend.

(DEFINE-GENERATOR SET-EXTEND-ELT (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (MOVE LOC3 `(IDX ,LOC2 ,(INDIRECT LOC1 NIL)))
  (YIELD NODE LOC3 FALL-THROUGH?))

(CPUT 'SET-EXTEND-ELT 'ARGUMENT-RESTRICTIONS
      '((INDIRECTABLE) (SCRATCH-REG) ()))
(CPUT 'SET-EXTEND-ELT 'TN-SCRIPT '((ARG 1) (ARG 2) (PREF (ARG 3) (RESULT))))
(CPUT 'SET-EXTEND-ELT 'WANTREP '(POINTER MFIX POINTER))
(CPUT 'SET-EXTEND-ELT 'EFFECTS '(SET-EXTEND-ELT))
(CPUT 'SET-EXTEND-ELT 'AFFECTED 'NONE)

;;; (SET-EXTEND-ELT-FIXED extend constant-index value)

(DEFINE-GENERATOR SET-EXTEND-ELT-FIXED (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (IGNORE LOC2)
  (LET ((ARG2 (CADR (CALL-ARGS (NODE-FORM NODE)))))
    (COND ((NOT (CONSTANT-NODE? ARG2))
           (BUGLET ((*NODE* NODE))
                   "second arg to SET-EXTEND-ELT-FIXED isn't a CONSTANT"
                   "generate no code for the call")
           '(BAD-OPERAND SET-EXTEND-ELT-FIXED))
          (ELSE
           (MOVE LOC3
                 (SREF LOC1 (FX* (CONSTANT-VALUE (NODE-FORM ARG2)) *POINTER*)))
           (YIELD NODE LOC3 FALL-THROUGH?)))))

(CPUT 'SET-EXTEND-ELT-FIXED 'ARGUMENT-RESTRICTIONS
      '((POINTER-REG) () ()))
(CPUT 'SET-EXTEND-ELT-FIXED 'TN-SCRIPT
      '((ARG 1) (PREF (ARG 3) (RESULT))))
(CPUT 'SET-EXTEND-ELT-FIXED 'WANTREP '(POINTER NONE POINTER))
(CPUT 'SET-EXTEND-ELT-FIXED 'EFFECTS '(SET-EXTEND-ELT))
(CPUT 'SET-EXTEND-ELT-FIXED 'AFFECTED 'NONE)

;;; (%XLOC extend index): gets machine pointer to index'th elt of an EXTEND.
;;; An unbelievably dirty primitive.  For the exclusive use of the GC.

(DEFINE-GENERATOR %XLOC (NODE FALL-THROUGH? LOC1 LOC2)
  (YIELD NODE
         `(ADDR (IDX ,LOC2 ,(INDIRECT LOC1 NIL)))
         FALL-THROUGH?))
(CPUT '%XLOC 'WANTREP '(POINTER MFIX))
(CPUT '%XLOC 'ARGUMENT-RESTRICTIONS '((INDIRECTABLE) (SCRATCH-REG)))
(CPUT '%XLOC 'EFFECTS 'NONE)
(CPUT '%XLOC 'AFFECTED 'NONE)

;;; This could be done as a fixed-selector but the side-effects info would be
;;; wrong.

(DEFINE-GENERATOR %LOC-CONTENTS (NODE FALL-THROUGH? LOC)
  (EMITCOMMENT NIL NODE)
  (YIELD NODE (INDIRECT LOC NIL) FALL-THROUGH?))

(CPUT '%LOC-CONTENTS 'ARGUMENT-RESTRICTIONS '((INDIRECTABLE)))  ; ?68K
(CPUT '%LOC-CONTENTS 'AFFECTED 'ANY)
(CPUT '%LOC-CONTENTS 'EFFECTS 'NONE)

(DEFINE-GENERATOR %SET-LOC-CONTENTS (NODE FALL-THROUGH? LOC1 LOC2)
  (EMITCOMMENT NIL NODE)
  (MOVE LOC2 (INDIRECT LOC1 NIL))
  (YIELD NODE LOC2 FALL-THROUGH?))

(CPUT '%SET-LOC-CONTENTS 'ARGUMENT-RESTRICTIONS '((INDIRECTABLE) ()))   ; ?68K
(CPUT '%SET-LOC-CONTENTS 'AFFECTED 'NONE)
(CPUT '%SET-LOC-CONTENTS 'EFFECTS 'ANY)


;;;   { MREF | MSET } [ - { 8 | 16 | 32 } [ -U ] ]

(DEFINE (*DEFINE-MREF-GENERATOR NAME REP)
  (LET ((OP (LOCAL-LOOKUP *PRIMOP-NAMESPACE* NAME T))
        (SET-OP (LOCAL-LOOKUP *PRIMOP-NAMESPACE*
			      (CONCATENATE-SYMBOL 'SET- NAME) T)))
    (CPUT OP
          'PRIMOP-GENERATE
          (LAMBDA (NODE FALL-THROUGH? LOC1 LOC2)
            (YIELD NODE
                   `(IDX ,LOC2 ,(INDIRECT LOC1 NIL))
                   FALL-THROUGH?)))
    (CPUT OP 'PRIMOP T)
    (CPUT OP 'ISREP REP)
    (CPUT OP 'WANTREP '(POINTER MFIX))
    (CPUT OP 'ARGUMENT-RESTRICTIONS '((INDIRECTABLE) (SCRATCH-REG)))
    (CPUT OP 'EFFECTS 'NONE)
    (CPUT OP 'SETTER SET-OP)

    (CPUT SET-OP
          'PRIMOP-GENERATE
          (LAMBDA (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
            (XMOVE LOC3 `(IDX ,LOC2 ,(INDIRECT LOC1 NIL)) REP 1)
            (YIELD NODE LOC3 FALL-THROUGH?)))
    (CPUT SET-OP 'PRIMOP T)
    (CPUT SET-OP
          'ARGUMENT-RESTRICTIONS
          '((INDIRECTABLE) (SCRATCH-REG) ()))
    (CPUT SET-OP 'TN-SCRIPT '((ARG 1) (ARG 2) (PREF (ARG 3) (RESULT))))
    (CPUT SET-OP 'WANTREP `(POINTER MFIX ,REP))
    (CPUT SET-OP 'AFFECTED 'NONE)
    NAME))

(*DEFINE-MREF-GENERATOR 'MREF-CHAR 'CHAR)
(*DEFINE-MREF-GENERATOR 'MREF-8-U  'BYTE)
(*DEFINE-MREF-GENERATOR 'MREF-16   'HALFPESO)
(*DEFINE-MREF-GENERATOR 'MREF-32   'MFIX)
(*DEFINE-MREF-GENERATOR 'MREF      'POINTER)

;;; Consing and stuff like that:

(DEFINE-GENERATOR CONS (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (EMIT `(MOVAQ (REG+ HP) ,DEST))
    (ADJUST-TAG DEST DEST '%%PAIR-TAG)
    (MOVE LOC1 (SREF DEST '%%CAR-OFFSET))
    (MOVE LOC2 (SREF DEST '%%CDR-OFFSET))
    (YIELD NODE DEST FALL-THROUGH?)))
(CPUT 'CONS 'EFFECTS '(CONS))
(CPUT 'CONS 'AFFECTED 'NONE)
(CPUT 'CONS 'RESULT-RESTRICTION '(POINTER-REG))
(CPUT 'CONS 'TN-SCRIPT '((RESULT) (ARG 1) (ARG 2)))

(DEFINE-GENERATOR NEW-CELL (NODE FALL-THROUGH?)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (EMIT (COND ((ALIKEV? DEST *PUSH*)
                 `(PUSHAQ (REG+ HP)))
                (ELSE
                 `(MOVAQ (REG+ HP) ,DEST))))
    (LET ((NEWDEST (MAYBE-INCREMENT-STACKNUM DEST)))
      (ADJUST-TAG NEWDEST NEWDEST '%%PAIR-TAG)
      (YIELD NODE NEWDEST FALL-THROUGH?))))
(CPUT 'NEW-CELL 'EFFECTS '(CONS))
(CPUT 'NEW-CELL 'AFFECTED 'NONE)

;;; Hair this up someday to do MOVAQ etc. ... I'm too lazy right now.
;;; (MAKE-EXTEND TEMPLATE SIZE-IN-QUADWORDS)

(DEFINE-GENERATOR MAKE-EXTEND (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (MOVE LOC1 '(REG HP (~ %%HEAP-TAG)))
    (ADJUST-TAG 'HP DEST '%%EXTEND-TAG)
    (EMITREMARK "Bump heap pointer")
    (EMIT `(ADDL2 ,LOC2 HP))
    (YIELD NODE DEST FALL-THROUGH?)))
(CPUT 'MAKE-EXTEND 'TN-SCRIPT '((ARG 1) (RESULT) (ARG 2)))
(CPUT 'MAKE-EXTEND 'RESULT-RESTRICTION '(POINTER-REG))

;;; Low-level, dirty, grungy stuff.

(DEFINE (GENERATE-BIT-FIELD NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (EMIT `(EXTZV ,LOC2 ,LOC3 ,LOC1 ,DEST))   ;pos.rl, size.rb, base.vb, dst.wl
    (YIELD NODE (MAYBE-INCREMENT-STACKNUM DEST) FALL-THROUGH?)))

(CPUT 'POINTER-BIT-FIELD 'PRIMOP-GENERATE GENERATE-BIT-FIELD)
(CPUT 'POINTER-BIT-FIELD 'ISREP 'MFIX)
(CPUT 'POINTER-BIT-FIELD 'WANTREP '(POINTER MFIX MFIX))
(CPUT 'POINTER-BIT-FIELD 'AFFECTED 'NONE)
(CPUT 'POINTER-BIT-FIELD 'EFFECTS  'NONE)
(CPUT 'POINTER-BIT-FIELD 'PRIMOP   'T)
(CPUT 'POINTER-BIT-FIELD 'PRIMOP-NUMBER-OF-ARGS '(3 . 3))

(CPUT 'FIXNUM-BIT-FIELD 'PRIMOP-GENERATE GENERATE-BIT-FIELD)
(CPUT 'FIXNUM-BIT-FIELD 'ISREP 'MFIX)
(CPUT 'FIXNUM-BIT-FIELD 'WANTREP '(MFIX MFIX MFIX))
(CPUT 'FIXNUM-BIT-FIELD 'AFFECTED 'NONE)
(CPUT 'FIXNUM-BIT-FIELD 'EFFECTS  'NONE)
(CPUT 'FIXNUM-BIT-FIELD 'PRIMOP   'T)
(CPUT 'FIXNUM-BIT-FIELD 'PRIMOP-NUMBER-OF-ARGS '(3 . 3))

(DEFINE-GENERATOR POINTER-TAG (NODE FALL-THROUGH? SOURCE)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (YIELD NODE
           (GEN-TOP '(BICL3 BICL2) '(SLINK ADDRESS-MASK) SOURCE DEST)
           FALL-THROUGH?)))
(CPUT 'POINTER-TAG 'ISREP 'MFIX)
(CPUT 'POINTER-TAG 'AFFECTED 'NONE)
(CPUT 'POINTER-TAG 'EFFECTS 'NONE)

(DEFINE-GENERATOR POINTER-ADDRESS (NODE FALL-THROUGH? SOURCE)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (YIELD NODE
           (GEN-TOP '(BICL3 BICL2) '(LIT 7) SOURCE DEST)
           FALL-THROUGH?)))
(CPUT 'POINTER-ADDRESS 'ISREP 'SWFIX)
(CPUT 'POINTER-ADDRESS 'AFFECTED 'NONE)
(CPUT 'POINTER-ADDRESS 'EFFECTS 'NONE)

;;; (%COPY-MEM target source count)
;;; Same arg order as REPLACE, but the count is a number of quadwords (!?).
;;; (What about implementation independence?)

(DEFINE-GENERATOR %COPY-MEM (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (EMITCOMMENT NIL NODE)
  (EMIT `(MOVC3 ,LOC3 ,(INDIRECT LOC2 NIL) ,(INDIRECT LOC1 NIL)))
  (YIELD NODE LOC1 FALL-THROUGH?))

(DEFINE-GENERATOR CHAR->POINTER (NODE FALL-THROUGH? LOC1)
  (YIELD NODE LOC1 FALL-THROUGH?))
(CPUT 'CHAR->POINTER 'WANTREP '(CHAR))
(CPUT 'CHAR->POINTER 'AFFECTED 'NONE)
(CPUT 'CHAR->POINTER 'EFFECTS 'NONE)

(DEFINE-GENERATOR POINTER->CHAR (NODE FALL-THROUGH? LOC1)
  (YIELD NODE LOC1 FALL-THROUGH?))
(CPUT 'POINTER->CHAR 'ISREP 'CHAR)
(CPUT 'POINTER->CHAR 'AFFECTED 'NONE)
(CPUT 'POINTER->CHAR 'EFFECTS 'NONE)


;; Local Modes:
;; Lisp QLOZURE Indent:1
;; Lisp BIND Indent:1
;; Lisp DESTRUCTURE Indent:1
;; Mode:Scheme
;; END:
