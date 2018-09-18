(HERALD M68FOO
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Code generation for the 68000


;;; Code generation utilities

;;;(DEFINE (ADJUST-TP FROM-TAG TO-TAG)  ; ???? which one of these should I use
;;;  (EMIT `(SUBA.W (LIT (- ,FROM-TAG ,TO-TAG)) TP)))

(DEFINE (ADJUST-TP FROM TO)
  (EMITREMARK "Adjust TP")  ;Remark already pending
  (EMIT `(LEA (REG TP (- ,TO ,FROM)) TP)))

(DEFINE (GEN-LOSSAGE)
  (EMIT '(JSB (@ (SLINK COMPILER-LOSSAGE)))))

(DEFINE (GEN-INAPPLICABLE)
  (EMIT '(JMP (SLINK INAPPLICABLE))))

(DEFINE (UNCONDITIONAL-JUMP TAG)
  (EMITJ 'BRA TAG NIL)
  'GONE)

;;; Compare LOC1 with LOC2.  Jump to EQTAG iff they're EQ and not REVERSEP.

(DEFINE (COMPARE LOC1 LOC2 EQTAG REVERSEP)
  (EMIT-COMPARE-AND-JUMP 'CMP.L LOC1 LOC2 'BEQ EQTAG REVERSEP '()))

;;; Emit a compare between LOC1 and LOC2 using COP, and jump to
;;; JUMPTAG using JOP.  If NODE is specified then a call to
;;; BRANCHCOMMENT is made.  Note the the stupid 68000 has restrictions
;;; on what can be compared (using CMP.L), and this function tries
;;; to avoid problems by reversing the operands to CMP if that will help.

(DEFINE (EMIT-COMPARE-AND-JUMP COP LOC1 LOC2 JOP JUMPTAG REVERSEP NODE)
  (COND ((OR (SYMBOL? LOC1)
             (AND (PAIR? LOC2) (EQ? (CAR LOC2) 'LIT)))
         (EMIT `(,COP ,LOC2 ,LOC1))
         (IF NODE (BRANCHCOMMENT NODE REVERSEP))
         (EMITJ (GET JOP 'REVERSED-COP) JUMPTAG REVERSEP))
        ((OR (SYMBOL? LOC2)
             (AND (PAIR? LOC1) (EQ? (CAR LOC1) 'LIT)))
         (EMIT `(,COP ,LOC1 ,LOC2))
         (IF NODE (BRANCHCOMMENT NODE REVERSEP))         
         (EMITJ JOP JUMPTAG REVERSEP))
;       (ELSE (BUG "Cant compare operands ~S and ~S" "emit no code" LOC1 LOC2))
        (ELSE (EMIT-COMPARE-AND-JUMP COP (MOVE LOC1 'D7) LOC2 JOP JUMPTAG REVERSEP NODE))))

;;; Cons &REST arg and return a pointer to it.

(DEFINE (GET-REST-ARG NARGS REST-ARG)
  (COND (REST-ARG
         (EMITREMARK (FORMAT NIL "~D required argument~P" NARGS NARGS))
         (MOVE `(LIT ,NARGS) 'D7)
         (EMITREMARK "Get list of remaining args")
         (EMIT '(JSR (SLINK LEXPR-SETUP)))
         *VAL*)
        (ELSE
         (MOVE-ADDRESS `(REG AP ,(FX- 0 (FIXNUM-ASHL NARGS 2))) 'SP))))

;;; Utility used by consers.

(DEFINE (ADJUST-TAG SOURCE DEST SYM)
  (LET ((HEAPTAG (MEVAL-LAP-OFFSET '%%HEAP-TAG))
        (LOCTAG  (MEVAL-LAP-OFFSET SYM)))
    (COND ((FX= HEAPTAG LOCTAG)
           (MOVE SOURCE DEST))
          (ELSE
           (EMITREMARK "Adjust type tag")
           (GEN-TOP
            (COND ((FX> HEAPTAG LOCTAG) '(SUB.L SUBA.W SUBA.L SUBI.L SUBQ.L))
                  (ELSE                 '(ADD.L ADDA.W ADDA.L ADDI.L ADDQ.L)))
            SOURCE
            `(LIT ,(ABS (FX- HEAPTAG LOCTAG)))
            DEST
            NIL)))))

;;; Generalized extend conser.  SIZE is in quadwords, not bytes.

(DEFINE (GEN-MAKE-EXTEND TEMPLATE SIZE DEST)
  (COND ((NOT (SYMBOL? DEST))
         (BUG "destination not a register in GENERATE-MAKE-EXTEND"
              "will generate no code"))
        ((FX< SIZE 1)
         (BUG "requested extend size is too small"
              "will generate no code"))
        (ELSE
         (MOVE (COND ((AND (PAIR? TEMPLATE) (EQ? (CAR TEMPLATE) 'ADDR))
                      (MOVE-ADDRESS (CADR TEMPLATE) DEST))
                     (ELSE TEMPLATE))
               '(REG HP (~ %%HEAP-TAG)))
         (ADJUST-TAG 'HP DEST '%%EXTEND-TAG)
         (EMITREMARK "Bump heap pointer")
         (EMIT `(ADD.L (LIT ,(LSH SIZE *SHIFT*)) HP)))) ; could be LEA if <64k
  DEST)

(DEFINE (USE-ALIAS? SOURCE ALIASP)      ;?68000
  (CASE ALIASP
    ((POINTER-REG INDIRECTABLE) (POINTER-REG? SOURCE))
    ((SCRATCH-REG) (SCRATCH-REG? SOURCE))
    (ELSE ALIASP)))

;;; Will need various XMOVE things at some point.  All okay for now.

(DEFINE (MOVE-HALFPESO SOURCE DEST)
  (COND ((AND (ALIKEV? SOURCE '(LIT 0)) (NOT (POINTER-REG? SOURCE)))
         (EMIT `(CLR.W ,DEST)))
        (ELSE
         (EMIT `(MOVE.W ,SOURCE ,DEST)))))
(PUT 'HALFPESO 'MOVE MOVE-HALFPESO)

(DEFINE (MOVE-BYTE SOURCE DEST)
  (COND ((OR (POINTER-REG? SOURCE) (POINTER-REG? DEST))
         (BUG "pointer register in BYTE movation from ~S to ~S"
              "will generate no code and assume data arrived at ~S"
              SOURCE DEST DEST)
         DEST)
        ((ALIKEV? SOURCE '(LIT 0))
         (EMIT `(CLR.B ,DEST)))
        (ELSE
         (EMIT `(MOVE.B ,SOURCE ,DEST)))))
(PUT 'BYTE 'MOVE MOVE-BYTE)
(PUT 'CHAR 'MOVE MOVE-BYTE)

(DEFINE (MOVE SOURCE DEST)
  (COND ((ALIKEV? SOURCE DEST))
        ((AND (ALIKEV? SOURCE '(LIT 0)) (NOT (POINTER-REG? DEST)))
         (EMIT `(CLR.L ,DEST)))
        ((AND (SCRATCH-REG? DEST) (8-BIT-LIT? SOURCE))
         (EMIT `(MOVEQ ,SOURCE ,DEST)))
        ((AND (PAIR? SOURCE) (EQ? (CAR SOURCE) 'ADDR))  ;Pseudo-type
         (SETQ DEST (MOVE-ADDRESS (CADR SOURCE) DEST)))
        (ELSE (EMIT `(MOVE.L ,SOURCE ,DEST))))
  (MAYBE-INCREMENT-STACKNUM DEST))

(STAT-COUNTER *MEA-HACK-COUNT*
              "non-existent MEA instruction simulated using stack")

(DEFINE (MOVE-ADDRESS SOURCE DEST)
  (COND ((ALIKEV? SOURCE DEST))
        ((ALIKEV? DEST '(-REG SP)) (EMIT `(PEA ,SOURCE)))
        ((POINTER-REG? DEST) (EMIT `(LEA ,SOURCE ,DEST)))
        ((OR (ATOM? SOURCE)
             (NOT (MEMQ (CAR SOURCE) '(UREF DATUM REL))))
         ;; This is a hack to simulate MEA - used when ADDR-LOC would bomb out
         (INCREMENT *MEA-HACK-COUNT*)
         ;(MENTION "using stack to simulate MEA" "uneasiness")
         (EMIT `(PEA ,SOURCE))
         (EMIT `(MOVE.L (REG+ SP) ,DEST)))
        (ELSE
         (EMIT `(MOVE.L (ADDR ,SOURCE) ,DEST))))
  (MAYBE-INCREMENT-STACKNUM DEST))


;;; Rep converters.

;;; These ought to use smaller moves (eg MOVE.W) where possible

(DEFINE-REP-CONVERTER ((POINTER POINTER)
                       (MFIX HALFPESO)  ; ???
                       (MFIX BYTE)
                       (HALFPESO BYTE))    (NODE SOURCE DEST ALIASP)
  (IGNORE NODE)
  (IF ALIASP SOURCE (MOVE SOURCE DEST)))        ; ??? (USE-ALIAS? SOURCE ALIASP)


(DEFINE-REP-CONVERTER ((POINTER MFIX)
                       (POINTER HALFPESO)
                       (POINTER BYTE)
                       (POINTER CHAR))      (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (EMIT-SHIFT -3 SOURCE DEST (CONVERT-REP-SPARELOC NODE)))


(DEFINE-REP-CONVERTER ((MFIX POINTER) (MFIX SWFIX)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (EMIT-SHIFT 3 SOURCE DEST (CONVERT-REP-SPARELOC NODE)))

(DEFINE-REP-CONVERTER ((HALFPESO MFIX)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (LET ((TEMPDEST (CONVERT-REP-SPARELOC NODE)))
    (EMITREMARK "HALFPESO->MFIX")
    (XMOVE SOURCE TEMPDEST 'HALFPESO 1)
    (EMIT `(EXT.L ,TEMPDEST))
    (IF (USE-ALIAS? SOURCE ALIASP)
        TEMPDEST
      (MOVE TEMPDEST DEST))))

(DEFINE-REP-CONVERTER ((HALFPESO POINTER)
                       (HALFPESO SWFIX)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (LET ((TEMPDEST (CONVERT-REP-SPARELOC NODE)))
    (EMITREMARK "HALFPESO->POINTER")
    (XMOVE SOURCE TEMPDEST 'HALFPESO 1)
    (EMIT `(EXT.L ,TEMPDEST))
    (EMIT `(ASL.L (LIT 3.) ,TEMPDEST))
    (IF (USE-ALIAS? SOURCE ALIASP)
        TEMPDEST
      (MOVE TEMPDEST DEST))))

(DEFINE-REP-CONVERTER ((BYTE MFIX)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (COND ((SCRATCH-REG? DEST)            ; Maybe clear dest, then move the byte?
         (XMOVE SOURCE DEST 'BYTE 1)
         (EMIT `(EXT.W ,DEST))
         (EMIT `(EXT.L ,DEST))
         DEST)
        (ELSE
         (LET ((NEWSOURCE (XMOVE SOURCE (CONVERT-REP-SPARELOC NODE) 'BYTE 1)))
           (EMIT `(EXT.W ,NEWSOURCE))
           (EMIT `(EXT.L ,NEWSOURCE))
           (MOVE NEWSOURCE DEST)))))

;;; This is hacked weirdly to prevent gc interrupt screws

(DEFINE-REP-CONVERTER ((BYTE POINTER)
                       (BYTE SWFIX))   (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (LET ((SPARE (CONVERT-REP-SPARELOC NODE)))
    (COND ((SCRATCH-REG? DEST)
           (XMOVE SOURCE DEST 'BYTE 1)
           (EMIT `(ASL.W (LIT 3) ,DEST))
           (EMIT `(AND.L (SLINK LOW-11-BITS) ,DEST))
           DEST)
          (ELSE
           (LET ((NEWSOURCE (XMOVE SOURCE SPARE 'BYTE 1)))
             (EMIT `(EXT.W ,NEWSOURCE))
             (EMIT `(EXT.L ,NEWSOURCE))
             (EMIT-SHIFT 3 NEWSOURCE DEST SPARE))))))


(DEFINE-REP-CONVERTER ((CHAR POINTER)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (LET ((NEWSOURCE (CONVERT-REP-SPARELOC NODE)))
    (EMIT `(CLR.L ,NEWSOURCE))
    (XMOVE SOURCE NEWSOURCE 'CHAR 1)
    (EMIT `(ASL.L (LIT 3) ,NEWSOURCE))
    (EMIT `(ADDQ.L (LIT %%CHAR-TAG) ,NEWSOURCE))
    (MOVE NEWSOURCE DEST)))

(DEFINE-REP-CONVERTER ((SWFIX MFIX*4)
                       (POINTER MFIX*4)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (EMIT-SHIFT -1 SOURCE DEST (CONVERT-REP-SPARELOC NODE)))

(DEFINE-REP-CONVERTER ((HALFPESO MFIX*4)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (COND ((SCRATCH-REG? DEST)
         (EMIT `(CLR.L ,DEST))
         (XMOVE SOURCE DEST 'HALFPESO 1)
         (EMIT `(ASL.L (LIT 2) ,DEST))
         DEST)
        (ELSE (LET ((NEWSOURCE (CONVERT-REP-SPARELOC NODE)))
                (EMIT `(CLR.L ,NEWSOURCE))
                (XMOVE SOURCE NEWSOURCE 'HALFPESO 1)
                (EMIT `(ASL.L (LIT 2) ,NEWSOURCE))
                (MOVE NEWSOURCE DEST)))))


(DEFINE-REP-CONVERTER ((MFIX*4 POINTER)
                       (MFIX*4 SWFIX)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (EMIT-SHIFT 1 SOURCE DEST (CONVERT-REP-SPARELOC NODE)))

(DEFINE-REP-CONVERTER ((MFIX MFIX*4)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (EMIT-SHIFT 2 SOURCE DEST (CONVERT-REP-SPARELOC NODE)))

(DEFINE-REP-CONVERTER ((MFIX*4 MFIX)) (NODE SOURCE DEST ALIASP)
  (NEED-SPARELOC SCRATCH-REG)
  (IGNORE ALIASP)
  (EMIT-SHIFT -2 SOURCE DEST (CONVERT-REP-SPARELOC NODE)))



(DEFINE (EMIT-SHIFT COUNT SOURCE DEST SPARELOC)
  (LET ((OP (IF (<= COUNT 0) 'ASR.L 'ASL.L))
        (COUNT (ABS COUNT)))
    (COND ((SCRATCH-REG? DEST)
           (MOVE SOURCE DEST)
           (EMIT `(,OP (LIT ,COUNT) ,DEST))
           DEST)
          ((NULL? SPARELOC)
           (BUG "need an extra TN to do a shift, but didn't get one"
                "generate no code for this shift")
           (MAYBE-INCREMENT-STACKNUM DEST))
          ((NOT (SCRATCH-REG? SPARELOC))
           (BUG "EMIT-SHIFT expected a scratch register, but got ~A instead"
                "generate no code for this shift"
                SPARELOC)
           (MAYBE-INCREMENT-STACKNUM DEST))
          (ELSE
           (MOVE SOURCE SPARELOC)
           (EMIT `(,OP (LIT ,COUNT) ,SPARELOC))
           (MOVE SPARELOC DEST)))))



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
;;; (DATUM FOO), not FOO.
;;; ?68000 - SPARELOC must be an address register!

(DEFINE (INDIRECT SOURCE SPARELOC)
  (COND ((REGISTER? SOURCE) `(REG ,SOURCE))
;?68000 ((MEMQ (CAR SOURCE) '(REG REG+ SLINK REL)) `(@ ,SOURCE))
        ((EQ? (CAR SOURCE) 'ADDR) (CADR SOURCE))
        ((NULL? SPARELOC)
         (BUG "can't indirect through ~S without a SPARELOC"
              "will use INDIRECT-LOSER instead"
              SOURCE)
         (INDIRECT (MOVE SOURCE 'INDIRECT-LOSER) NIL))
        (ELSE (INDIRECT (MOVE SOURCE SPARELOC) NIL))))

;;; Predicate: returns true iff operand LOC employs register REG.
;;; No one uses this any more, so far as I know...

(DEFINE (INVOLVES LOC REG)
  (COND ((REGISTER? LOC) (EQ? LOC REG))
        (ELSE (XCASE (CAR LOC)
                     ((LIT SHORTLIT REL SLINK STATIC TRIVPROC) NIL)
                     ((REG -REG REG+) (EQ? (CADR LOC) REG))
                     ((IDX) (OR (EQ? (CADR LOC) REG)
                                (EQ? (CADDR LOC) REG)))
                     ((@) (INVOLVES (CADR LOC) REG))))))

;;; TNLOC and friends; ALIAS-IF-SAFE

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
             ;; (RREF *REGION* 'SP (+ *FRAME-SIZE* *STACKNUM*) TN NIL)
             (FRAMELOC (TN-ISLOC TN)))
            ((SCRATCH-MEM)
             (FRAMELOC (FX+ (TN-ISLOC TN)
                            (REGION-POINTER-MEM-SIZE *REGION*))))))))
        


;;; Given an operand for a longword,  return and operand to fetch the
;;; low 16 bits

(DEFINE-INTEGRABLE (WORDLOC LOC)
  (NLOC LOC 2))

;;; Given an operand for a longword,  return and operand to fetch the
;;; low 8 bits

(DEFINE-INTEGRABLE (BYTELOC LOC)
  (NLOC LOC 3))

(DEFINE (NLOC LOC N)
;  (FORMAT T "NLOC called for loc ~S, size: ~S~%" LOC N)
  (COND ((OR (LITERAL? LOC) (REGISTER? LOC)) LOC)
        ((EQ? (CAR LOC) 'REG) 
         (IF (NULL? (CDDR LOC))
             `(,@LOC ,N)
           `(,(CAR LOC) ,(CADR LOC) (+ ,(CADDR LOC) ,N))))
        ((MEMQ (CAR LOC) '(IDX IDX.L IDX.W))
         `(,(CAR LOC) ,(CADR LOC) ,(CADDR LOC) (+ ,(CADDDR LOC) ,N)))
        (ELSE
         (BUG "Tried to make ~S into a 8 bit operand"
              "will return it as is" LOC)
         LOC)))

;;; Hack to fix asymmetry in 68K between fetch a byte(word) from a
;;; register or from memory.

(DEFINE (ADJUST-LOC TN REP LOC)
  (COND ((EMPTY REP)
         (BUGLET ((*TN* TN))
                 "Unitialized TN-REP for TN ~S"
                 "don't know what to do"
                 LOC))
        (ELSE (CASE REP
                ((POINTER SWFIX MFIX MFIX*4) LOC)
                ((HALFPESO) (WORDLOC LOC))
                ((CHAR BYTE) (BYTELOC LOC))
                (ELSE (BUG "ADJUST-LOC got a rep of ~S for location ~S"
                           "don't know what to do"
                           REP
                           LOC))))))



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
          (ELSE `(REG SP ,(FIXNUM-ASHL (FX- OFFSET 1) 2))))))

;;; OFFSET is a longword index into the argument frame (i.e. continuation);
;;; thus OFFSET = 0 addresses the continuation's template, OFFSET = 1
;;; addresses the first argument, etc.
;;; The sign is backwards from what the machine really does; this may be
;;; confusing.

(DEFINE (FRAMELOC OFFSET)
  (STACKLOC (FX- OFFSET *FRAME-SIZE*)))   ; cheat


;;; EMITPUSH

(DEFINE (EMITPUSH SOURCE)
  (MOVE SOURCE *PUSH*)
  (INCREMENT-STACKNUM))

;;; Push N zeroes onto the stack.
;;; Someday this should write a loop to do the clear for big frames

(DEFINE (ZPUSH N)
  (DO ((N N (FX- N 1)))
      ((FX= N 0.))
    (EMIT '(CLR.L (-REG SP)))))

;;; ... for N > 5
;;; ... The following takes 12 bytes, each "clr.l -(sp)" takes 2
;;; (EMIT `(MOVE.W (LIT ,N) ,*RANDOM-SCRATCH-REG-1*)
;;; (EMIT `(CLR.L ,*RANDOM-SCRATCH-REG-2*))
;;; (LET ((LOOPTAG (GENTAG 'CLEAR)))
;;;   (EMITTAG LOOPTAG)
;;;   (EMIT '(MOVE.L ,*RANDOM-SCRATCH-REG-2* (-REG SP)))
;;;   (EMIT `(DBEQ ,*RANDOM-SCRATCH-REG-1* ,LOOPTAG))))

(DEFINE (ADJUST-STACK STACKNUM)
  (LET ((OFFSET (FX- *STACKNUM* STACKNUM)))
    (COND ((FX< OFFSET 0)
           (BUG "arg ~S exceeds *STACKNUM* in ADJUST-STACK"
                "will make no attempt to adjust the stack"
                STACKNUM))
          ((FX= OFFSET 0))
          (ELSE (EMIT `(LEA (REG SP ,(FIXNUM-ASHL OFFSET 2)) SP)))))
  (SETQ *STACKNUM* STACKNUM))


;;; Utilities for stack continuations, function application, and the like.


;;; Leave a procedure by invoking its continuation.  

(DEFINE (GEN-IRETURN)
  (COND ((NOT (FX= (FX+ *STACKNUM* *FRAME-SIZE*) 0))
         (EMITREMARK "Get return point address")
         (MOVE-ADDRESS (FRAMELOC -1) 'SP)))
  (EMITREMARK "Return from procedure")
; (EMIT '(MOVE.L (REG+ SP) TP))         ; This is all that's happening, btw
; (EMIT '(JMP (REG TP)))
  (EMIT `(JMP (SLINK IRETURN))))

;;; This is just a very rough approximation... the theory doesn't really
;;;  hold water yet, but the code here is supposed to be suggestive.
;;; A major possibility not indicated by this code is that instead of
;;;  an indirect jump through an EXTEND, we might be able to do a PC-relative
;;;  transfer, if the function's template belongs to the same assembly as 
;;;  this one.  Think about this.

(DEFINE *ICALL-SAFETY* 'NORMAL)

(DEFINE (ICALL-SAFETY NODE) (IGNORE NODE) *ICALL-SAFETY*)

(DEFINE (GEN-ICALL NODE)
; (NOTE-FUNCTION-REFERENCE NODE)
  (IGNORE NODE)
  (EMITREMARK "Call the procedure")
  (EMIT `(JMP ,(CASE (ICALL-SAFETY NODE)
                 ((PARANOID)  '(SLINK PARANOID-ICALL))
                 ((NORMAL)    '(SLINK ICALL))
                 ((CONFIDENT) '(SLINK CONFIDENT-ICALL)))
              )))

(DEFINE (GEN-DIRECT-CALL PTAGS)
  (EMITREMARK "Call known procedure")
  (ADJUST-TP *TP-LOC* (CADR PTAGS))
; (EMIT '(JMP (REG TP)))
  (UNCONDITIONAL-JUMP (CAR PTAGS)))

(DEFINE (SETUP-CONTINUATION NARGS)
  (EMIT (IF (FX= NARGS 0)
            '(MOVE SP AP)
          `(LEA (REG SP ,(FIXNUM-ASHL NARGS 2)) AP))))

;;; Primitive types:

;;; ... GENERATE-TAG-PREDICATE


(DEFINE (GENERATE-TAG-PREDICATE-0 NODE JUMPTAG REVERSEP LOC1)
  (GENERATE-TAG-PREDICATE NODE JUMPTAG REVERSEP LOC1))

(DEFINE (GENERATE-TAG-PREDICATE NODE JUMPTAG REVERSEP SOURCE)
  (LET* ((FM (NODE-FORM NODE))
         (SPARELOC (TNLOC (GETSPARETN NODE 'FOO)))
         (SYM (CGET (NODE-FORM (CALL-FUNCTION FM)) 'TAG-PREDICATE-INFO)))
    (BRANCHCOMMENT NODE REVERSEP)
    (EMIT `(MOVE.L ,SOURCE ,SPARELOC))
    (EMIT `(ANDI.B (LIT #b00000111) ,SPARELOC))
    (EMIT `(CMPI.B (LIT ,SYM) ,SPARELOC))
    (EMITJ 'BEQ JUMPTAG REVERSEP))) 

;;; Establish values for primitive pointer type-tags

(DEFINE-PRIMITIVE-TYPE FIXNUM     0)
(DEFINE-PRIMITIVE-TYPE FLONUM     1)
(DEFINE-PRIMITIVE-TYPE TEMPLATE   2) ;Coordinate this with EMIT-TEMPLATE-HEADER
(DEFINE-PRIMITIVE-TYPE REL-ITEM   3)
(DEFINE-PRIMITIVE-TYPE EXTEND     4)
(DEFINE-PRIMITIVE-TYPE PAIR       5)
(DEFINE-PRIMITIVE-TYPE STRING     6)
(DEFINE-PRIMITIVE-TYPE MISC       7)

(DEFINE-LAP-CONSTANT %%CHAR-TAG       TARGET:%%MISC-TAG)
(DEFINE-LAP-CONSTANT %%NULL-TAG       TARGET:%%MISC-TAG)
(DEFINE-LAP-CONSTANT %%UNBOUND-TAG    TARGET:%%MISC-TAG)
(DEFINE-LAP-CONSTANT %%HEAP-TAG       TARGET:%%PAIR-TAG)
