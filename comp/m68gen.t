(HERALD M68GEN
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Code generation for the 68000


;;; ... Binary comparisons: things like EQ?, FIXNUM-LESS?, CHAR>=.

(DEFINE (GENERATE-BINARY-COMPARISON NODE JUMPTAG REVERSEP LOC1 LOC2)
  (LET* ((FM (NODE-FORM NODE))
         (INFO (CGET (NODE-FORM (CALL-FUNCTION FM))
                     'PRIMOP-BINARY-COMPARISON-INFO))
         (JOP (CADR INFO))
         (COP (CAR INFO)))
    ;; The following may be redundant if the condition codes are already
    ;; correct, e. g. in (COND ((-P (SETQ FOO X)) ...)).
    ;; Fix this sometime.
;   (EMITCOMMENT NIL NODE)
    (EMIT-COMPARE-AND-JUMP COP LOC1 LOC2 JOP JUMPTAG REVERSEP NODE)))

(PROG1 'PRIMOP-BINARY-COMPARISON-INFO
       (WALK (LAMBDA (GROUP)
               (DESTRUCTURE (((WANT INSTR) (CAR GROUP)))
                 (WALK (LAMBDA (FOO)
                         (CPUT (CAR FOO) 'PRIMOP-BINARY-COMPARISON-INFO
                               (CONS INSTR (CDR FOO)))
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
             '(((() CMP.L)
                (EQ?    BEQ)            ; Comparisons are all backwards due to
                (NEQ?   BNE)            ; funny 68K operand order
                (CHAR=  BEQ)            ; Perhaps this should be fixed by 
                (CHAR<  BHI)            ; changing the order of LOC1 & LOC2 in
                (CHAR>  BLO)            ; GENERATE-BINARY-COMPARISON   
                (CHARN= BNE)
                (CHAR>= BLS)
                (CHAR<= BHS)
                (POINTER-EQUAL?    BEQ)
                (POINTER-LESS?     BHI)
                (POINTER-GREATER?  BLO)
                (POINTER-NOT-EQUAL?   BNE)
                (POINTER-NOT-LESS?    BLS)
                (POINTER-NOT-GREATER? BHS))
               ((SWFIX CMP.L)
                (FIXNUM-EQUAL?    BEQ)
                (FIXNUM-LESS?     BGT)
                (FIXNUM-GREATER?  BLT)
                (FIXNUM-NOT-EQUAL?   BNE)
                (FIXNUM-NOT-LESS?    BLE)
                (FIXNUM-NOT-GREATER? BGE)))))


;;; Sign predicates should go here!!!

;(FUNREP JUMP SWFIX
;       FIXNUM-ZERO? FIXNUM-POSITIVE? FIXNUM-NEGATIVE?)

;(FUNREP JUMP DWFLO                     ;What's this doing here?
;       FLONUM-ZERO? FLONUM-POSITIVE? FLONUM-NEGATIVE?)


;;; Hair these next two up to be somewhat smarter (constant args etc)
(DEFINE-PREDICATE-GENERATOR FIXNUM-ODD? (NODE JUMPTAG REVERSEP LOC)
  (BRANCHCOMMENT NODE REVERSEP)
  (EMIT `(BTST (LIT 3) ,LOC))
  (EMIT `(,(IF REVERSEP 'BEQ 'BNE) ,JUMPTAG)))  ; why isn't this an EMITJ?
(CPUT 'FIXNUM-ODD? 'WANTREP 'SWFIX)
(CPUT 'FIXNUM-ODD? 'ARGUMENT-RESTRICTIONS '((SCRATCH-REG)))

;;; Think of a reasonable name for this.

(DEFINE-PREDICATE-GENERATOR FIXNUM-BIT? (NODE JUMPTAG REVERSEP LOC1 LOC2)
  (BRANCHCOMMENT NODE REVERSEP)
  (LET ((TMP (TNLOC (GETSPARETN NODE 'TMP))))
    (EMIT `(BTST ,(COND ((QUICK-LIT? LOC2) LOC2)
                        (ELSE (MOVE LOC2 TMP) TMP))
                 ,LOC1))
  (EMIT `(,(IF REVERSEP 'BEQ 'BNE) ,JUMPTAG))))
(CPUT 'FIXNUM-BIT? 'WANTREP '(MFIX MFIX))
(CPUT 'FIXNUM-BIT? 'TN-SCRIPT
      '((SPARE TMP SCRATCH-REG ()) (ARG 2) (ARG 1) (RESULT)))
(CPUT 'FIXNUM-BIT? 'ARGUMENT-RESTRICTIONS '((SCRATCH-REG) (ANY)))

(DEFINE-PREDICATE-GENERATOR TEMPLATE-GUTS? (NODE JUMPTAG REVERSEP LOC)
  (BRANCHCOMMENT NODE REVERSEP)
  (EMIT `(MOVE.L (SLINK TEMPLATE-GUTS) D7))
  (EMIT `(CMP.L ,LOC D7))
  (EMIT `(,(IF REVERSEP 'BNE 'BEQ) ,JUMPTAG))
  )

;;; An "XOP" is a standard two-operand instruction -- but I cheat for the 68K

(DEFINE (GENERATE-STANDARD-XOP NODE FALL-THROUGH? LOC)
  (LET ((FM (NODE-FORM NODE)))
    (LET ((INFO (CGET (NODE-FORM (CALL-FUNCTION FM)) 'PRIMOP-XOP-INFO))
          (DEST (ISTNLOC NODE)))
      (EMITCOMMENT NIL NODE)
      (MOVE LOC DEST)
      (EMIT `(,INFO ,DEST))
      (YIELD NODE DEST FALL-THROUGH?))))

(PROGN 'PRIMOP-XOP-INFO
       (WALK (LAMBDA (FOO)
               (DESTRUCTURE (((FN WANTREP ISREP INFO) FOO))
                            (CPUT FN 'PRIMOP-XOP-INFO INFO)
                            (CPUT FN 'PRIMOP-GENERATE GENERATE-STANDARD-XOP)
                            (CPUT FN 'PRIMOP T)
                            (CPUT FN 'PRIMOP-NUMBER-OF-ARGS '(1 . 1))
                            (CPUT FN 'EFFECTS 'NONE)
                            (CPUT FN 'AFFECTED 'NONE)
                            (CPUT FN 'RESULT-RESTRICTION '(SCRATCH-REG))
                            (IF WANTREP (CPUT FN 'WANTREP WANTREP))
                            (IF ISREP   (CPUT FN 'ISREP ISREP))
                            ))
             '((FIXNUM-NEGATE  SWFIX SWFIX NEG.L)
               (FIXNUM-LOGNOT  MFIX MFIX NOT.L)
               )))

;;; A "TOP" is a standard three-operand instruction, (utilities first)

(DEFINE (GENERATE-STANDARD-TOP NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((FM (NODE-FORM NODE)))
    (LET ((INFO (CGET (NODE-FORM (CALL-FUNCTION FM)) 'PRIMOP-TOP-INFO))
          (SPARE (LET ((TN (GETSPARETN NODE 'TMP))) (IF TN (TNLOC TN))))
          (DEST (COND ((AND (MEMQ (NODE-WANTREP NODE) '(SWFIX POINTER))
                            (NODE-WANTTN NODE))
                       (WANTTNLOC NODE))
                      (ELSE (ISTNLOC NODE)))))
      (COND ((NULL? INFO)
             (BUGLET ((*NODE* NODE))
                     "can't find PRIMOP-TOP-INFO for this node"
                     "will assume answer is in D7")
             (YIELD NODE 'D7 FALL-THROUGH?))
            (ELSE (EMITCOMMENT NIL NODE)
               (YIELD NODE
                      (IF (MEMQ (CAR INFO) '(AND.L OR.L EOR.L))
                          (GEN-TOP-1 (CAR INFO) LOC1 LOC2)
                        (GEN-TOP INFO LOC1 LOC2 DEST SPARE))
                      FALL-THROUGH?))))))

;;; RETHINK! this is too complicated
;;; For SUB, do SUB loc1,loc2 (loc2-loc1->dest)
;;; If LOC1 and DEST are the same, say B, and LOC2 is A, do SUB A,B;NEG B
(DEFINE (GEN-TOP INFO LOC1 LOC2 DEST SPARE)
  (COND ((ALIKEV? LOC2 DEST)
         (GEN-TOP-523 INFO LOC1 LOC2 T SPARE))
        (ELSE
         (LET ((NEWDEST (MOVE LOC1 DEST)))
           (GEN-TOP-523 INFO
                        ;; Miserable hack to kludge around famous + bug!
                        (COND ((AND (ALIKEV? DEST *PUSH*)
                                    (PAIR? LOC2)
                                    (EQ? (CAR LOC2) 'REG)
                                    (EQ? (CADR LOC2) 'SP))
                               `(REG SP (+ ,(OR (CADDR LOC2) 0) 4)))
                              (ELSE LOC2))
                        NEWDEST
                        NIL
                        SPARE)))))

(DEFINE (GEN-TOP-523 INFO SOURCE DEST COMMUTED? SPARE)
  (LET ((FINAL (GEN-ADD-SUB INFO SOURCE DEST SPARE)))
    (COND ((AND COMMUTED? (EQ? (CAR INFO) 'SUB.L))
           (EMITREMARK "Attention: commuted subtract at large")
           (COND ((NOT (POINTER-REG? FINAL))
                  (EMIT `(NEG.L ,FINAL)))
                 (ELSE
                  (MOVE FINAL SPARE)
                  (EMIT `(NEG.L ,SPARE))
                  (MOVE SPARE FINAL)))))
    FINAL))

(DEFINE (GEN-ADD-SUB INFO SOURCE DEST SPARE)
  (COND ((QUICK-LIT? SOURCE)
         (GEN-TOP-1 (LAST INFO) SOURCE DEST))
        ((POINTER-REG? DEST)
         (GEN-TOP-1 (IF (16-BIT-LIT? SOURCE)
                        (CADR INFO)     ; adda.w (gets signed extended!)
                      (CADDR INFO))     ; adda.l
                    SOURCE
                    DEST))
        ((LITERAL? SOURCE)
         (GEN-TOP-1 (CADDDR INFO) SOURCE DEST)) ;immediate
        ((OR (SCRATCH-REG? SOURCE) (SCRATCH-REG? DEST))
         (GEN-TOP-1 (CAR INFO) SOURCE DEST))    ;regular 
        (ELSE
         (MOVE SOURCE SPARE)
         (GEN-TOP-1 (CAR INFO) SPARE DEST))))


(DEFINE (GEN-TOP-1 OP SOURCE DEST)
  (EMIT `(,OP ,SOURCE ,DEST))
  (MAYBE-INCREMENT-STACKNUM DEST))

(PROG1 'PRIMOP-TOP-INFO-1
       (WALK (LAMBDA (FOO)
               (CPUT (CAR FOO) 'PRIMOP-TOP-INFO (CDR FOO))
               (CPUT (CAR FOO) 'PRIMOP-GENERATE GENERATE-STANDARD-TOP)
               (CPUT (CAR FOO) 'TN-SCRIPT
                     '((ARG 2)
                       (SPARE TMP SCRATCH-REG ())
                       (PREF (ARG 1) (RESULT))))
               (CPUT (CAR FOO) 'PRIMOP T)
               (CPUT (CAR FOO) 'PRIMOP-NUMBER-OF-ARGS '(2 . 2))
               (CPUT (CAR FOO) 'EFFECTS 'NONE)
               (CPUT (CAR FOO) 'AFFECTED 'NONE)
               )
             '((FIXNUM-ADD       ADD.L ADDA.W ADDA.L ADDI.L ADDQ.L)
               (FIXNUM-SUBTRACT  SUB.L SUBA.W SUBA.L SUBI.L SUBQ.L)
               (POINTER-ADD      ADD.L ADDA.W ADDA.L ADDI.L ADDQ.L)
               (POINTER-SUBTRACT SUB.L SUBA.W SUBA.L SUBI.L SUBQ.L)
               (MAKE-POINTER     ADD.L ADDA.W ADDA.L ADDI.L ADDQ.L)
               )))

(PROG1 'PRIMOP-TOP-INFO-2
         (WALK (LAMBDA (FOO)
                 (CPUT (CAR FOO) 'PRIMOP-TOP-INFO (CDDR FOO))
                 (CPUT (CAR FOO) 'PRIMOP-GENERATE GENERATE-STANDARD-TOP)
                 (CPUT (CAR FOO) 'PRIMOP T)
                 (CPUT (CAR FOO) 'ARGUMENT-RESTRICTIONS
                       '((SCRATCH-REG) (SCRATCH-REG)))
                 (CPUT (CAR FOO) 'PRIMOP-NUMBER-OF-ARGS '(2 . 2))
                 (CPUT (CAR FOO) 'EFFECTS 'NONE)
                 (CPUT (CAR FOO) 'AFFECTED 'NONE)
                 (LET ((REPS (CADR FOO)))
                   (COND (REPS 
                          (CPUT (CAR FOO) 'ISREP (CAR REPS))
                          (CPUT (CAR FOO) 'WANTREP (CADR REPS)))))
                 )
               '((FIXNUM-LOGIOR  () OR.L  ORI.L)
                 (FIXNUM-LOGAND  () AND.L ANDI.L)
                 (POINTER-LOGIOR () OR.L  ORI.L)
                 (POINTER-LOGAND () AND.L ANDI.L)
                 (FIXNUM-LOGXOR  (MFIX (MFIX MFIX)) EOR.L  EORI.L)
                 )))

(FUNREP SWFIX SWFIX FIXNUM-ADD  FIXNUM-SUBTRACT)

(FUNREP POINTER (SWFIX MFIX) MAKE-POINTER)


;;; String primops

;;; (STRING-HEAD string) gets the first character from a string.

(DEFINE-GENERATOR STRING-HEAD  (NODE FALL-THROUGH? LOC)
  (LET ((DEST (ISTNLOC NODE))
        (PTR (TNLOC (GETSPARETN NODE 'PTR)))
        (BASEPTR (TNLOC (GETSPARETN NODE 'BASEPTR))))
    (EMITCOMMENT NIL NODE)
    ;; offset to substr in string
    (EMIT `(MOVE.W (REG ,LOC %%STRING-BASE-OFFSET) ,BASEPTR))
    ;; ptr to beg of str
    (EMIT `(MOVE.L (REG ,LOC %%STRING-POINTER-OFFSET) ,PTR))
    (EMIT `(MOVE.B (IDX.W ,BASEPTR ,PTR 0) ,DEST))      ; char into dest
    (EMIT `(AND.L (SLINK CHARACTER-MASK) ,DEST))
    (YIELD NODE DEST FALL-THROUGH?)))

(CPUT 'STRING-HEAD 'ISREP 'CHAR)
(CPUT 'STRING-HEAD 'TN-SCRIPT '((ARG 1)
                         (SPARE PTR POINTER-REG ())
                         (SPARE BASEPTR SCRATCH-REG ())
                         (RESULT)))
(CPUT 'STRING-HEAD 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'STRING-HEAD 'RESULT-RESTRICTION '(SCRATCH-REG))
(CPUT 'STRING-HEAD 'EFFECTS 'NONE)
(CPUT 'STRING-HEAD 'AFFECTED '(SET-STRING-ELT STRING-TAIL!))
(CPUT 'STRING-HEAD 'SETTER
      (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-STRING-HEAD T))

;;; (SET-STRING-HEAD string character) sets a string's first character.

(DEFINE-GENERATOR SET-STRING-HEAD  (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((PTR (TNLOC (GETSPARETN NODE 'PTR)))
        (BASEPTR (TNLOC (GETSPARETN NODE 'BASEPTR))))
    (EMITCOMMENT NIL NODE)   
    (EMITREMARK "Get offset in string to substring")
    (EMIT `(MOVE.W (REG ,LOC1 %%STRING-BASE-OFFSET) ,BASEPTR))
    (EMITREMARK "point to head of string")
    (EMIT `(MOVE.L (REG ,LOC1 %%STRING-POINTER-OFFSET) ,PTR))
    (EMIT `(MOVE.B ,LOC2 (IDX.W ,BASEPTR ,PTR 0)))
    (YIELD NODE LOC2 FALL-THROUGH?)))

(CPUT 'SET-STRING-HEAD 'WANTREP '(POINTER CHAR))
(CPUT 'SET-STRING-HEAD 'ISREP 'CHAR)
(CPUT 'SET-STRING-HEAD 'TN-SCRIPT '((SPARE BASEPTR SCRATCH-REG ())
                             (ARG 1)
                             (SPARE PTR POINTER-REG ())
                             (SPARE BASEPTR)
                             (PREF (ARG 2) (RESULT))))
(CPUT 'SET-STRING-HEAD 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY)))
(CPUT 'SET-STRING-HEAD 'EFFECTS '(SET-STRING-HEAD))
(CPUT 'SET-STRING-HEAD 'AFFECTED '(STRING-TAIL!))

;;; (STRING-ELT string index) gets some character from a string.

(DEFINE-GENERATOR STRING-ELT  (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((DEST (ISTNLOC NODE))
        (PTR (TNLOC (GETSPARETN NODE 'PTR)))
        (BASEPTR (TNLOC (GETSPARETN NODE 'BASEPTR))))
    (EMITCOMMENT NIL NODE)
    ;; offset to substr in string
    (EMIT `(MOVE.W (REG ,LOC1 %%STRING-BASE-OFFSET) ,BASEPTR))
    ;; offset in string to char
    (EMIT `(ADD.W ,(WORDLOC LOC2) ,BASEPTR))
    ;; ptr to beg of str
    (EMIT `(MOVE.L (REG ,LOC1 %%STRING-POINTER-OFFSET) ,PTR))
    (EMIT `(MOVE.B (IDX.W ,BASEPTR ,PTR 0) ,DEST))      ; char into destination
    (EMIT `(AND.L (SLINK CHARACTER-MASK) ,DEST))
    (YIELD NODE DEST FALL-THROUGH?)))

(CPUT 'STRING-ELT 'WANTREP '(POINTER MFIX))
(CPUT 'STRING-ELT 'ISREP 'CHAR)
(CPUT 'STRING-ELT 'TN-SCRIPT '((SPARE BASEPTR SCRATCH-REG ())
                            (SPARE PTR POINTER-REG ())
                            (ARG 1)
                            (SPARE BASEPTR)
                            (ARG 2) (RESULT)))
(CPUT 'STRING-ELT 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY)))
(CPUT 'STRING-ELT 'RESULT-RESTRICTION '(SCRATCH-REG))
(CPUT 'STRING-ELT 'EFFECTS 'NONE)
(CPUT 'STRING-ELT 'AFFECTED '(SET-STRING-ELT STRING-TAIL!))
(CPUT 'STRING-ELT 'SETTER
      (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-STRING-ELT T))

;;; (SET-STRING-ELT string index char) sets a character in a string

(DEFINE-GENERATOR SET-STRING-ELT  (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (LET ((PTR (TNLOC (GETSPARETN NODE 'PTR)))
        (BASEPTR (TNLOC (GETSPARETN NODE 'BASEPTR))))
    (EMITCOMMENT NIL NODE)
    (EMITREMARK "Get offset in string to substring")
    (EMIT `(MOVE.W (REG ,LOC1 %%STRING-BASE-OFFSET) ,BASEPTR))
    (EMITREMARK "offset in substring to character")
    (EMIT `(ADD.W ,(WORDLOC LOC2) ,BASEPTR))    
    (EMITREMARK "point to head of string")
    (EMIT `(MOVE.L (REG ,LOC1 %%STRING-POINTER-OFFSET) ,PTR))
    (EMIT `(MOVE.B ,LOC3 (IDX.W ,BASEPTR ,PTR 0)))
    (YIELD NODE LOC3 FALL-THROUGH?)))   ; Careful about lifetimes!

(CPUT 'SET-STRING-ELT 'WANTREP '(POINTER MFIX CHAR))
(CPUT 'SET-STRING-ELT 'ISREP 'CHAR)
(CPUT 'SET-STRING-ELT 'TN-SCRIPT '((SPARE BASEPTR SCRATCH-REG ())
                                (SPARE PTR POINTER-REG ())
                                (SPARE BASEPTR)
                                (ARG 1)
                                (ARG 2)
                                (ARG 3)
                                (RESULT)))
(CPUT 'SET-STRING-ELT 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY) (ANY)))
(CPUT 'SET-STRING-ELT 'EFFECTS '(SET-STRING-ELT))
(CPUT 'SET-STRING-ELT 'AFFECTED '(STRING-TAIL!))

;;; (CHOPY string) copies a string header.

(DEFINE-GENERATOR CHOPY  (NODE FALL-THROUGH? LOC)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (ADJUST-TAG 'HP DEST '%%STRING-TAG)
    (EMIT '(LEA (REG HP 8) HP))
    (EMIT `(MOVE.L  (REG ,LOC  %%STRING-POINTER-OFFSET)
                    (REG ,DEST %%STRING-POINTER-OFFSET)))
    (EMIT `(MOVE.W  (REG ,LOC  %%STRING-LENGTH-OFFSET)
                    (REG ,DEST %%STRING-LENGTH-OFFSET)))
    (EMIT `(MOVE.W  (REG ,LOC  %%STRING-BASE-OFFSET)
                    (REG ,DEST %%STRING-BASE-OFFSET)))
    (YIELD NODE DEST FALL-THROUGH?)))  

(CPUT 'CHOPY 'TN-SCRIPT '((RESULT) (ARG 1)))
(CPUT 'CHOPY 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'CHOPY 'RESULT-RESTRICTION '(POINTER-REG))
(CPUT 'CHOPY 'EFFECTS '(CONS))

;;; (CHOPY! dest source) destructively copies a string header.

(DEFINE-GENERATOR CHOPY!  (NODE FALL-THROUGH? LOC1 LOC2)
  (EMITCOMMENT NIL NODE)
  (EMIT `(MOVE.L  (REG ,LOC2 %%STRING-POINTER-OFFSET)
                  (REG ,LOC1 %%STRING-POINTER-OFFSET)))
  (EMIT `(MOVE.W  (REG ,LOC2 %%STRING-LENGTH-OFFSET)
                  (REG ,LOC1 %%STRING-LENGTH-OFFSET)))
  (EMIT `(MOVE.W  (REG ,LOC2 %%STRING-BASE-OFFSET)
                  (REG ,LOC1 %%STRING-BASE-OFFSET)))
  (YIELD NODE LOC2 FALL-THROUGH?))  

(CPUT 'CHOPY! 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (POINTER-REG)))

;;; Destructively "chdr" (increment) a string header.

(DEFINE-GENERATOR STRING-TAIL!  (NODE FALL-THROUGH? LOC)
  (EMITCOMMENT NIL NODE)
  (EMIT `(SUBQ.W (LIT 1) (REG ,LOC %%STRING-LENGTH-OFFSET)))
  (EMIT `(ADDQ.W (LIT 1) (REG ,LOC %%STRING-BASE-OFFSET)))
  (YIELD NODE LOC FALL-THROUGH?))

(CPUT 'STRING-TAIL! 'TN-SCRIPT '((PREF (ARG 1) (RESULT))))
(CPUT 'STRING-TAIL! 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'STRING-TAIL! 'AFFECTED '(STRING-TAIL!))
(CPUT 'STRING-TAIL! 'EFFECTS '(STRING-TAIL!))

;;; (STRING-NTHTAIL! string index)  Do <index> destructive chdr's.

(DEFINE-GENERATOR STRING-NTHTAIL!  (NODE FALL-THROUGH? LOC1 LOC2)
  (EMITCOMMENT NIL NODE)
  (EMIT `(SUB.W ,LOC2 (REG ,LOC1 %%STRING-LENGTH-OFFSET)))
  (EMIT `(ADD.W ,LOC2 (REG ,LOC1 %%STRING-BASE-OFFSET)))
  (YIELD NODE LOC1 FALL-THROUGH?))

(CPUT 'STRING-NTHTAIL! 'WANTREP '(POINTER MFIX))
(CPUT 'STRING-NTHTAIL! 'TN-SCRIPT '((ARG 2) (PREF (ARG 1) (RESULT))))
(CPUT 'STRING-NTHTAIL! 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY)))
(CPUT 'STRING-NTHTAIL! 'AFFECTED '(STRING-TAIL!))
(CPUT 'STRING-NTHTAIL! 'EFFECTS '(STRING-TAIL!))

;;; Test a string for nullity.

(DEFINE-PREDICATE-GENERATOR STRING-EMPTY? (NODE JUMPTAG REVERSEP LOC)
  (EMIT `(TST.W (REG ,LOC %%STRING-LENGTH-OFFSET)))
  (BRANCHCOMMENT NODE REVERSEP)
  (EMITJ 'BLS JUMPTAG REVERSEP))

(CPUT 'STRING-EMPTY? 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT 'STRING-EMPTY? 'AFFECTED '(STRING-TAIL! SET-STRING-LENGTH))

;;; (EXTEND-ELT extend index): gets the index'th elt from an extend.
;;; An extremely dirty primitive.

(DEFINE-GENERATOR EXTEND-ELT (NODE FALL-THROUGH? LOC1 LOC2)
  (YIELD NODE
         `(IDX ,LOC2 ,LOC1 0)
         FALL-THROUGH?))
(CPUT 'EXTEND-ELT 'WANTREP '(POINTER MFIX*4))
(CPUT 'EXTEND-ELT 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (SCRATCH-REG)))
(CPUT 'EXTEND-ELT 'EFFECTS 'NONE)
(CPUT 'EXTEND-ELT 'AFFECTED '(SET-EXTEND-ELT))
(CPUT 'EXTEND-ELT 'SETTER
      (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-EXTEND-ELT T))

;;; (EXTEND-ELT-FIXED extend constant-index)
;;; Same as above, but second arg is known to be a constant fixnum,
;;; There should be another case of this, where offset is exactly 0.

(DEFINE-GENERATOR EXTEND-ELT-FIXED (NODE FALL-THROUGH? LOC1 LOC2)
  (IGNORE LOC2)
  (YIELD NODE (SREF LOC1 (FX* (CONSTANT-VALUE (NODE-FORM (CADR (CALL-ARGS (NODE-FORM NODE)))))
                              *POINTER*))
         FALL-THROUGH?))
(CPUT 'EXTEND-ELT-FIXED 'WANTREP '(POINTER NONE))
(CPUT 'EXTEND-ELT-FIXED 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY)))
(CPUT 'EXTEND-ELT-FIXED 'EFFECTS 'NONE)
(CPUT 'EXTEND-ELT-FIXED 'SETTER
      (LOCAL-LOOKUP *PRIMOP-NAMESPACE* 'SET-EXTEND-ELT-FIXED T))


;;; (SET-EXTEND-ELT extend index value): sets the index'the elt of an extend.

(DEFINE-GENERATOR SET-EXTEND-ELT (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (MOVE LOC3 `(IDX ,LOC2 ,LOC1 0))
  (YIELD NODE LOC3 FALL-THROUGH?))

(CPUT 'SET-EXTEND-ELT 'ARGUMENT-RESTRICTIONS
      '((POINTER-REG) (SCRATCH-REG) (ANY)))
(CPUT 'SET-EXTEND-ELT 'TN-SCRIPT '((ARG 1) (ARG 2) (PREF (ARG 3) (RESULT))))
(CPUT 'SET-EXTEND-ELT 'WANTREP '(POINTER MFIX*4 POINTER))
(CPUT 'SET-EXTEND-ELT 'EFFECTS '(SET-EXTEND-ELT))
(CPUT 'SET-EXTEND-ELT 'AFFECTED 'NONE)


;;; (SET-EXTEND-ELT-FIXED extend constant-index value)

(DEFINE-GENERATOR SET-EXTEND-ELT-FIXED (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
  (IGNORE LOC2)
  (MOVE LOC3 (SREF LOC1 (FX* (CONSTANT-VALUE (NODE-FORM (CADR (CALL-ARGS (NODE-FORM NODE)))))
                             *POINTER*)))
  (YIELD NODE LOC3 FALL-THROUGH?))

(CPUT 'SET-EXTEND-ELT-FIXED 'ARGUMENT-RESTRICTIONS
      '((POINTER-REG) (ANY) (ANY)))
(CPUT 'SET-EXTEND-ELT-FIXED 'TN-SCRIPT
      '((ARG 1) (PREF (ARG 3) (RESULT))))
(CPUT 'SET-EXTEND-ELT-FIXED 'WANTREP '(POINTER NONE POINTER))
(CPUT 'SET-EXTEND-ELT-FIXED 'EFFECTS '(SET-EXTEND-ELT))
(CPUT 'SET-EXTEND-ELT-FIXED 'AFFECTED 'NONE)


(DEFINE-GENERATOR %XLOC (NODE FALL-THROUGH? LOC1 LOC2)
  (YIELD NODE
         `(ADDR (IDX ,LOC2 ,LOC1 0))
         FALL-THROUGH?))
(CPUT '%XLOC 'WANTREP '(POINTER MFIX*4))
(CPUT '%XLOC 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (SCRATCH-REG)))
(CPUT '%XLOC 'EFFECTS 'NONE)
(CPUT '%XLOC 'AFFECTED 'NONE)


;;; This could be done as a fixed-selector but the side-effects info would be
;;; wrong.

(DEFINE-GENERATOR %LOC-CONTENTS (NODE FALL-THROUGH? LOC)
  (EMITCOMMENT NIL NODE)
  (YIELD NODE (INDIRECT LOC NIL) FALL-THROUGH?))

(CPUT '%LOC-CONTENTS 'ARGUMENT-RESTRICTIONS '((POINTER-REG)))
(CPUT '%LOC-CONTENTS 'AFFECTED 'ANY)
(CPUT '%LOC-CONTENTS 'EFFECTS 'NONE)

(DEFINE-GENERATOR %SET-LOC-CONTENTS (NODE FALL-THROUGH? LOC1 LOC2)
  (EMITCOMMENT NIL NODE)
  (MOVE LOC2 (INDIRECT LOC1 NIL))
  (YIELD NODE LOC2 FALL-THROUGH?))

(CPUT '%SET-LOC-CONTENTS 'ARGUMENT-RESTRICTIONS '((POINTER-REG) ()))
(CPUT '%SET-LOC-CONTENTS 'AFFECTED 'NONE)
(CPUT '%SET-LOC-CONTENTS 'EFFECTS 'ANY)

;;; BYTEV primops.

(DEFINE-MACRO (DEFINE-BYTEV-ELT-GENERATOR GETTER GET-REP SET-REP)
  (LET ((SETTER (SYMBOLCONC 'SET- GETTER))
        (GEN-SETFN (SYMBOLCONC 'SET- GETTER '$GENERATE)))
    `(PROGN 'COMPILE
          (DEFINE (,GEN-SETFN NODE FALL-THROUGH? LOC1 LOC2 LOC3)
            (XMOVE (ADJUST-LOC NIL ',GET-REP LOC3)
                   `(IDX ,LOC2 ,LOC1 0) ',GET-REP 1)
            (YIELD NODE LOC3 FALL-THROUGH?))
             
          (*DEFINE-BYTEV-ELT-GEN
           ',GETTER ',SETTER ,GEN-SETFN ',GET-REP ',SET-REP))))

(DEFINE (GEN-BYTEV-ELT NODE FALL-THROUGH? LOC1 LOC2)
  (YIELD NODE `(IDX ,LOC2 ,LOC1 0) FALL-THROUGH?))

(DEFINE (*DEFINE-BYTEV-ELT-GEN GETTER SETTER GEN-SETFN GET-REP SET-REP)
  (CPUT GETTER 'WANTREP '(POINTER MFIX))
  (CPUT GETTER 'ISREP GET-REP)
  (CPUT GETTER 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (SCRATCH-REG)))
  (CPUT GETTER 'EFFECTS 'NONE)
  (CPUT GETTER 'AFFECTED `(,SETTER))
  (CPUT GETTER 'SETTER (LOCAL-LOOKUP *PRIMOP-NAMESPACE* SETTER T))
  (CPUT GETTER 'PRIMOP T)
  (CPUT GETTER 'PRIMOP-NUMBER-OF-ARGS '(2 . 2))
  (CPUT GETTER 'PRIMOP-GENERATE GEN-BYTEV-ELT)

  (CPUT SETTER 'WANTREP `(POINTER MFIX ,SET-REP))
  (CPUT SETTER 'ISREP SET-REP)
  (CPUT SETTER 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (SCRATCH-REG) (ANY)))
  (CPUT SETTER 'TN-SCRIPT '((ARG 1) (ARG 2) (PREF (ARG 3) (RESULT))))
  (CPUT SETTER 'EFFECTS `(,SETTER))
  (CPUT SETTER 'AFFECTED 'NONE)
  (CPUT SETTER 'PRIMOP T)
  (CPUT SETTER 'PRIMOP-NUMBER-OF-ARGS '(3 . 3))
  (CPUT SETTER 'PRIMOP-GENERATE GEN-SETFN))
            
(DEFINE-BYTEV-ELT-GENERATOR BYTEV-ELT-8       BYTE     MFIX)
(DEFINE-BYTEV-ELT-GENERATOR BYTEV-ELT-16      HALFPESO MFIX)
(DEFINE-BYTEV-ELT-GENERATOR BYTEV-ELT-32      MFIX     MFIX)
(DEFINE-BYTEV-ELT-GENERATOR BYTEV-ELT-POINTER POINTER  POINTER)

;;; Arithmetic shifting routines
(DEFINE-CONSTANT *ASH-TN-SCRIPT*
  '((ARG 1) (SPARE TMP SCRATCH-REG ())
    (ARG 2) (SPARE COUNT SCRATCH-REG ()) (SPARE TMP)))

(DEFINE-GENERATOR FIXNUM-ASHR (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((COUNT (TNLOC (GETSPARETN NODE 'COUNT)))
        (TMP  (TNLOC (GETSPARETN NODE 'TMP))))
    (MOVE LOC1 TMP)
    (EMIT `(ASR.L
            ,(COND ((OR (SCRATCH-REG? LOC2) (QUICK-LIT? LOC2)) LOC2)
                   (ELSE (MOVE LOC2 COUNT)))
            ,TMP))
    (EMIT `(ANDI.B (LIT #b11111000) ,TMP))
    (YIELD NODE (MOVE TMP (ISTNLOC NODE)) FALL-THROUGH?)))
(CPUT 'FIXNUM-ASHR 'WANTREP '(POINTER MFIX))
(CPUT 'FIXNUM-ASHR 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY)))
(CPUT 'FIXNUM-ASHR 'TN-SCRIPT *ASH-TN-SCRIPT*)
(CPUT 'FIXNUM-ASHR 'EFFECTS 'NONE)
(CPUT 'FIXNUM-ASHR 'AFFECTED 'NONE)

(DEFINE-GENERATOR POINTER-ASHR (NODE FALL-THROUGH? LOC1 LOC2)
  (GEN-POINTER-ASH 'R NODE FALL-THROUGH? LOC1 LOC2))
(CPUT 'POINTER-ASHR 'WANTREP '(POINTER MFIX))
(CPUT 'POINTER-ASHR 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY)))
(CPUT 'POINTER-ASHR 'TN-SCRIPT *ASH-TN-SCRIPT*)
(CPUT 'POINTER-ASHR 'EFFECTS 'NONE)
(CPUT 'POINTER-ASHR 'AFFECTED 'NONE)

(DEFINE-GENERATOR POINTER-ASHL (NODE FALL-THROUGH? LOC1 LOC2)
  (GEN-POINTER-ASH 'L NODE FALL-THROUGH? LOC1 LOC2))
(CPUT 'POINTER-ASHL 'WANTREP '(POINTER MFIX))
(CPUT 'POINTER-ASHL 'ARGUMENT-RESTRICTIONS '((POINTER-REG) (ANY)))
(CPUT 'POINTER-ASHL 'TN-SCRIPT *ASH-TN-SCRIPT*)
(CPUT 'POINTER-ASHL 'EFFECTS 'NONE)
(CPUT 'POINTER-ASHL 'AFFECTED 'NONE)

(DEFINE (GEN-POINTER-ASH R-OR-L NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((COUNT (TNLOC (GETSPARETN NODE 'COUNT)))
        (TMP  (TNLOC (GETSPARETN NODE 'TMP))))
    (MOVE LOC1 TMP)
    (EMIT `(,(SYMBOLCONC 'AS R-OR-L '\.L)
            ,(COND ((OR (SCRATCH-REG? LOC2) (QUICK-LIT? LOC2)) LOC2)
                   (ELSE (MOVE LOC2 COUNT)))
            ,TMP))
    (YIELD NODE (MOVE TMP (ISTNLOC NODE)) FALL-THROUGH?)))

;;; Consing and stuff like that:

;;; Here's your basic CONS.

(DEFINE-GENERATOR CONS (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (ADJUST-TAG 'HP DEST '%%PAIR-TAG)
    (EMIT '(LEA (REG HP 8) HP))
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
    (LET ((NEWDEST (ADJUST-TAG 'HP DEST '%%PAIR-TAG)))
      (EMIT '(LEA (REG HP 8) HP))
      (YIELD NODE NEWDEST FALL-THROUGH?))))
(CPUT 'NEW-CELL 'EFFECTS '(CONS))
(CPUT 'NEW-CELL 'AFFECTED 'NONE)

(DEFINE-GENERATOR MAKE-EXTEND (NODE FALL-THROUGH? LOC1 LOC2)
  (LET ((DEST (ISTNLOC NODE)))
    (EMITCOMMENT NIL NODE)
    (MOVE LOC1 '(REG HP (~ %%HEAP-TAG)))
    (ADJUST-TAG 'HP DEST '%%EXTEND-TAG)
    (EMITREMARK "Bump heap pointer")
    (EMIT `(ADDA.L ,LOC2 HP))
    (YIELD NODE DEST FALL-THROUGH?)))
(CPUT 'MAKE-EXTEND 'TN-SCRIPT '((ARG 1) (RESULT) (ARG 2)))
(CPUT 'MAKE-EXTEND 'RESULT-RESTRICTION '(POINTER-REG))


;;; Low-level, dirty, grungy stuff.


(DEFINE-GENERATOR POINTER-BIT-FIELD (NODE FALL-THROUGH? LOC1 LOC2 LOC3)
                                        ; base pos size
  (LET ((DEST (ISTNLOC NODE))
        (TMP (TNLOC (GETSPARETN NODE 'TMP))))
    (EMITCOMMENT "Attention:" NODE)
    (COND ((AND (LITERAL? LOC2) (LITERAL? LOC3))
           (MOVE LOC1 DEST)
           (EMIT `(ANDI.L (LIT (<< (- (<< 1 ,(CADR LOC3)) 1)
                                   ,(CADR LOC2)))
                          ,DEST))
           (COND ((FX<= (CADR LOC2) 8)
                  (EMIT `(LSR.L ,LOC2 ,DEST)))
                 (ELSE (MOVE LOC2 TMP)
                       (EMIT `(LSR.L ,TMP ,DEST))))
           DEST)
          (ELSE (BUG "unimplemented variable POINTER-BIT-FIELD"
                     "generate no code and hope someone writes the code needed")
                DEST))
    (YIELD NODE DEST FALL-THROUGH?)))

(CPUT 'POINTER-BIT-FIELD 'TN-SCRIPT
      '((ARG 1) (RESULT) (ARG 2) (SPARE TMP SCRATCH-REG ()) (RESULT)))
(CPUT 'POINTER-BIT-FIELD 'ISREP 'MFIX)
(CPUT 'POINTER-BIT-FIELD 'WANTREP '(POINTER MFIX MFIX))
(CPUT 'POINTER-BIT-FIELD 'RESULT-RESTRICTION '(SCRATCH-REG))
(CPUT 'POINTER-BIT-FIELD 'AFFECTED 'NONE)
(CPUT 'POINTER-BIT-FILED 'EFFECTS 'NONE)


(DEFINE-GENERATOR POINTER-TAG (NODE FALL-THROUGH? SOURCE)
  (LET ((DEST (ISTNLOC NODE))
        (TMP (TNLOC (GETSPARETN NODE 'TMP))))
    (EMITCOMMENT NIL NODE)
    (YIELD NODE
           (COND ((POINTER-REG? DEST)
                  (MOVE SOURCE TMP)
                  (EMIT `(ANDI.L (LIT 7) ,TMP))
                  (MOVE TMP DEST))
                 (ELSE
                  (LET ((NEWDEST (MOVE SOURCE DEST)))
                    (EMIT `(ANDI.L (LIT 7) ,NEWDEST))
                    NEWDEST)))
           FALL-THROUGH?)))
(CPUT 'POINTER-TAG 'TN-SCRIPT '((ARG 1) (SPARE TMP SCRATCH-REG ()) (RESULT)))
(CPUT 'POINTER-TAG 'ISREP 'MFIX)
(CPUT 'POINTER-TAG 'EFFECTED 'NONE)
(CPUT 'POINTER-TAG 'AFFECTS 'NONE)

(DEFINE-GENERATOR POINTER-ADDRESS (NODE FALL-THROUGH? SOURCE)
  (LET ((DEST (ISTNLOC NODE))
        (TMP (TNLOC (GETSPARETN NODE 'TMP))))
    (EMITCOMMENT NIL NODE)
    (MOVE '(SLINK ADDRESS-MASK) 'D7)
    (YIELD NODE
           (COND ((POINTER-REG? DEST)
                  (MOVE SOURCE TMP)
                  (EMIT `(AND.L D7 ,TMP))
                  (MOVE TMP DEST))
                 (ELSE
                  (LET ((NEWDEST (MOVE SOURCE DEST)))
                    (EMIT `(AND.L D7 ,NEWDEST))
                    NEWDEST)))     
           FALL-THROUGH?)))
(CPUT 'POINTER-ADDRESS
      'TN-SCRIPT '((ARG 1) (SPARE TMP SCRATCH-REG ()) (RESULT)))
(CPUT 'POINTER-ADDRESS 'ISREP 'SWFIX)
(CPUT 'POINTER-ADDRESS 'EFFECTED 'NONE)
(CPUT 'POINTER-ADDRESS 'AFFECTS 'NONE)

(DEFINE-GENERATOR CHAR->POINTER (NODE FALL-THROUGH? LOC1)
  (YIELD NODE LOC1 FALL-THROUGH?))
(CPUT 'CHAR->POINTER 'WANTREP '(CHAR))
(CPUT 'CHAR->POINTER 'AFFECTED 'NONE)
(CPUT 'CHAR->POINTER 'EFFECTS 'NONE)

(DEFINE-GENERATOR POINTER->CHAR (NODE FALL-THROUGH? LOC1)
  (YIELD NODE (ADJUST-LOC NIL 'CHAR LOC1) FALL-THROUGH?))
(CPUT 'POINTER->CHAR 'ISREP 'CHAR)
(CPUT 'POINTER->CHAR 'AFFECTED 'NONE)
(CPUT 'POINTER->CHAR 'EFFECTS 'NONE)
