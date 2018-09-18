(HERALD (TCOMP VMSEMIT)
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;; Generate proper syntax for the VMS assembler (also known as "MACRO").

;;; VAXGEN patches - replace existing definitions - lose lose.

(DEFINE-PREDICATE-GENERATOR FIXNUM-BIT? (NODE JUMPTAG REVERSEP LOC1 LOC2)
  (BRANCHCOMMENT NODE REVERSEP)
  (GEN-JBS LOC2 JUMPTAG REVERSEP LOC1))

(DEFINE (EMITJ JOP JDEST REVERSEP)
  (LET ((Z (IF REVERSEP (GET JOP 'REVERSED-JOP) JOP)))
    (COND (*LISP-ASSEMBLY-SYNTAX?*
           (EMIT `(,Z ,JDEST)))
          ((AND (EQ? *ASSEMBLER-TYPE* 'VAX-VMS)
                (GET Z 'LOSE))
           (LET ((TAG (GENTAG 'LOSE)))
             (EMIT `(,(GET Z 'LOSE) ,TAG))
             (EMIT `(BRW ,JDEST))
             (EMITTAG TAG)))
          (ELSE
           (EMIT-OPCODE Z)
           (EMIT-OFFSET JDEST NIL)
           (SHOVECOMMENT T)))))

;;; Non-patches.  This should parallel UNEMIT.T.

(DEFINE (BEGIN-ASSEMBLY-FILE-1 ID)
  (EMIT `(\.TITLE ,ID)))                ; ?

(DEFINE (END-ASSEMBLY-FILE)             ; this becomes FINISH-ASSEMBLY-FILE !!
  (EMIT '(\.END)))

(DEFINE (TEXT-SECTION)
  (COND ((NEQ? *ASSEMBLY-SECTION* 'TEXT)
         (SET *ASSEMBLY-SECTION* 'TEXT)
         (FLUSHTAG)
         (FORMAT *ASSEMBLY-OUTPUT*
                 (IF *PRE-COOK?*
                     "~& .PSECT T$ABS,NOWRT,EXE,QUAD"
                     "~& .PSECT T$PURE,NOWRT,EXE,PIC,QUAD"))
         (SHOVECOMMENT T))))

(DEFINE (DATA-SECTION)
  (FLUSHTAG)
  (COND ((NEQ? *ASSEMBLY-SECTION* 'DATA)
         (SET *ASSEMBLY-SECTION* 'DATA)
         (FORMAT *ASSEMBLY-OUTPUT* "~& .PSECT T$DATA,WRT,QUAD")
         (SHOVECOMMENT T)
         (EMIT-LINKAGE-AREA))))

(DEFINE (EMIT-OFFSET X PARENTHESIZE?)
  ;; X is an assembler expression, like (+ FOO 2) or whatever
  (COND ((NULL? X)
         (IF PARENTHESIZE? (BUG "expression is () in EMIT-OFFSET" "none")))
        ((INTEGER? X)
         (COND ((FX< X 10.)
                (PRIN1 X *ASSEMBLY-OUTPUT*))
               (ELSE
                (WRITES *ASSEMBLY-OUTPUT* "^x")
                (RADIX-16-PRINT X *ASSEMBLY-OUTPUT*))))
        ((STRING? X) (WRITES *ASSEMBLY-OUTPUT* X))
        ((SYMBOL? X)
         (COND ((GET X 'SYM) (EMIT-OFFSET (GET X 'SYM) PARENTHESIZE?))
               (ELSE (EMIT-SYLLABLE X))))
        ((FLONUM? X)
         (FORMAT *ASSEMBLY-OUTPUT* "~S" X))
        ((NOT (PAIR? X)) (BUG))
        (ELSE
         (XCASE (CAR X)
                ((~)
                 (WRITEC *ASSEMBLY-OUTPUT* #\-)
                 (EMIT-OFFSET (CADR X) T))
                ((+ - * /)
                 (COND ((NULL? (CDDR X))
                        (EMIT-OFFSET (CADR X) NIL))
                       (ELSE
                        (IF PARENTHESIZE? (WRITEC *ASSEMBLY-OUTPUT* #\<))
                        (EMIT-OFFSET (CADR X) T)
                        (DO ((Z (CDDR X) (CDR Z)))
                            ((NULL? Z))
                          (DISPLAY (CAR X) *ASSEMBLY-OUTPUT*)
                          (EMIT-OFFSET (CAR Z) T))
                        (IF PARENTHESIZE? (WRITEC *ASSEMBLY-OUTPUT* #\>)))))
                ((<<)
                 (IF PARENTHESIZE? (WRITEC *ASSEMBLY-OUTPUT* #\<))
                 (EMIT-OFFSET (CADR X) T)
                 (WRITEC *ASSEMBLY-OUTPUT* #\@)
                 (EMIT-OFFSET (CADDR X) T)
                 (IF PARENTHESIZE? (WRITEC *ASSEMBLY-OUTPUT* #\>)))
                ((LABEL) (OUTPUT-TAG (CDR X)))
                ((COMMA)
                 (EMIT-OFFSET (EVAL (CADR X) *TC-ENV*) PARENTHESIZE?))
                ((VERBATIM)
                 (DISPLAY (CADR X) *ASSEMBLY-OUTPUT*))
                ))))

(DEFUN (DEF LAP-MAGIC) (X)
  (DESTRUCTURE (((() SYM VAL) X))
    (FRESH-LINE *ASSEMBLY-OUTPUT*)
    (SET-HPOS  *ASSEMBLY-OUTPUT* (OR *TAB-STOP-INTERVAL* 1))
    (EMIT-OFFSET SYM NIL)
    (WRITEC *ASSEMBLY-OUTPUT* #\=)
    (EMIT-OFFSET VAL NIL)
    (SHOVECOMMENT T)))

(DEFUN (SPACE LAP-MAGIC) (X)
  (EMIT `(DEF \. (+ \. ,(CADR X)))))

;;; JFP's version

(define (make-special-tag prefix symbol)
    (let* ((tag (symbolconc prefix '$ symbol))
           (z   (string-length (symbol-pname tag))))
      (cond ((fx> z 22)
             (let* ((foo (symbol->string tag))
                    (bar (string-slice foo 0 13))
                    (bas (string-nthtail foo (fx- z 9))))
               (string->symbol (string-append bar bas))))
            (else tag))))


;(DEFINE (MAKE-SPECIAL-TAG PREFIX SYMBOL)
;  (LET ((TAG (SYMBOLCONC PREFIX '$ SYMBOL)))
;    (LET* ((TAGSTRING (SYMBOL-PNAME TAG))
;           (Z (STRING-LENGTH TAGSTRING)))
;      (COND ((FX> Z 23)
;             ;; (LET ((FOO (EXPLODEN TAG)))
;             ;;   (LET ((BAR (+NTHCDR FOO 13)))
;             ;;     (RPLACD BAR (+NTHCDR BAR (- Z 22))))
;             ;;   (MAKNAM FOO))
;             (SYMBOLCONC (STRING-SLICE TAGSTRING 0 14)
;                         (STRING-SLICE TAGSTRING (FX- Z 9) 9))
;            (ELSE TAG))))))

;;; Whomever calls this must make sure that it goes in the right section
;;; (probably DATA), and that the section gets restored if necessary.

(DEFINE (OUTPUT-STRING-HEADER TAG TEXTTAG STRING)
  (OR (FX= %%STRING-TAG 2) (BUG "something's wrong with %%STRING-TAG" "none"))
  (EMITREMARK "String")
  (EMIT `(HALFPESO ,(STRING-LENGTH STRING)))    ;Length
  (EMITTAG TAG)
  (COND (*PRE-COOK?*                    ;Text pointer
         (EMIT `(ADDRESS ,TEXTTAG)))
        (ELSE
         (EMIT `(LONG (- ,TEXTTAG ,*THE-CODE-TAG*)))))
  (EMIT '(HALFPESO 0))          ;Index (base)
  (IF (EQ? *ASSEMBLY-SECTION* 'DATA) (SET *OFFSET* (FX+ *OFFSET* 2))))

(DEFINE (OUTPUT-ASCIZ S)
  (EMIT-OPCODE '\.ASCIZ)
  (WRITEC *ASSEMBLY-OUTPUT* *STRING-CHARACTER*)
  (WALK-STRING (LAMBDA (C)
                 (COND ((OR (CHAR< C #\SPACE)
                            (CHAR= C #\RUBOUT)
                            (CHAR= C *STRING-CHARACTER*))
                        (FORMAT *ASSEMBLY-OUTPUT*
                                "~C<~S>~C"
                                *STRING-CHARACTER*
				(CHAR->ASCII C)
                                *STRING-CHARACTER*))
                       (ELSE
                        (WRITEC *ASSEMBLY-OUTPUT* C))))
               S)
  (WRITEC *ASSEMBLY-OUTPUT* *STRING-CHARACTER*))

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

;;; General OP-ALIASes

(WALK (LAMBDA (X)
        (DESTRUCTURE (((OP V) X))
          (PUT OP 'OP-ALIAS V)))
      '((BYTE     \.BYTE)
        (WORD     \.WORD)
        (HALFPESO \.WORD)
        (LONG     \.LONG)
        (PESO     \.LONG)
        (SPACE    \.SPACE) ; unix only
        (ALIGN    \.ALIGN)
        (GLOBL    \.GLOBL)
        (EXTERNAL \.GLOBL)
        (DATA     \.DATA)  ; unix only
        (TEXT     \.TEXT)  ; unix only
        (ALIGN    \.ALIGN)
        (ADDRESS  \.ADDRESS)
        ))
