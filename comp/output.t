(HERALD OUTPUT                          ; -*- Mode:T; System:TCOMP -*-
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;; EMIT: put out an instruction or pseudo-op.

(STAT-COUNTER *EMIT-COUNT* "LAP instructions emitted")

(DEFINE (EMIT X)
  (COND (*LISP-ASSEMBLY-SYNTAX?* 
         (SET-HPOS *ASSEMBLY-OUTPUT* (OR *TAB-STOP-INTERVAL* 1))
         (PRINT X *ASSEMBLY-OUTPUT*))
        (ELSE
         (LET ((MAGIC (GET (CAR X) 'LAP-MAGIC)))
           (COND (MAGIC (MAGIC X))
                 (ELSE (REALLY-EMIT X)))))))

(DEFINE (REALLY-EMIT X)
  (EMIT-OPCODE (CAR X))
  (COND ((CDR X)
         (EMIT-OPERAND (CADR X))
         (DO ((L (CDDR X) (CDR L)))
             ((NULL? L))
           (WRITEC *ASSEMBLY-OUTPUT* #\,)
           (EMIT-OPERAND (CAR L)))))
  (INCR *EMIT-COUNT*)
  (SHOVECOMMENT T))

;;; EMITJ - emit a jump instruction.  If REVERSEP is non-nil, the sense of the
;;; jump is reversed.

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

;;; Special hack deals with tags of the form (LABEL . N).

(DEFINE (EMITTAG X)
  (FRESH-LINE *ASSEMBLY-OUTPUT*)
  (COND (*LISP-ASSEMBLY-SYNTAX?*
         (DISPLAY X *ASSEMBLY-OUTPUT*)
         (WRITEC *ASSEMBLY-OUTPUT* #\SPACE))
        (ELSE
         (COND ((SYMBOL? X) (EMIT-SYLLABLE X))
               ((PAIR? X)
                (OUTPUT-TAG (CDR X)))
               (ELSE (BUG "bogus tag ~S in EMITTAG" "will ignore it" X)))
         (WRITEC *ASSEMBLY-OUTPUT* #\:))))

(DEFINE (OUTPUT-TAG N)
  (WRITEC *ASSEMBLY-OUTPUT*
          (CHAR+ (FIXNUM-REMAINDER N 26) #\A))  ; Accomodate Apollo asmblr.
  (PRINT N *ASSEMBLY-OUTPUT*))

;;; Flush any pending label.

(DEFINE (FLUSHTAG)
  (FRESH-LINE *ASSEMBLY-OUTPUT*))

(LSET *DELAYED-COMMENT-TEXT* NIL)
(LSET *DELAYED-COMMENT-NODE* NIL)

;;; Call EMITCOMMENT to comment the *next* instruction or tag to be output.
;;; A delaying tactic causes it to come out at the right time.

(DEFINE (EMITCOMMENT TEXT NODE)
  (SHOVECOMMENT NIL)
  (SETQ *DELAYED-COMMENT-TEXT* TEXT)
  (SETQ *DELAYED-COMMENT-NODE* NODE))

(DEFINE (EMITREMARK TEXT) (EMITCOMMENT TEXT NIL))

(DEFINE (SHOVECOMMENT FOO?)
  (COND ((AND *ENABLE-LAP-COMMENTARY?*
              (OR *DELAYED-COMMENT-TEXT* *DELAYED-COMMENT-NODE*))
         (OUTPUT-COMMENT *DELAYED-COMMENT-TEXT* *DELAYED-COMMENT-NODE* FOO?)))
  (SETQ *DELAYED-COMMENT-TEXT* NIL)
  (SETQ *DELAYED-COMMENT-NODE* NIL)
  '*INTERNAL-SHOVECOMMENT-VALUE-FALL-THROUGH-BUG*)

;;; The FOO? parameter is true iff it's okay to put the comment on the same
;;; line as whatever's there.  (IBM-style assemblers only - i.e. 68000.)

(DEFINE (OUTPUT-COMMENT TEXT NODE FOO?)
  (COND (*LISP-ASSEMBLY-SYNTAX?*
         (SET-HPOS *ASSEMBLY-OUTPUT* 48))
        ((EQ? FOO? T) (IF (AND (EQ? *ASSEMBLER-TYPE* '68000-APOLLO)
                               (NOT *LISP-ASSEMBLY-SYNTAX?*))
                          (COMMENT-TAB-TO 32)
                        (SET-HPOS *ASSEMBLY-OUTPUT* 32)))
        (ELSE (FRESH-LINE *ASSEMBLY-OUTPUT*)))
  (WRITEC *ASSEMBLY-OUTPUT*
          (IF *LISP-ASSEMBLY-SYNTAX?* #\SEMICOLON *COMMENT-START-CHARACTER*))
  (WRITEC *ASSEMBLY-OUTPUT* #\SPACE)
  (COND (TEXT
         (WRITES *ASSEMBLY-OUTPUT* TEXT)
         (AND NODE (WRITEC *ASSEMBLY-OUTPUT* #\SPACE))))
  (IF NODE
      (PRINT-ONE-LINE (SEXPRFY1 NODE 3) *ASSEMBLY-OUTPUT*))
  (NEWLINE *ASSEMBLY-OUTPUT*))

(DEFINE (COMMENT-TAB-TO N)
  (COND ((FX>= (HPOS *ASSEMBLY-OUTPUT*) N)
         (NEWLINE *ASSEMBLY-OUTPUT*)
         (WRITEC *ASSEMBLY-OUTPUT* *COMMENT-START-CHARACTER*)))
  (SET-HPOS *ASSEMBLY-OUTPUT* N))

(DEFINE (EMIT-REG-NAME-WITH-PARENS R)
  (WRITEC *ASSEMBLY-OUTPUT* #\LEFT-PAREN)
  (EMIT-OFFSET R NIL)
  (WRITEC *ASSEMBLY-OUTPUT* #\RIGHT-PAREN))

(DEFINE (OUTPUT-SYSBUILD-ITEM ITEM)
  (COND ((NOT *DOING-SYSGEN?*)
         (FORMAT *SUPPORT-OUTPUT* "~&~S~%"
                 `(SYSBUILD-ITEM . ,(MAKE-PRINTABLE-STRUCTURE ITEM
                                                              1000000.))))))
