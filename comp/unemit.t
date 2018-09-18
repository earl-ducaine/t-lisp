(herald (tcomp unemit t 17)
        (env tcomp))


(DEFINE (END-ASSEMBLY-FILE) T)          ; No END required

(DEFINE (EMIT-OFFSET X PARENTHESIZE?)
  ;; X is an assembler expression, like (+ FOO 2) or whatever
  (COND ((NULL? X)
         (IF PARENTHESIZE? (BUG "expression is () in EMIT-OFFSET" "none")))
        ((INTEGER? X)
         (COND ((< X 10.)
                (PRIN1 X *ASSEMBLY-OUTPUT*))
               (T
                (WRITES *ASSEMBLY-OUTPUT* "0x")
                (RADIX-16-PRINT X *ASSEMBLY-OUTPUT*))))
        ((FLONUM? X)
         (FORMAT *ASSEMBLY-OUTPUT* "0d~S" X))
        ((STRING? X) (WRITES *ASSEMBLY-OUTPUT* X))
        ((SYMBOL? X)
         (COND ((GET X 'SYM) (EMIT-OFFSET (GET X 'SYM) PARENTHESIZE?))
               (T (EMIT-SYLLABLE X))))
        ((NOT (PAIR? X))
         (BUG "~S unassemblable offset" "will emit a 0 instead" X)
         (EMIT-OFFSET 0 NIL))
        (T (CASE (CAR X)
             ((~)
              (WRITEC *ASSEMBLY-OUTPUT* #\-)
              (EMIT-OFFSET (CADR X) T))
             ((+ - * / << >>)
              (COND ((NULL? (CDDR X))
                     (EMIT-OFFSET (CADR X) NIL))
                    (T
                     (IF PARENTHESIZE? (WRITEC *ASSEMBLY-OUTPUT* #\())
                     (EMIT-OFFSET (CADR X) T)
                     (DO ((Z (CDDR X) (CDR Z)))
                         ((NULL? Z))
                       (DISPLAY (CAR X) *ASSEMBLY-OUTPUT*)
                       (EMIT-OFFSET (CAR Z) T))
                     (IF PARENTHESIZE? (WRITEC *ASSEMBLY-OUTPUT* #\))))))
             ((LABEL) (OUTPUT-TAG (CDR X)))
             ((COMMA)
              (EMIT-OFFSET (EVAL (CADR X) *TC-ENV*) PARENTHESIZE?))
             ((VERBATIM)
              (DISPLAY (CADR X) *ASSEMBLY-OUTPUT*))
             (T
              (BUG "~S unassemblable operand" "will emit a 0 instead" X)
              (EMIT-OFFSET 0 NIL))
             ))))

(DEFUN (DEF LAP-MAGIC) (X)
  (DESTRUCTURE (((() SYM VAL) X))
    (EMIT `(\.SET ,SYM (REL ,VAL)))))

(DEFINE (BEGIN-ASSEMBLY-FILE-1 ID) (IGNORE ID) T)   

(DEFINE (MAKE-SPECIAL-TAG PREFIX SYMBOL)
  (CONCATENATE-SYMBOL PREFIX '_ SYMBOL))

;;; Whoever calls this must make sure that it goes in the right section
;;; (probably DATA), and that the section gets restored if necessary.

(DEFINE (OUTPUT-STRING-HEADER TAG TEXTTAG STRING)
  (IF (NOT (FX= TARGET:%%STRING-TAG 2))
      (BUG "something's wrong with %%STRING-TAG" "none"))
  (EMITREMARK "String")
  (EMIT `(HALFPESO ,(STRING-LENGTH STRING)))    ;Length
  (EMITTAG TAG)
  (EMIT `(ADDRESS ,TEXTTAG))            ;Text pointer
  (EMIT '(HALFPESO 0))          ;Index (base)
  (IF (EQ? *ASSEMBLY-SECTION* 'DATA)
      (SETQ *OFFSET* (+ *OFFSET* 2))))

(DEFINE (OUTPUT-ASCIZ S)
  (EMIT-OPCODE '\.ASCIZ)
  (WRITEC *ASSEMBLY-OUTPUT* *STRING-CHARACTER*)
  (WALK-STRING (LAMBDA (C)
                 (COND ((OR (CHAR< C #\SPACE) (CHAR= C #\RUBOUT))
                        (FORMAT *ASSEMBLY-OUTPUT* "\\0~2O" (CHAR->ASCII C)))
                       ((OR (CHAR= C *STRING-CHARACTER*) (CHAR= C #\\))
                        (WRITEC *ASSEMBLY-OUTPUT* #\\)
                        (WRITEC *ASSEMBLY-OUTPUT* C))
                       (T
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
                               (FORMAT *ASSEMBLY-OUTPUT* "~S_"
                                       (CHAR->ASCII C)))))
                      (SYMBOL-PNAME R)))))

;; General OP-ALIASes

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
        (ADDRESS  \.LONG)
        ))
