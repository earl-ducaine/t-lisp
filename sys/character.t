(HERALD (TSYS CHARACTER T 66)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Character-related routines

;;; Very gross names. Think of better ones.
;;; Figure out what to do about integrability.  These shouldn't be integrable
;;; if CHAR-UPCASE isn't integrable.

(DEFINE (CHAR=IC C1 C2)
  (CHAR= (CHAR-UPCASE C1) (CHAR-UPCASE C2)))

(DEFINE (CHAR<IC C1 C2)
  (CHAR< (CHAR-UPCASE C1) (CHAR-UPCASE C2)))

(DEFINE (CHAR>IC C1 C2)
  (CHAR> (CHAR-UPCASE C1) (CHAR-UPCASE C2)))

(DEFINE (CHARN=IC C1 C2)
  (CHARN= (CHAR-UPCASE C1) (CHAR-UPCASE C2)))

(DEFINE (CHAR>=IC C1 C2)
  (CHAR>= (CHAR-UPCASE C1) (CHAR-UPCASE C2)))

(DEFINE (CHAR<=IC C1 C2)
  (CHAR<= (CHAR-UPCASE C1) (CHAR-UPCASE C2)))

(DEFINE-INTEGRABLE (LOWERCASE? C)
  (AND (CHAR>= C #\a) (CHAR<= C #\z)))

(DEFINE-INTEGRABLE (UPPERCASE? C)
  (AND (CHAR>= C #\A) (CHAR<= C #\Z)))

(DEFINE (ALPHABETIC? C)                 ; open-code someday
  (LET ((C (CHECK-ARG CHAR? C ALPHABETIC?)))
    (OR (LOWERCASE? C) (UPPERCASE? C))))

(DEFINE (CHAR-UPCASE C)                 ; open-code someday
  (%CHAR-UPCASE (CHECK-ARG CHAR? C CHAR-UPCASE)))

(DEFINE (%CHAR-UPCASE C)                ; ugh.  for READ.
  (IF (LOWERCASE? C) (CHAR- C #o40) C))

(DEFINE (CHAR-DOWNCASE C)               ; open-code someday
  (LET ((C (CHECK-ARG CHAR? C CHAR-DOWNCASE)))
    (IF (UPPERCASE? C) (CHAR+ C #o40) C)))

(DEFINE (GRAPHIC? C)                    ; open-code someday
  (LET ((C (CHECK-ARG CHAR? C GRAPHIC?)))
    (AND (CHAR>= C #\SPACE) (CHAR< C #\RUBOUT))))

(DEFINE (CONTROL? C)                    ; open-code someday
  (LET ((C (CHECK-ARG CHAR? C CONTROL?)))
    (OR (CHAR< C #\SPACE) (CHAR= C #\RUBOUT))))

(DEFINE (CONTROLIFY C)                  ; open-code someday
  (LET ((C (CHECK-ARG CHAR? C CONTROLIFY)))
    (ASCII->CHAR (POINTER-LOGAND C #o37))))

(DEFINE (UNCONTROLIFY C)                ; open-code someday
  (LET ((C (CHECK-ARG CHAR? C UNCONTROLIFY)))
    (CHAR+ C #o100)))

(DEFINE (ACCEPTABLE-RADIX? RADIX)
  (AND (FIXNUM? RADIX) (FX> RADIX 0) (FX<= RADIX 36)))

(DEFINE (DIGIT C RADIX)
  (%DIGIT (CHECK-ARG CHAR?             C     DIGIT)
          (CHECK-ARG ACCEPTABLE-RADIX? RADIX DIGIT)))

(DEFINE (%DIGIT C RADIX)                ; for entry from reader.
  (COND ((FX<= RADIX 10.)
         (COND ((AND (CHAR< C (CHAR+ #\0 RADIX))
                     (CHAR>= C #\0))
                (CHAR- C #\0))
               (ELSE NIL)))
        ((AND (CHAR<= C #\9)
              (CHAR>= C #\0))
         (CHAR- C #\0))
        (ELSE
         (LET ((CC (CHAR-UPCASE C)))
           (COND ((AND (CHAR>= CC #\A) (CHAR< CC (CHAR+ #\A (FX- RADIX 10.))))
                  (FX+ 10. (CHAR- CC #\A)))
                 (ELSE NIL))))))

(DEFINE %DIGIT? %DIGIT)

(DEFINE (DIGIT? C RADIX) (TRUE? (DIGIT C RADIX)))

(DEFINE (ALPHANUMERIC? C)                 ; open-code someday?
  (LET ((C (CHECK-ARG CHAR? C ALPHANUMERIC?)))
    (OR (ALPHABETIC? C) (DIGIT? C 10))))

(DEFINE (CHAR->DIGIT C RADIX)
  (OR (DIGIT C RADIX)
      (CHAR->DIGIT (ERROR "argument isn't a digit in given radix.~%  ~S"
                          `(CHAR->DIGIT ,C ,RADIX))
                   RADIX)))

(DEFINE %CHAR->DIGIT %DIGIT)

;;; Common Lisp calls this DIGIT-CHAR.

(DEFINE (DIGIT->CHAR N RADIX)
  (LET ((N     (CHECK-ARG NONNEGATIVE-FIXNUM? N     DIGIT->CHAR))
        (RADIX (CHECK-ARG ACCEPTABLE-RADIX?   RADIX DIGIT->CHAR)))
    (COND ((FX> N RADIX)
           (ERROR "argument doesn't correspond to a digit.~%  ~S"
                  `(DIGIT->CHAR ,N ,RADIX)))
          (ELSE (%DIGIT->CHAR N RADIX)))))

(DEFINE (%DIGIT->CHAR N RADIX)
  (COND ((FX< N 10.)
         (CHAR+ #\0 N))
        (ELSE
         (CHAR+ #\A (FX- N 10.)))))

;;; This looks circular.  It is.

(DEFINE *SYMBOLIC-CHARACTER-TABLE*
  '(
    ;; System dependent options

    (NEWLINE    . #\NEWLINE)

    ;; Distinguished ASCII codes on any system

    (NULL       . #\NULL)
    (BELL       . #\BELL)
    (BACKSPACE  . #\BACKSPACE)
    (TAB        . #\TAB)
    (LINEFEED   . #\LINEFEED)
    (FORM       . #\FORM)
    (RETURN     . #\RETURN)
    (ESCAPE     . #\ALT)
    (ALTMODE    . #\ALT)
    (ALT        . #\ALT)
    (SPACE      . #\SPACE)
    (RUBOUT     . #\RUBOUT)
    (NUL        . #\NULL)               ; flush when compiler is fully native

    (LEFT-PAREN    . #\()
    (RIGHT-PAREN   . #\))
    (LEFT-BRACKET  . #\[)
    (RIGHT-BRACKET . #\])
    (LEFT-BRACE    . #\{)
    (RIGHT-BRACE   . #\})
    (BACKSLASH     . #\\)
    (QUOTE         . #\')
    (BACKQUOTE     . #\`)
    (DOUBLEQUOTE   . #\")
    (COMMA         . #\,)
    (DOT           . #\.)
    (SEMICOLON     . #\;)
    
    ))

(DEFINE (CHAR-NAME CH)
  (CAR (RASSQ CH *SYMBOLIC-CHARACTER-TABLE*)))

(DEFINE (NAME-CHAR SYMBOL)
  (CDR (ASSQ SYMBOL *SYMBOLIC-CHARACTER-TABLE*)))

