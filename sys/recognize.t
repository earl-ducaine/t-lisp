(HERALD (TSYS RECOGNIZE T 85)
        (ENV TSYS (TSYS READTABLE)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Recognizers for numbers

;;; This is used by the reader, to decide whether something's a number
;;; or not, and by the printer, to decide whether it needs to slashify
;;; a symbol.

;;; El Hacko versions to use until continuation-passing is really cheap, or
;;; until we have a good regular-expression package, or whatever.  This is not
;;; intended to be clean or final in any sense.

;;; What about recognizing 15./29. for ratios?  Hmm.  Later.

;;; Utilities:

(DEFINE-CONSTANT *DOT-CHAR* #\.)
(DEFINE-INTEGRABLE (DOT-CHAR? CH) (CHAR= CH *DOT-CHAR*))

(DEFINE-CONSTANT *POSITIVE-SIGN-CHAR* #\+)
(DEFINE-CONSTANT *NEGATIVE-SIGN-CHAR* #\-)
(DEFINE-CONSTANT *RATIO-CHAR*         #\/)

(DEFINE-INTEGRABLE (EXPONENT-INTRODUCER? CH)
  (OR (CHAR= CH #\e) (CHAR= CH #\E)))

(DEFINE-INTEGRABLE (SIGN-CHAR? CH)
  (OR (CHAR= CH *POSITIVE-SIGN-CHAR*)
      (CHAR= CH *NEGATIVE-SIGN-CHAR*)))

;;; Main entry point:

(DEFINE (RECOGNIZE-ATOM S RT)                ; Ad hac
  (LET ((C (CHAR S))
	(RADIX (RT-RADIX RT)))
    (OR (COND ((CHAR= C *DOT-CHAR*)
               (COND ((FX= (STRING-LENGTH S) 1) *PARSES-AS-DOT*)
                     (ELSE (RECOGNIZE-FRACTION (CHDR S)))))
              ((%DIGIT? C (IF (FX< RADIX 10) 10 RADIX))
               (RECOGNIZE-NUMBER (CHOPY S) RT))
              ((FX= (STRING-LENGTH S) 1) NIL)   ; + and - aren't numbers
              ((SIGN-CHAR? C)
               (RECOGNIZE-SIGNED-NUMBER (CHDR S) RT))
              (ELSE NIL))
        *PARSES-AS-SYMBOL*)))

(DEFINE (RECOGNIZE-SIGNED-NUMBER S RT)
  ;; Sign has been gobbled.  Dispatch on next character.
  (LET ((C (CHAR S)))
    (COND ((CHAR= C *DOT-CHAR*)
           (RECOGNIZE-FRACTION (CHDR S)))
          ((%DIGIT? C (LET ((RADIX (RT-RADIX RT)))
			(IF (FX< RADIX 10) 10 RADIX)))
           (RECOGNIZE-NUMBER S RT))
          (ELSE NIL))))

(DEFINE (RECOGNIZE-NUMBER S RT)
  ;; Determine radix: if there are e's or dots, then base 10; else given radix.
  (LET ((RADIX (COND ((OR (STRING-POSQ #\. S)
                          (AND (FX< (RT-RADIX RT) 15)
                               (OR (STRING-POSQ #\E S)
                                   (STRING-POSQ #\e S))))
                      10)
                     (ELSE (RT-RADIX RT)))))
    ;; Scan over initial digits.
    (ITERATE LOOP ()
      (COND ((STRING-EMPTY? S) *PARSES-AS-INTEGER*)
            (ELSE
             (LET ((C (CHAR S)))
               (CHDR! S)
               (COND ((%DIGIT? C RADIX)
                      (LOOP))
                     ((CHAR= C #\.)
                      (COND ((STRING-EMPTY? S) *PARSES-AS-DECIMAL-INTEGER*)
                            (ELSE (RECOGNIZE-OPTIONAL-FRACTION S))))
                     ((EXPONENT-INTRODUCER? C) (RECOGNIZE-FLOAT-EXPONENT S))
                     ((CHAR= C *RATIO-CHAR*)
                      (IF (RECOGNIZE-INTEGER S RADIX) *PARSES-AS-RATIO* NIL))
                     (ELSE NIL))))))))

(DEFINE (RECOGNIZE-FRACTION S)
  ;; Dot has already been gobbled.  Digits must follow.
  (COND ((%DIGIT? (CHAR S) 10)
         (RECOGNIZE-OPTIONAL-FRACTION S))
        (ELSE NIL)))

(DEFINE (RECOGNIZE-OPTIONAL-FRACTION S)
  (ITERATE LOOP ()
    (COND ((STRING-EMPTY? S) *PARSES-AS-FLOAT*)
          (ELSE
           (LET ((C (CHAR S)))
             (CHDR! S)
             (COND ((%DIGIT? C 10) (LOOP))
                   ((EXPONENT-INTRODUCER? C) (RECOGNIZE-FLOAT-EXPONENT S))
                   (ELSE NIL)))))))

(DEFINE (RECOGNIZE-FLOAT-EXPONENT S)
  ;; E has already been gobbled.
  (COND ((STRING-EMPTY? S) *PARSES-AS-FLOAT*)
        (ELSE (IF (SIGN-CHAR? (CHAR S)) (CHDR! S))
              (IF (RECOGNIZE-INTEGER S 10) *PARSES-AS-FLOAT* NIL))))

(DEFINE (RECOGNIZE-INTEGER S RADIX)
  (COND ((STRING-EMPTY? S) NIL)
        (ELSE
         (ITERATE LOOP ()
           (LET ((C (CHAR S)))
             (CHDR! S)
             (COND ((NOT (%DIGIT? C RADIX)) NIL)
                   ((STRING-EMPTY? S) *PARSES-AS-INTEGER*)
                   (ELSE (LOOP))))))))

(DEFINE *PARSES-AS-DOT*
  (LAMBDA (S RT) (IGNORE S RT) *DOT-TOKEN*))

(DEFINE *PARSES-AS-SYMBOL*
  (LAMBDA (S RT)
    ((RT-STRING->SYMBOL RT) S)))

(DEFINE *PARSES-AS-INTEGER*
  (LAMBDA (S RT)
    (STRING->INTEGER S (RT-RADIX RT))))      ; Defined in BIGNUM module

(DEFINE *PARSES-AS-DECIMAL-INTEGER*
  (LAMBDA (S RT)
    (IGNORE RT)
    (LET ((S (CHOPY S)))
      (SET-STRING-LENGTH S (FX- (STRING-LENGTH S) 1))
      (STRING->INTEGER S 10))))

(DEFINE *PARSES-AS-FLOAT*
  (LAMBDA (S RT)
    (IGNORE RT)
    (STRING->FLONUM S)))

(DEFINE *PARSES-AS-RATIO*
  (LAMBDA (S RT)
    (LET ((S1 (CHOPY S))
          (Q (STRING-POSQ *RATIO-CHAR* S))
	  (RADIX (RT-RADIX RT)))
      (SET-STRING-LENGTH S1 Q)
      (RATIO (STRING->INTEGER S1                    RADIX)
             (STRING->INTEGER (NTHCHDR S (FX+ Q 1)) RADIX)))))
