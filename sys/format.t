(HERALD (TSYS FORMAT T 45)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; FORMAT

;;; For robustness' sake, we put a stub definition of FORMAT in the low-level
;;; error-handling module which only decides to go here if it thinks the
;;; world's in a consistent state.

(DEFINE (REALLY-FORMAT STREAM F STUFF)
  (COND ((EQ? STREAM NIL)
         (WITH-OUTPUT-TO-STRING S (FORMAT-INTERNAL S F STUFF)))
        ((EQ? STREAM T)
         (FORMAT-INTERNAL (STANDARD-OUTPUT) F STUFF))
        (ELSE
         (FORMAT-INTERNAL STREAM F STUFF))))

(DEFINE *FORMAT-DISPATCH-TABLE*
  (VECTOR-FILL (MAKE-VECTOR *NUMBER-OF-CHAR-CODES*)
               NIL))

(DEFINE-INTEGRABLE (FORMAT-PROC CHAR)
  (VREF *FORMAT-DISPATCH-TABLE* (CHAR->ASCII CHAR)))

(DEFINE (SET-FORMAT-PROC CHAR PROC)
  (VSET *FORMAT-DISPATCH-TABLE* (CHAR->ASCII (CHAR-DOWNCASE CHAR)) PROC)
  (VSET *FORMAT-DISPATCH-TABLE* (CHAR->ASCII (CHAR-UPCASE CHAR)) PROC))

(DEFINE (FORMAT-INTERNAL STREAM F STUFF)
  (LET ((FOO (FORMAT-INTERNAL-LOOP STREAM F STUFF F)))
      (COND ((NOT (NULL? FOO))
             (ERROR "too many arguments in call to ~S~%  ~S"
                    'FORMAT
                    (CONS* 'FORMAT STREAM F STUFF))))
      '**VALUE-OF-FORMAT**))

(DEFINE (FORMAT-INTERNAL-LOOP STREAM FMT STUFF F)
  (COND ((PAIR? FMT)
         (DO ((FMT FMT (CDR FMT))
              (STUFF STUFF (FORMAT-INTERNAL-LOOP STREAM (CAR FMT) STUFF F)))
             ((NULL-LIST? FMT) STUFF)))
        (ELSE
         (LET ((FMT (CHOPY (CHECK-ARG STRING? FMT FORMAT))))
           (ITERATE LOOP ((STUFF STUFF)) 
             (COND ((STRING-EMPTY? FMT) STUFF)
                   ((CHAR= (CHAR FMT) #\~)
                    (CHDR! FMT)
                    (LET ((ARG (COND ((OR (CHAR= (CHAR FMT) #\-)
                                          (DIGIT? (CHAR FMT) 10))
                                      (FORMAT-GET-NUMBER FMT))  ; speed hack (?)
                                     (ELSE NIL)))
                          (OP (CHAR FMT)))      ; left-to-right
                      (CHDR! FMT)
                      (LET ((PROC (FORMAT-PROC OP)))
                        (COND ((NULL? PROC)
                               (ERROR "~C is an unknown code~%  ~S"
                                      OP
                                      (CONS* 'FORMAT STREAM F STUFF))
                               (LOOP STUFF))
                              (ELSE (LOOP (PROC STREAM FMT STUFF ARG)))))))
                   (ELSE
                    (WRITEC STREAM (CHAR FMT))
                    (CHDR! FMT)
                    (LOOP STUFF))))))))

;;; Gnaw a number off the string.  Clobber string header.

(DEFINE (FORMAT-GET-NUMBER F)           ; NWM
  (LET ((F2 (CHOPY F)))
    (DO ((F F (CHDR! F))
         (I 0 (FX+ I 1)))
        ((NOT (OR (CHAR= (CHAR F) #\-) (DIGIT? (CHAR F) 10)))
         (STRING->INTEGER (STRING-SLICE F2 0 I) 10)))))

;;; Peel off one object from argument list

(DEFINE (FORMAT-CAR STREAM FMT STUFF)
  (COND ((NULL? STUFF)
         (ERROR "too few arguments in call to ~S~%  (~S ~S ~S ...)"
                'FORMAT 'FORMAT
                STREAM FMT))
        (ELSE (CAR STUFF))))

;;; Kludge format: take the next argument as a format string.

(DEFINE (FORMAT-KLUDGE STREAM FMT STUFF ARG)
  (IGNORE ARG)
  (FORMAT-INTERNAL-LOOP STREAM (FORMAT-CAR STREAM FMT STUFF) (CDR STUFF) FMT))
(SET-FORMAT-PROC #\K FORMAT-KLUDGE)

;;; Fresh line followed by <arg>-1 newlines

(DEFINE (FORMAT-FRESH-LINE STREAM FMT STUFF ARG)
  (IGNORE FMT)
  (FRESH-LINE STREAM)
  (LET ((COUNT (IF (FIXNUM? ARG) ARG 1)))
    (DO ((I 1 (FX+ I 1)))
        ((FX>= I COUNT) STUFF)
      (NEWLINE STREAM))))
(SET-FORMAT-PROC #\& FORMAT-FRESH-LINE)

;;; <arg> new lines

(DEFINE (FORMAT-NEWLINE STREAM FMT STUFF ARG)
  (IGNORE FMT)
  (LET ((COUNT (IF (FIXNUM? ARG) ARG 1)))
    (DO ((I 0 (FX+ I 1)))
        ((FX>= I COUNT) STUFF)
      (NEWLINE STREAM))))
(SET-FORMAT-PROC #\% FORMAT-NEWLINE)

;;; Space over <arg> spaces

(DEFINE (FORMAT-SPACE STREAM FMT STUFF ARG)
  (IGNORE FMT)
  (LET ((COUNT (IF (FIXNUM? ARG) ARG 1)))
    (DO ((I 0 (FX+ I 1)))
        ((FX>= I COUNT) STUFF)
      (SPACE STREAM))))
(SET-FORMAT-PROC #\_ FORMAT-SPACE)

;;; Tab to column <arg>

(DEFINE (FORMAT-TAB STREAM FMT STUFF ARG)
  (IGNORE FMT)
  (COND ((FIXNUM? ARG)
         (SET-HPOS STREAM ARG))
        (ELSE
         (WRITEC STREAM #\TAB)))
  STUFF)
(SET-FORMAT-PROC #\T FORMAT-TAB)

;;; Utility for printing within fixed-width field.

(DEFINE (FORMAT-WRITE-FIELD STREAM WIDTH WRITER)
  (LET ((BUFFER (GET-BUFFER)))
    (LET ((OUT (BUFFER->OUTPUT-STREAM BUFFER)))
      (WRITER OUT)
      (LET ((COUNT (STRING-LENGTH BUFFER)))
        (COND ((FX>= WIDTH 0)
               ;; Pad on right.
               (WRITES STREAM BUFFER)
               (COND ((FX< COUNT WIDTH)
                      (WRITE-SPACES STREAM (FX- WIDTH COUNT)))))
              (ELSE
               ;; Pad on left.
               (LET ((WIDTH (FX- 0 WIDTH)))
                 (COND ((FX< COUNT WIDTH)
                        (WRITE-SPACES STREAM (FX- WIDTH COUNT)))))
               (WRITES STREAM BUFFER))))
      (CLOSE OUT))))

;;; Print

(DEFINE (FORMAT-PRINT STREAM FMT STUFF ARG)     ; hack field width
  (LET ((OBJ (FORMAT-CAR STREAM FMT STUFF)))
    (COND ((FIXNUM? ARG)
           (FORMAT-WRITE-FIELD STREAM
                               ARG
                               (LAMBDA (STREAM) (PRINT OBJ STREAM))))
          (ELSE
           (PRINT OBJ STREAM))))
  (CDR STUFF))
(SET-FORMAT-PROC #\S FORMAT-PRINT)

;;; Display

(DEFINE (FORMAT-DISPLAY STREAM FMT STUFF ARG)   ; hack field width
  (LET ((OBJ (FORMAT-CAR STREAM FMT STUFF)))
    (COND ((FIXNUM? ARG)
           (FORMAT-WRITE-FIELD STREAM
                               ARG
                               (LAMBDA (STREAM) (DISPLAY OBJ STREAM))))
          (ELSE
           (DISPLAY OBJ STREAM))))
  (CDR STUFF))
(SET-FORMAT-PROC #\A FORMAT-DISPLAY)

;;; Pretty-print

(DEFINE (FORMAT-PRETTY-PRINT STREAM FMT STUFF ARG)     ; hack field width
  (IGNORE ARG)
  (LET ((OBJ (FORMAT-CAR STREAM FMT STUFF)))
    (PRETTY-PRINT OBJ STREAM))
  (CDR STUFF))
(SET-FORMAT-PROC #\G FORMAT-PRETTY-PRINT)

;;; Pluralize - this is a hack

(DEFINE (FORMAT-PLURAL STREAM FMT STUFF ARG)
  (IGNORE ARG)
  (LET ((OBJ (CHECK-ARG NUMBER? (FORMAT-CAR STREAM FMT STUFF) FORMAT)))
    (IF (N= OBJ 1) (WRITEC STREAM #\s))
    (CDR STUFF)))
(SET-FORMAT-PROC #\P FORMAT-PLURAL)

;;; Number in various radices

(DEFINE (MAKE-RADICAL-FORMATTER RADIX)
  (LAMBDA (STREAM FMT STUFF ARG)
    (LET ((OBJ (FORMAT-CAR STREAM FMT STUFF)))
      (BIND ((*PRINT-TABLE* (RT-WITH-RADIX *PRINT-TABLE* RADIX)))
        (COND ((FIXNUM? ARG)
               (FORMAT-WRITE-FIELD STREAM
                                   ARG
                                   (LAMBDA (STREAM) (PRINT OBJ STREAM))))
              (ELSE
               (PRINT OBJ STREAM))))
      (CDR STUFF))))
(SET-FORMAT-PROC #\D (MAKE-RADICAL-FORMATTER 10))
(SET-FORMAT-PROC #\X (MAKE-RADICAL-FORMATTER 16))
(SET-FORMAT-PROC #\O (MAKE-RADICAL-FORMATTER  8))
(SET-FORMAT-PROC #\B (MAKE-RADICAL-FORMATTER  2))

;;; Number in radix <arg>

(DEFINE (FORMAT-RADICAL STREAM FMT STUFF ARG)
  (BIND ((*PRINT-TABLE*
	  (RT-WITH-RADIX *PRINT-TABLE*
			 (CHECK-ARG ACCEPTABLE-RADIX? ARG FORMAT))))
    (WRITE STREAM (FORMAT-CAR STREAM FMT STUFF))
    (CDR STUFF)))
(SET-FORMAT-PROC #\R FORMAT-RADICAL)

;;; Character

(DEFINE (FORMAT-CHAR STREAM FMT STUFF ARG)
  (IGNORE ARG)
  (COND ((CONTROL? (FORMAT-CAR STREAM FMT STUFF))
         (WRITEC STREAM #\^)
         (WRITEC STREAM (UNCONTROLIFY (FORMAT-CAR STREAM FMT STUFF))))
        (ELSE
         (WRITEC STREAM (FORMAT-CAR STREAM FMT STUFF))))
  (CDR STUFF))
(SET-FORMAT-PROC #\C FORMAT-CHAR)

;;; ~~ prints a tilde

(DEFINE (FORMAT-TILDE STREAM FMT STUFF ARG)
  (IGNORE ARG)
  (WRITEC STREAM #\~)
  STUFF)
(SET-FORMAT-PROC #\~ FORMAT-TILDE)

;;; ~<whitespace> is ignored

(DEFINE (FORMAT-SKIP-WHITESPACE STREAM FMT STUFF ARG)
  (IGNORE ARG)
  (ITERATE SKIP ()
    (COND ((WHITESPACE? (CHAR FMT))
           (CHDR! FMT) (SKIP))
          (ELSE STUFF))))
(SET-FORMAT-PROC #\LINEFEED FORMAT-SKIP-WHITESPACE)
(SET-FORMAT-PROC #\RETURN   FORMAT-SKIP-WHITESPACE)
(SET-FORMAT-PROC #\SPACE    FORMAT-SKIP-WHITESPACE)
(SET-FORMAT-PROC #\TAB      FORMAT-SKIP-WHITESPACE)
