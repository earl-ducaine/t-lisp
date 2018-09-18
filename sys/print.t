(HERALD (TSYS PRINT T 105)
        (ENV TSYS (TSYS READTABLE)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; PRINT

;;; What about robustness problem - errors within the printer?
;;; Have to be able to do something intelligent with pair pointer into
;;; outer space, etc.  Can we hook the PRINT operation to do
;;; reasonableness check on arg before dispatching?

;;; DEFINE-OPERATIONS for PRINT, DISPLAY, and PRINT-TYPE-STRING have been
;;; moved to OPERATION due to bug in DEFINE-METHODS.

(LSET *PRINT-TABLE* *STANDARD-READ-TABLE*)

;;; Printers for primitive types.

;;; ()

(LSET *PRINT-NULL-AS-SHARP-F?* NIL)

(DEFINE-METHODS HANDLE-NULL
  ((PRINT OBJ STREAM)
   (IGNORE OBJ)
   (COND (*PRINT-NULL-AS-SHARP-F?*
	  (WRITE-STRING STREAM "#F"))
	 (ELSE
	  (WRITE-CHAR STREAM *LIST-BEGIN-CHAR*)
	  (WRITE-CHAR STREAM *LIST-END-CHAR*)))))

;;; Characters.

(DEFINE-METHODS HANDLE-CHAR
  ((PRINT OBJ STREAM)
   (COND ((CHAR= OBJ #\SPACE)
          (WRITE-STRING STREAM "#\\SPACE"))
         ((GRAPHIC? OBJ)
          (WRITE-STRING STREAM "#\\")
          (WRITE-CHAR STREAM OBJ))
         ((CHAR-NAME OBJ)
          => (LAMBDA (NAME) (FORMAT STREAM "#\\~S" NAME)))
         ((CONTROL? OBJ)
          (WRITE-STRING STREAM "#^")
          (WRITE-CHAR STREAM (UNCONTROLIFY OBJ)))
         (ELSE
          (FORMAT STREAM "#[Ascii~_~D]" (CHAR->ASCII OBJ)))))
  ((DISPLAY OBJ STREAM) (WRITE-CHAR STREAM OBJ)))

;;; Strings.  Should pre-scan the string to decide whether it can be
;;; blatted out with a single WRITE-STRING.

(DEFINE-METHODS HANDLE-STRING
  ((PRINT OBJ STREAM)
   (PRINT-DELIMITED-STRING OBJ STREAM *STRING-DELIMITER*))
  ((DISPLAY OBJ STREAM) (WRITE-STRING STREAM OBJ)))

(LSET *STRING-PRINT-LENGTH* 32768.)

(DEFINE (PRINT-DELIMITED-STRING OBJ STREAM DELIM)
  (COND ((NOT (REASONABLE? OBJ))        ; Robustness implies hair.  Sorry.
         (PRINT-RANDOM OBJ STREAM))
        (ELSE
         (LET ((LEN (STRING-LENGTH OBJ)))
           (COND ((OR (FX< LEN 0) (FX>= LEN *STRING-PRINT-LENGTH*))
                  (FORMAT STREAM "#{Very-long-string-whose-length-is~_~S}"
                          LEN))
                 (ELSE
                  (WRITE-CHAR STREAM DELIM)
                  (DO ((I 0 (FX+ I 1)))
                      ((FX>= I LEN)
                       (WRITE-CHAR STREAM DELIM))
                    (LET ((CH (NTHCHAR OBJ I)))
                      (COND ((CHAR= CH #\NEWLINE)
                             (NEWLINE STREAM))
                            (ELSE
                             (COND ((OR (CHAR= CH DELIM)
                                        (CHAR= CH *ESCAPE-CHAR*))
                                    (WRITE-CHAR STREAM *ESCAPE-CHAR*)))
                             (WRITE-CHAR STREAM CH)))))))))))

;;; List printer.

(DEFINE-METHODS HANDLE-PAIR
  ((PRINT OBJ STREAM)
   (RPRINT OBJ STREAM 0 T))
  ((DISPLAY OBJ STREAM)
   (RPRINT OBJ STREAM 0 NIL)))

(DEFINE *PRINT-LEVEL-EXCESS* "(...)")
(DEFINE *PRINT-LENGTH-EXCESS* "...")

;;; "Recurring print" - this needs a better name, yes?  PRINT-OBJECT?
;;; Maybe it should be an operation?  Whaddaya think?

(DEFINE (RPRINT OBJ STREAM LEVEL SLASHIFY?)
  (COND ((ATOM? OBJ) (PRINT OBJ STREAM))
        ((NOT (REASONABLE? OBJ)) (PRINT-RANDOM OBJ STREAM))
        ((FX> LEVEL *PRINT-LEVEL*)
         (WRITE-STRING STREAM *PRINT-LEVEL-EXCESS*))
        (ELSE
         (WRITE-CHAR STREAM *LIST-BEGIN-CHAR*)
         (PRINT-DELIMITED-LIST OBJ STREAM (FX+ LEVEL 1) SLASHIFY?)
         (WRITE-CHAR STREAM *LIST-END-CHAR*))))

;;; This prints the elements of a list, without the parentheses.

(DEFINE (PRINT-DELIMITED-LIST OBJ STREAM NEW-LEVEL SLASHIFY?)
  (ITERATE LOOP ((L OBJ)
                 (FLAG NIL)
                 (N 0))
    (COND ((ATOM? L)
           (COND ((NOT (NULL? L))
                  (SPACE STREAM)
                  (WRITE-CHAR STREAM *DOT-CHAR*)
                  (SPACE STREAM)
                  (RPRINT L STREAM NEW-LEVEL SLASHIFY?))))
          (ELSE
           (IF FLAG (SPACE STREAM))
           (COND ((FX>= N *PRINT-LENGTH*)
                  (WRITE-STRING STREAM *PRINT-LENGTH-EXCESS*))
                 (ELSE
                  (RPRINT (CAR L) STREAM NEW-LEVEL SLASHIFY?)
                  (LOOP (CDR L) T (FX+ N 1))))))))

(DEFINE-METHODS HANDLE-VECTOR
  ((PRINT OBJ STREAM)
   (WRITE-CHAR STREAM *DISPATCH-CHAR*)
   (WRITE-CHAR STREAM *LIST-BEGIN-CHAR*)
   (ITERATE LOOP ((FLAG NIL)
                  (I 0))
     (COND ((FX>= I (VECTOR-LENGTH OBJ)))
           (ELSE
            (IF FLAG (SPACE STREAM))
            (COND ((FX>= I *PRINT-LENGTH*)
                   (WRITE-STRING STREAM *PRINT-LENGTH-EXCESS*))
                  (ELSE
                   (PRINT (VREF OBJ I) STREAM)
                   (LOOP T (FX+ I 1)))))))
   (WRITE-CHAR STREAM *LIST-END-CHAR*)))

;;; Symbol printer.

(LSET *FANCY-SYMBOL-PRINTING?* T)

(DEFINE-METHODS HANDLE-SYMBOL
  ((PRINT OBJ STREAM)
   (IF *FANCY-SYMBOL-PRINTING?*
       (FANCY-SYMBOL-PRINTER OBJ STREAM)
     (WRITE-STRING STREAM (SYMBOL-PNAME OBJ))))
  ((DISPLAY OBJ STREAM)
   (WRITE-STRING STREAM (SYMBOL-PNAME OBJ))))

;;; Fancy symbol printer.  Handles slashification; attempts printing with
;;; minimum number of characters under the constraint of rereadability.

(DEFINE (FANCY-SYMBOL-PRINTER OBJ STREAM)
  (LET* ((PNAME (SYMBOL-PNAME OBJ))
         (LEN (STRING-LENGTH PNAME)))
    (COND ((FX> LEN 0)
           (ITERATE LOOP ((I 0) (NEED-TO-ESCAPE? NIL))
             (COND ((FX>= I LEN)
                    (COND (NEED-TO-ESCAPE?
                           (PRINT-ESCAPED-PNAME PNAME STREAM))
                          ((NEQ? ((RT-RECOGNIZER *PRINT-TABLE*)
                                  PNAME
                                  *PRINT-TABLE*)
                                 *PARSES-AS-SYMBOL*)
                           (PRINT-PNAME-WITH-LEADING-ESCAPE PNAME STREAM))
                          (ELSE
                           (PRINT-REGULAR-PNAME PNAME STREAM))))
                   (ELSE
                    (LET ((NEED-TO-ESCAPE
                           (LAMBDA ()
                             (COND ((OR NEED-TO-ESCAPE?
                                        (NOT *ESCAPE-CHAR*))
                                    (PRINT-DELIMITED-PNAME PNAME STREAM))
                                   ((NOT *SYMBOL-DELIMITER*)
                                    (PRINT-ESCAPED-PNAME PNAME STREAM))
                                   (ELSE (LOOP (FX+ I 1) T)))))
                          (NEED-TO-DELIMIT
                           (LAMBDA () (PRINT-DELIMITED-PNAME PNAME STREAM)))
                          (CH (NTHCHAR PNAME I)))
                      (LET ((E (CHAR-SYNTAX *PRINT-TABLE* CH)))
                        (SELECT E
                          ((%%CONSTITUENT)
                           (COND ((NEQ? CH ((RT-TRANSLATOR *PRINT-TABLE*) CH))
                                  (NEED-TO-ESCAPE))
                                 (ELSE
                                  ;; This is the most important case ...
                                  (LOOP (FX+ I 1) NEED-TO-ESCAPE?))))
                          ((%%ESCAPE-CHAR)
                           (NEED-TO-ESCAPE))
                          ((%%WHITESPACE %%IGNORED %%UNDEFINED)
                           (NEED-TO-DELIMIT))
                          (ELSE
                           ;; Read macro.
                           (COND ((OR (NOT (CONSTITUENT-SYNTAX? E))
                                      (FX= I 0))
                                  (NEED-TO-ESCAPE))
                                 (ELSE
                                  (LOOP (FX+ I 1) NEED-TO-ESCAPE?)))))))))))
          (*SYMBOL-DELIMITER* (PRINT-DELIMITED-PNAME PNAME STREAM))
          (ELSE (LOSING-SYMBOL-PRINTATION PNAME STREAM)))))

;;; Just emit the symbol directly.

(DEFINE (PRINT-REGULAR-PNAME PNAME STREAM)
  (COND ((EQ? *TRANSLATE-CONSTITUENT-INVERSE* IDENTITY)
         (WRITE-STRING STREAM PNAME))
        (ELSE
         (LET ((LEN (STRING-LENGTH PNAME)))
           (DO ((I 0 (FX+ I 1)))
               ((FX>= I LEN) NIL)
             (WRITE-CHAR STREAM
                     (*TRANSLATE-CONSTITUENT-INVERSE* (NTHCHAR PNAME I))))))))

;;; Symbol has no funny characters, but if it were printed using
;;; PRINT-REGULAR-PNAME then it would be re-read as a number or some
;;; random thing.

(DEFINE (PRINT-PNAME-WITH-LEADING-ESCAPE PNAME STREAM)
  (COND (*ESCAPE-CHAR*
         (WRITE-CHAR STREAM *ESCAPE-CHAR*)
         (WRITE-CHAR STREAM (CHAR PNAME))
         (PRINT-REGULAR-PNAME (CHDR PNAME) STREAM))
        (ELSE (PRINT-DELIMITED-PNAME PNAME STREAM))))

;;; Try to print symbol using vertical-bar syntax.

(DEFINE (PRINT-DELIMITED-PNAME PNAME STREAM)
  (COND (*SYMBOL-DELIMITER*
         (PRINT-DELIMITED-STRING PNAME STREAM *SYMBOL-DELIMITER*))
        (*ESCAPE-CHAR*
         (PRINT-ESCAPED-PNAME PNAME STREAM))
        (ELSE (LOSING-SYMBOL-PRINTATION PNAME STREAM))))

;;; Try to print symbol, escaping funny characters.

(DEFINE (PRINT-ESCAPED-PNAME PNAME STREAM)
  (COND (*ESCAPE-CHAR*
         (DO ((LEN (STRING-LENGTH PNAME))
              (I 0 (FX+ I 1)))
             ((FX>= I LEN) T)
           ;; What about control characters?
           (LET* ((CH (NTHCHAR PNAME I))
                  (SYN (CHAR-SYNTAX *PRINT-TABLE* CH)))
             (COND ((OR (NOT (CONSTITUENT-SYNTAX? SYN))         ; E.g. \(
                        (AND (NOT (FIXNUM? SYN)) (FX= I 0))     ; E.g. \'X
                        (NEQ? CH ((RT-TRANSLATOR *PRINT-TABLE*) CH))) ; E.g. \x
                    (WRITE-CHAR STREAM *ESCAPE-CHAR*)
                    (WRITE-CHAR STREAM CH))
                   (ELSE
                    (WRITE-CHAR STREAM (*TRANSLATE-CONSTITUENT-INVERSE* CH)))))))
        (*SYMBOL-DELIMITER*
         (PRINT-DELIMITED-PNAME PNAME STREAM))
        (ELSE (LOSING-SYMBOL-PRINTATION PNAME STREAM))))

;;; We go here if none of the above strategies is appropriate.

(DEFINE (LOSING-SYMBOL-PRINTATION PNAME STREAM)
  (FORMAT STREAM "#[Symbol~_~S]" PNAME))


;;; Fixnum printer.

(DEFINE-METHODS HANDLE-FIXNUM
  ((PRINT OBJ STREAM)
   (COND ((FX= OBJ 0)
          (WRITE-CHAR STREAM #\0))
         (ELSE
          (IF (FX< OBJ 0) (WRITE-CHAR STREAM *NEGATIVE-SIGN-CHAR*))
          (LABELS (((PNUM N STREAM)
                    (LET ((RADIX (RT-RADIX *PRINT-TABLE*)))
                      (COND ((NOT (FX= N 0))
                             (PNUM (FX/ N RADIX) STREAM)
                             (WRITE-CHAR STREAM
                                     (DIGIT->CHAR (FIXNUM-ABS
                                                   (FIXNUM-REMAINDER
                                                      N RADIX))
                                                  RADIX)))))))
            (PNUM OBJ STREAM))))))

;;; PRINT-FLONUM can be very slow, so we give the user the option of using
;;; a faster "kludgey" flonum printer.

(LSET *PRINT-FLONUMS-KLUDGILY?* NIL)

(DEFINE-METHODS HANDLE-FLONUM
  ((PRINT OBJ STREAM)
   (COND ((NOT (REASONABLE? OBJ))   (PRINT-RANDOM          OBJ STREAM))
         (*PRINT-FLONUMS-KLUDGILY?* (PRINT-FLONUM-KLUDGILY OBJ STREAM))
         (ELSE                      (PRINT-FLONUM          OBJ STREAM)))))

(DEFINE-METHODS HANDLE-NONVALUE
  ((PRINT OBJ STREAM)
   (LET ((FOO (NONVALUE->VALUE OBJ)))
     (COND ((NOT (REASONABLE? OBJ))
            (PRINT-RANDOM OBJ STREAM))
           ((VCELL? FOO)
            (FORMAT STREAM "#{Has-no-value~_~S}" (VCELL-ID FOO)))
           (ELSE
            (FORMAT STREAM "#{Nonvalue~_~S}" FOO))))))

(DEFINE-METHODS HANDLE-XENOID
  ((PRINT OBJ STREAM)
   (FORMAT STREAM "#{Xenoid~_#x~X}" (POINTER->INTEGER (XENOID-POINTER OBJ)))))

(DEFINE-METHODS HANDLE-BYTEV
  ((PRINT OBJ STREAM)
   (FORMAT STREAM "#{Bytev")
   (DO ((I 0 (FX+ I 1)))
       ((FX>= I (BYTEV-LENGTH OBJ)) (WRITE-CHAR STREAM #\}))
     (LET ((BYTE (BREF OBJ I)))
       (IF (FX= (FIXNUM-REMAINDER I 4) 0) (SPACE STREAM))
       (WRITE-CHAR STREAM (DIGIT->CHAR (FIXNUM-ASHR BYTE 4.) 16.))
       (WRITE-CHAR STREAM (DIGIT->CHAR (FIXNUM-LOGAND BYTE 15.) 16.))))))

(DEFINE (PRINT-RANDOM OBJ STREAM)
  (COND ((NOT (REASONABLE? OBJ))
         (FORMAT STREAM "#{Unreasonable~_#x~X}" (POINTER->INTEGER OBJ)))
        (ELSE
         (LET ((TYPE (PRINT-TYPE-STRING OBJ))
               (H    (OBJECT-HASH OBJ))
               (ID   (IDENTIFICATION OBJ)))
           (COND (ID (FORMAT STREAM "#{~A~_~S~_~S}" TYPE H ID))
                 (ELSE (FORMAT STREAM "#{~A~_~S}" TYPE H)))))))

(DEFINE (PRINT-TYPE-STRING-RANDOM OBJ)
  (COND ((BOGUS-ENTITY? OBJ)
         (IF (PROCEDURE? (BOGUS-ENTITY-PROCEDURE OBJ)) "Procedure" "Object"))
        ((PROCEDURE? OBJ) "Procedure")
        ((FRAME?     OBJ) "Continuation")
        ((EXTEND?    OBJ) "Object")
        ;;; Should never fall through past REASONABLE? check.
        (ELSE "Random")))

(DEFINE-METHODS HANDLE-TEMPLATE
  ((PRINT-TYPE-STRING OBJ) (IGNORE OBJ) "Template"))

(DEFINE-METHODS HANDLE-ESCAPE-PROCEDURE
  ((PRINT-TYPE-STRING OBJ) (IGNORE OBJ) "Escape-procedure"))
