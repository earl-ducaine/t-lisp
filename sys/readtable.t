(HERALD (TSYS READTABLE T 93)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Read-table manipulation

;;; Read tables consist of data which drives READ and PRINT.

;;; For now, the read table is a vector with one entry for each ascii
;;; character.  Each such entry can be either a procedure, in which case the
;;; character is a procedure for a read macro, or (the value of) one of:
;;;   %%WHITESPACE
;;;   %%IGNORED
;;;   %%CONSTITUENT
;;;   %%ESCAPE-CHAR
;;;   %%UNDEFINED
;;; Thanks to Common Lisp (i.e. Steele) for the general shape of the thing.

(DEFINE-CONSTANT %%WHITESPACE  0)
(DEFINE-CONSTANT %%IGNORED     1)
(DEFINE-CONSTANT %%CONSTITUENT 2)
(DEFINE-CONSTANT %%ESCAPE-CHAR 3)
(DEFINE-CONSTANT %%UNDEFINED   4)

(DEFINE-INTEGRABLE (READ-MACRO? SYN) (NOT (FIXNUM? SYN)))       ; Hack!

(DEFINE-INTEGRABLE (READ-TABLE? OBJ) (RT? OBJ))

;;; The read table structure type.

(DEFINE-STRUCTURE-TYPE RT
  ID                ; Identification
  MUTABLE?          ; True if writable
  VECTOR            ; Vector with one entry per ascii code
  TRANSLATOR        ; Consituent translation function for atom scan
  STRING->SYMBOL    ; Thing to call if there are slashified chars  (???)
  RADIX             ; Input radix
  RECOGNIZER        ; Atom recognation functional  (???)
  KEYWORD-TABLE     ; Keyword table for #[...]
  )

(DEFINE-METHODS HANDLE-RT
  ((SET-IMMUTABLE RT)  (SET (RT-MUTABLE? RT) NIL))
  ((MUTABLE? RT)       (RT-MUTABLE? RT))
  ((IDENTIFICATION RT) (RT-ID RT))
  ((PRINT RT STREAM)
   (FORMAT STREAM "#{Read-table~_~S~_~S}" (OBJECT-HASH RT) (RT-ID RT))))

(DEFINE-INTEGRABLE (CHAR-SYNTAX RT CH)
  (VREF (RT-VECTOR RT) (CHAR->ASCII CH)))

(DEFINE *VANILLA-READ-TABLE*
  (LET ((RT (MAKE-RT)))
    (SET (RT-ID RT)         '*VANILLA-READ-TABLE*)
    (SET (RT-MUTABLE? RT)   NIL)
    (SET (RT-VECTOR RT)
         (LET ((V (VECTOR-FILL (MAKE-VECTOR *NUMBER-OF-CHAR-CODES*)
                               %%UNDEFINED)))
           (DO ((I 0 (FX+ I 1)))
               ((FX>= I *NUMBER-OF-CHAR-CODES*))
             (COND ((GRAPHIC? (ASCII->CHAR I))  ; careful - don't be circular
                    (SET (VREF V I) %%CONSTITUENT))))
           (WALK (LAMBDA (CH)
                   (SET (VREF V (CHAR->ASCII CH)) %%WHITESPACE))
                 '(#\SPACE #\RETURN #\LINEFEED #\TAB #\FORM))
           (SET (VREF V (CHAR->ASCII #\RUBOUT)) %%IGNORED)
           (SET (VREF V (CHAR->ASCII #\ESCAPE)) %%CONSTITUENT) ; pacify MIT?
           V))
    (SET (RT-TRANSLATOR RT)     %CHAR-UPCASE)
    (SET (RT-STRING->SYMBOL RT) STRING->SYMBOL)
    (SET (RT-RADIX RT)          10.)
    (SET (RT-RECOGNIZER RT)     RECOGNIZE-ATOM)
    RT))

(DEFINE (MAKE-READ-TABLE SUPER ID)
  (LET ((RT (COPY-STRUCTURE (CHECK-ARG READ-TABLE? SUPER MAKE-READ-TABLE)))
        (NEW (MAKE-VECTOR *NUMBER-OF-CHAR-CODES*)))
    (SET (RT-ID RT) ID)
    (SET (RT-MUTABLE? RT) T)
    (DO ((I 0 (FX+ I 1)))
        ((FX>= I *NUMBER-OF-CHAR-CODES*)
         (SET (RT-VECTOR RT) NEW)
         RT)
      (LET ((Z (VREF (RT-VECTOR RT) I)))
        (SET (VREF NEW I)
             (COND ((READ-MACRO? Z) (COPY-READ-TABLE-ENTRY Z))
                   (ELSE Z)))))))

(DEFINE-OPERATION (COPY-READ-TABLE-ENTRY SYN) SYN)

(DEFINE READ-TABLE-ENTRY
  (OBJECT (LAMBDA (RT CH)
            (CHAR-SYNTAX RT
                         (CHECK-ARG CHAR?
                                    CH
                                    READ-TABLE-ENTRY)))
          ((SETTER SELF)
           (LAMBDA (RT CH VAL)
             (COND ((RT-MUTABLE? RT)
                    (SET (VREF (RT-VECTOR RT)
                               (CHAR->ASCII (CHECK-ARG CHAR?
                                                       CH
                                                       SET-READ-TABLE-ENTRY)))
                         (IF (AND (PROCEDURE? VAL)
                                  (FX= (CAR (ARGSPECTRUM VAL)) 2))
                             ;; Hack from compatibility with 2.7-
                             (OBJECT (LAMBDA (STREAM CH RT)
                                       (IGNORE RT)
                                       (VAL STREAM CH))
                                     ((DELIMITING-READ-MACRO? SELF)
                                      (DELIMITING-READ-MACRO? VAL)))
                             VAL)))
                   (ELSE
                    (ERROR "attempt to alter an immutable read-table~%  ~S"
                           `(SET (READ-TABLE-ENTRY ,SELF ,CH) ,VAL))))))))

(DEFINE SET-READ-TABLE-ENTRY (SETTER READ-TABLE-ENTRY))

(DEFINE-INTEGRABLE (WHITESPACE? CH)
  (EQ? (CHAR-SYNTAX *VANILLA-READ-TABLE* CH) %%WHITESPACE))

(DEFINE-OPERATION (ESTABLISH-READ-TABLE-ENTRY VAL CH)
  (COND ((AND (EQ? VAL %%ESCAPE-CHAR) (NULL? *ESCAPE-CHAR*))
         (SET *ESCAPE-CHAR* CH))))

(DEFINE-PREDICATE DELIMITING-READ-MACRO?)

(DEFINE (CONSTITUENT-SYNTAX? E)     ; Used by print - can go away some day
  (COND ((FIXNUM? E)
         (FX= E %%CONSTITUENT))
        (ELSE
         (NOT (DELIMITING-READ-MACRO? E)))))

(DEFINE (NOT-CONSTITUENT PROC)      ; Probably not used by anyone
  (OBJECT PROC
          ((DELIMITING-READ-MACRO? SELF) T)
          ((ESTABLISH-READ-TABLE-ENTRY SELF CH)
           (ESTABLISH-READ-TABLE-ENTRY PROC CH))
          ((COPY-READ-TABLE-ENTRY SELF)
           (COPY-READ-TABLE-ENTRY PROC))
          ((PRINT-TYPE-STRING SELF) "Not-constituent")))

;;; Hack for reading/printing in different radices.
;;; If consing is a worry, pool these things.

(DEFINE (RT-WITH-RADIX RT RADIX)
  (LET ((NEW-RT (COPY-STRUCTURE RT)))
    (SET (RT-RADIX NEW-RT) RADIX)
    NEW-RT))

;;; Output control:

(LSET *TRANSLATE-CONSTITUENT-INVERSE* IDENTITY) ; set to %CHAR-DOWNCASE, e.g.

(LSET *PRINT-LEVEL*  *MAX-FIXNUM*)
(LSET *PRINT-LENGTH* *MAX-FIXNUM*)

(LSET *LIST-BEGIN-CHAR*  #\LEFT-PAREN)
(LSET *LIST-END-CHAR*    #\RIGHT-PAREN)
(LSET *DISPATCH-CHAR*    #\#)
(LSET *ESCAPE-CHAR*      #\BACKSLASH)
(LSET *STRING-DELIMITER* #\DOUBLEQUOTE)
(LSET *SYMBOL-DELIMITER* NIL)
