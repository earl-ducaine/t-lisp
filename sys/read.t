(HERALD (TSYS READ T 144)
        (ENV TSYS (TSYS READTABLE)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; External representation parser

;;; The incredible Reader.  It is here that the language's external syntax
;;; (such as it is) is definitively defined.

;;; ---------- Internal markers and tokens

;;; Class of markers for right brackets

(DEFINE *CLOSE-TOKEN-MARKER* (MAKE-TOKEN '*CLOSE-TOKEN-MARKER*))

(DEFINE-INTEGRABLE (MAKE-CLOSE-TOKEN)
  (CONS *CLOSE-TOKEN-MARKER* NIL))

(DEFINE-INTEGRABLE (CLOSE-TOKEN? X)
  (AND (PAIR? X) (EQ? (CAR X) *CLOSE-TOKEN-MARKER*)))

;;; Token for dot syntax

(DEFINE *DOT-TOKEN* (MAKE-TOKEN '*DOT-TOKEN*))

(DEFINE-INTEGRABLE (DOT-TOKEN? X)
  (EQ? X *DOT-TOKEN*))

;;; *NOTHING-READ* is the mechanism whereby readmacros can choose to not
;;; return anything.

(DEFINE *NOTHING-READ* (MAKE-TOKEN '*NOTHING-READ*))

(DEFINE-INTEGRABLE (NOTHING-READ? X)
  (EQ? X *NOTHING-READ*))

;;; ---------- Top-level entries into reader

;;; Some of the following procedure names can be explained by imagining
;;; that an "object" is either an object read in the normal way or an
;;; eof token, and that a "thing" might include some strange internal
;;; reader token, like a dot or a close-bracket.

;;; READ-OBJECT is the default method for the READ operation.

(DEFINE (READ-OBJECT STREAM READ-TABLE)
  (READ-OBJECT-1 STREAM READ-TABLE NIL))

;;; Recursive entry from readmacros.  This is guaranteed to return a useable
;;; value, no funny tokens of any sort.

(DEFINE (READ-REFUSING-EOF STREAM)
  (READ-OBJECT-REFUSING-EOF STREAM (STREAM-READ-TABLE STREAM)))

(DEFINE (READ-OBJECT-REFUSING-EOF STREAM RT)
  (READ-OBJECT-1 STREAM RT T))

(DEFINE (READ-OBJECT-1 STREAM RT REFUSE-EOFS?)
  (ITERATE LOOP ()                      ; Throw away bogus close tokens.
    (LET ((OBJ (READ-THING-REFUSING-DOTS STREAM RT)))
      (COND ((EOF? OBJ)
             ;; EOF's might or might not be returnable.
             (COND (REFUSE-EOFS?
                    (READ-ERROR STREAM "unexpected end-of-file"))
                   (ELSE OBJ)))
            ((CLOSE-TOKEN? OBJ)
             ;; Right parentheses might or might not be ignorable.
             (COND ((AND (NOT REFUSE-EOFS?)
                         (INTERACTIVE-STREAM? STREAM))
                    (LOOP))
                   (ELSE
                    (UNREAD-CHAR STREAM)    ; Incredible hack.
                    (READ-ERROR STREAM "unexpected \"~C\""
                                (READ-CHAR STREAM)))))
            (ELSE
             ;; Object is neither EOF nor close.  Return it.
             OBJ)))))

;;; This is called from the list reader and from the two above routines.

(DEFINE (READ-THING-REFUSING-DOTS STREAM RT)
  (LET ((OBJ (READ-THING STREAM RT)))
    (COND ((DOT-TOKEN? OBJ)
           (READ-ERROR STREAM "\" . \" in illegal context"))
          (ELSE OBJ))))

;;; ---------- Main dispatch for reader

;;; This is the place where the scanning and dispatching actually happens.

(DEFINE (READ-THING STREAM RT)
  (ITERATE LOOP ()
    (LET ((CH (READ-CHAR STREAM)))
      (COND ((EOF? CH) CH)
	    (ELSE
	     (LET ((SYN (CHAR-SYNTAX RT CH)))
	       (COND ((READ-MACRO? SYN)
		      (LET ((OBJ (SYN STREAM CH RT)))
			(IF (NOTHING-READ? OBJ) (LOOP) OBJ)))
		     (ELSE
		      (SELECT SYN
			      ((%%WHITESPACE %%IGNORED) (LOOP))
			      (ELSE (READ-ATOM STREAM RT CH)))))))))))

;;; Not readmacro, not whitespace: just a vanilla symbol or number.

(DEFINE (READ-ATOM STREAM RT CH)
  (LET ((B (GET-BUFFER)))               ; ought to bind this somehow
    (ITERATE LOOP ((CH CH) (SLASHES? NIL))
      ((LAMBDA (ACCUM DONE)
         (COND ((EOF? CH) (DONE))
               (ELSE (LET ((SYN (CHAR-SYNTAX RT CH)))
                       (COND ((NOT (READ-MACRO? SYN))
                              (SELECT SYN
                                ((%%CONSTITUENT)
                                 (ACCUM ((RT-TRANSLATOR RT) CH)
                                        SLASHES?))
                                ((%%WHITESPACE)
                                 (DONE))
                                ((%%ESCAPE-CHAR)
				 (LET ((CH (READ-CHAR STREAM)))
				   (IF (EOF? CH)
				       (READ-ERROR STREAM
						   ;; Elaborate on this...
						   "EOF follows escape char")
				       (ACCUM CH T))))
                                ((%%UNDEFINED)
                                 (ILLEGAL-CHAR-ENCOUNTERED STREAM CH))
                                (ELSE
                                 (LOOP (READ-CHAR STREAM) SLASHES?))))
                             ((NOT (DELIMITING-READ-MACRO? SYN))
                              (ACCUM ((RT-TRANSLATOR RT) CH)
                                     SLASHES?))
                             (ELSE (DONE)))))))
       (LAMBDA (CH SLASHES?)            ; ACCUM
         (BUFFER-WRITEC B CH)
         (LOOP (READ-CHAR STREAM) SLASHES?))
       (LAMBDA ()                       ; DONE
         (UNREAD-CHAR STREAM)           ; Put back the delimiter
         (LET ((RESULT (COND (SLASHES? ((RT-STRING->SYMBOL RT) B))
                             (ELSE (((RT-RECOGNIZER RT) B RT)
                                    B
                                    RT)))))
           (RELEASE-BUFFER B)
           RESULT))))))

(DEFINE (ILLEGAL-CHAR-ENCOUNTERED STREAM CH)
  (READ-ERROR STREAM "illegal character ~S" CH))

;;; Create the standard read table.

(DEFINE *STANDARD-READ-TABLE*
  (MAKE-READ-TABLE *VANILLA-READ-TABLE* '*STANDARD-READ-TABLE*))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\BACKSLASH %%ESCAPE-CHAR)

(WALK (LAMBDA (CH)
        (SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* CH %%UNDEFINED))
      '(#\LEFT-BRACE #\RIGHT-BRACE #\LEFT-BRACKET #\RIGHT-BRACKET))

;;; ---------- Standard read macros

(DEFINE (MAKE-LIST-READER)
  (LET* ((TOKEN (MAKE-CLOSE-TOKEN))
         (RIGHT
          (OBJECT (LAMBDA (STREAM CH RT)
                    (IGNORE STREAM CH RT)
                    TOKEN)
                  ((DELIMITING-READ-MACRO? SELF) T)
                  ((ESTABLISH-READ-TABLE-ENTRY SELF CH)
                   (IF (AND (EQ? SELF
                                 (READ-TABLE-ENTRY *STANDARD-READ-TABLE*
                                                   #\RIGHT-PAREN))
                            (NULL? *LIST-END-CHAR*))
                       (SET *LIST-END-CHAR* CH)))
                  ((PRINT-TYPE-STRING SELF) "List-terminator"))))
    (OBJECT (LAMBDA (STREAM CH RT)
              (IGNORE CH)
              (READ-DELIMITED-LIST STREAM TOKEN RT))
            ((DELIMITING-READ-MACRO? SELF) T)
            ((ESTABLISH-READ-TABLE-ENTRY SELF CH)
             (IF (AND (EQ? SELF
                           (READ-TABLE-ENTRY *STANDARD-READ-TABLE*
                                             #\LEFT-PAREN))
                      (NULL? *LIST-BEGIN-CHAR*))
                 (SET *LIST-BEGIN-CHAR* CH)))
            ((LIST-TERMINATOR SELF) RIGHT)
            ((PRINT-TYPE-STRING SELF) "List-reader"))))

(DEFINE-OPERATION (LIST-TERMINATOR SYN))

(LET ((READER (MAKE-LIST-READER)))
  (SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE*
                        #\LEFT-PAREN
                        READER)
  (SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE*
                        #\RIGHT-PAREN
                        (LIST-TERMINATOR READER)))

(DEFINE (READ-DELIMITED-LIST STREAM TOKEN RT)
  (LET ((LOSING-RIGHT-BRACKET
         (LAMBDA ()
           ;; Ends in wrong kind of token.  This is the place to implement
           ;; super-brackets, if we ever decide that we want them.
           (READ-ERROR STREAM
                       "right bracket doesn't match left bracket")))
        (LOSING-EOF
         (LAMBDA ()
           (READ-ERROR STREAM
                       "end of file inside list (missing right bracket)"))))
    (ITERATE LOOP ((L '()))
      (LET ((OBJ (READ-THING STREAM RT)))
        (COND ((EOF? OBJ) (LOSING-EOF))
              ((CLOSE-TOKEN? OBJ)
               ;; List ends with right paren
               (COND ((NEQ? OBJ TOKEN)
                      (LOSING-RIGHT-BRACKET))
                     (ELSE (REVERSE! L))))
              ((DOT-TOKEN? OBJ)
               (LET ((TAIL (READ-OBJECT-REFUSING-EOF STREAM RT)))
                 (LET ((Z (READ-THING-REFUSING-DOTS STREAM RT))) ;Expect close.
                   (COND ((EOF? Z) (LOSING-EOF))
                         ((EQ? Z TOKEN)
                          ;; List ends <.> <frob> <rparen>
                          (APPEND-REVERSE! L TAIL))
                         ((CLOSE-TOKEN? Z)
                          (LOSING-RIGHT-BRACKET))
                         (ELSE
                          (READ-ERROR STREAM "two objects follow dot in list"))
                         ))))
              (ELSE (LOOP (CONS OBJ L))))))))

(DEFINE READ-DELIMITED-STRING
  (OBJECT (LAMBDA (STREAM CH RT)
            (LET ((BUFFER (GET-BUFFER)))
              (READ-DELIMITED-STRING-INTO-BUFFER STREAM CH RT BUFFER)
              (BLOCK0 (COPY-STRING BUFFER)
                      (RELEASE-BUFFER BUFFER))))
          ((ESTABLISH-READ-TABLE-ENTRY SELF CH)
           (IF (NULL? *STRING-DELIMITER*)
               (SET *STRING-DELIMITER* CH)))))

(DEFINE (READ-DELIMITED-STRING-INTO-BUFFER STREAM DELIMITER RT BUFFER)
  (ITERATE LOOP ()
    (LET ((CH (READ-CHAR STREAM)))
      (COND ((EOF? CH)
	     (READ-ERROR STREAM
			 "end of file within ~C...~C (missing delimiter)"
			 DELIMITER
			 DELIMITER)
	     BUFFER)
	    ((CHAR= CH DELIMITER) BUFFER)
	    (ELSE
	     (BUFFER-WRITEC BUFFER
			    (IF (FX= (CHAR-SYNTAX RT CH) %%ESCAPE-CHAR)
				(READ-CHAR STREAM)
				CH))
	     (LOOP))))))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\DOUBLEQUOTE READ-DELIMITED-STRING)

(DEFINE READ-DELIMITED-SYMBOL
  (OBJECT (LAMBDA (STREAM CH RT)
            (LET ((BUFFER (GET-BUFFER)))
              (READ-DELIMITED-STRING-INTO-BUFFER STREAM CH RT BUFFER)
              (BLOCK0 (STRING->SYMBOL BUFFER)
                      (RELEASE-BUFFER BUFFER))))
          ((ESTABLISH-READ-TABLE-ENTRY SELF CH)
           (IF (NULL? *SYMBOL-DELIMITER*)
               (SET *SYMBOL-DELIMITER* CH)))))

;(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\| READ-DELIMITED-SYMBOL)

(DEFINE READ-COMMENT
  (OBJECT (LAMBDA (STREAM CH RT)
            (IGNORE CH RT)
	    (ITERATE LOOP ()
	      (LET ((CH (READ-CHAR STREAM)))
		(COND ((EOF? CH) CH)
		      ((CHAR= CH #\NEWLINE) *NOTHING-READ*)
		      (ELSE (LOOP))))))
          ((DELIMITING-READ-MACRO? SELF) T)))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\SEMICOLON READ-COMMENT)

(DEFINE (READ-QUOTATION STREAM CH RT)
  (IGNORE CH)
  (LIST *QUOTE* (READ-OBJECT-REFUSING-EOF STREAM RT)))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\QUOTE READ-QUOTATION)

(DEFINE (READ-BACKQUOTE STREAM CH RT)
  (IGNORE CH)
  (LIST *BACKQUOTE* (READ-OBJECT-REFUSING-EOF STREAM RT)))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\BACKQUOTE READ-BACKQUOTE)

(DEFINE (READ-COMMA STREAM CH RT)
  (IGNORE CH)
  (LIST (COND ((CHAR= (PEEK-CHAR STREAM) #\@)
               (READ-CHAR STREAM)
               *COMMA-ATSIGN*)
              (ELSE *COMMA*))
        (READ-OBJECT-REFUSING-EOF STREAM RT)))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\COMMA READ-COMMA)

;;; ---------- Sharpsign

(DEFINE (MAKE-DISPATCH-READ-MACRO)
  (MAKE-DISPATCH-READ-MACRO-1 (VECTOR-FILL (MAKE-VECTOR *NUMBER-OF-CHAR-CODES*)
                                           NIL)))

(DEFINE (MAKE-DISPATCH-READ-MACRO-1 DISPATCH-TABLE)
  (OBJECT (LAMBDA (STREAM CH RT)
            (LET ((NEXTCH (READ-CHAR STREAM)))
              ;; Should read a number here, for #nRfoo.
              (LET ((FN (VREF DISPATCH-TABLE (CHAR->ASCII NEXTCH))))
                (COND (FN (FN STREAM NEXTCH NIL RT))
                      (ELSE (READ-ERROR STREAM
                                        "\"~C\" is an unknown ~C dispatch"
                                        NEXTCH CH))))))
          ((DISPATCH-SYNTAX SELF CH)
           (VREF DISPATCH-TABLE (CHAR->ASCII CH)))
          ((SET-DISPATCH-SYNTAX SELF CH FN)
           (COND ((LOWERCASE? CH)
                  (VSET DISPATCH-TABLE
                        (CHAR->ASCII (CHAR-UPCASE CH))
                        FN))
                 ((UPPERCASE? CH)
                  (VSET DISPATCH-TABLE
                        (CHAR->ASCII (CHAR-DOWNCASE CH))
                        FN))
                 ((DIGIT? CH 10.)
                  (ERROR "can't set a digit's dispatch-macro syntax")))
           (VSET DISPATCH-TABLE (CHAR->ASCII CH) FN))
          ((ESTABLISH-READ-TABLE-ENTRY SELF CH)
           (IF (NULL? *DISPATCH-CHAR*)
               (SET *DISPATCH-CHAR* CH)))
          ((COPY-READ-TABLE-ENTRY SELF)
           (MAKE-DISPATCH-READ-MACRO-1 (COPY-VECTOR DISPATCH-TABLE)))
          ((DISPATCHER? SELF) T)))

(DEFINE-SETTABLE-OPERATION (DISPATCH-SYNTAX TABLE CH))
(DEFINE SET-DISPATCH-SYNTAX (SETTER DISPATCH-SYNTAX))
(DEFINE-OPERATION (COPY-DISPATCHER TABLE))
(DEFINE-PREDICATE DISPATCHER?)

(DEFINE READ-DISPATCH (MAKE-DISPATCH-READ-MACRO))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\# READ-DISPATCH)

;;; #\c, #\foo - funny character reader

(DEFINE (READ-CHARACTER STREAM CH N RT)
  (IGNORE N)
  (LET ((Q (PEEK-CHAR STREAM)))
    (COND ((ALPHABETIC? Q)
           (LET ((PROBE (READ-OBJECT STREAM RT)))
             (COND ((NOT (SYMBOL? PROBE))
                    (READ-ERROR STREAM
                                "utter randomness in READ-CHARACTER - read ~S"
                                PROBE))
                   ((FX= (STRING-LENGTH (SYMBOL-PNAME PROBE)) 1) Q)
                   ((NAME-CHAR PROBE))
                   (ELSE (READ-ERROR STREAM "#~C~S: unknown #~C form"
                                     CH PROBE CH)))))
          (ELSE (READ-CHAR STREAM)))))

(SET-DISPATCH-SYNTAX READ-DISPATCH #\\ READ-CHARACTER)

;;; #t, #f - true and false

(SET-DISPATCH-SYNTAX READ-DISPATCH #\t (LAMBDA X (IGNORE X) *TRUE-OBJECT*))
(SET-DISPATCH-SYNTAX READ-DISPATCH #\f FALSE)

;;; #b nnn, #o nnn, #x nnn - alternate radices

(DEFINE (MAKE-RADICAL-READER RADIX)
  (LAMBDA (STREAM CH N RT)
    (IGNORE CH N)
    (READ-OBJECT-REFUSING-EOF STREAM (RT-WITH-RADIX RT RADIX))))

(SET-DISPATCH-SYNTAX READ-DISPATCH #\b (MAKE-RADICAL-READER 2.))

(SET-DISPATCH-SYNTAX READ-DISPATCH #\o (MAKE-RADICAL-READER 8.))

(SET-DISPATCH-SYNTAX READ-DISPATCH #\x (MAKE-RADICAL-READER 16.))

(SET-DISPATCH-SYNTAX READ-DISPATCH #\r
                     (LAMBDA (STREAM CH N RT)
                       (IGNORE CH)
                       (READ-OBJECT-REFUSING-EOF STREAM
                                                 (RT-WITH-RADIX RT N))))

;;; #^x - control character

(SET-DISPATCH-SYNTAX READ-DISPATCH #\^
                     (LAMBDA (STREAM CH N RT)
                       (IGNORE CH N RT)
                       (CONTROLIFY (READ-CHAR STREAM))))

;;; #(a b c ...) - vector syntax
;;; Extremely kludgey definition

(DEFINE (READ-VECTOR STREAM CH N RT)
  (IGNORE N)
  (LIST->VECTOR ((READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\LEFT-PAREN)
                 STREAM CH RT)))

(SET-DISPATCH-SYNTAX READ-DISPATCH #\LEFT-PAREN READ-VECTOR)

;;; #[keyword stuff ...] - general rereadable object

(DEFINE READ-TO-RIGHT-BRACKET (MAKE-LIST-READER))

(SET-READ-TABLE-ENTRY *STANDARD-READ-TABLE* #\RIGHT-BRACKET
                      (LIST-TERMINATOR READ-TO-RIGHT-BRACKET))

(DEFINE (READ-KEYWORDED STREAM CH N RT)
  (IGNORE CH N)
  (LET ((L (READ-TO-RIGHT-BRACKET STREAM CH RT)))
    (COND ((TABLE-ENTRY (RT-KEYWORD-TABLE RT) (CAR L))
           => (LAMBDA (PROC) (PROC L STREAM)))
          (ELSE
           (READ-ERROR STREAM "unknown #[...] syntax: #[~S ...]" (CAR L))))))

(SET-DISPATCH-SYNTAX READ-DISPATCH #\[ READ-KEYWORDED)

(DEFINE *READ-KEYWORD-TABLE* (MAKE-TABLE '*READ-KEYWORD-TABLE*))

(SET (RT-KEYWORD-TABLE *STANDARD-READ-TABLE*) *READ-KEYWORD-TABLE*)

(LET ((READ-ASCII
       (LAMBDA (L STREAM)
         (LET ((N (CADR L)))
           (COND ((OR (NULL? (CDR L))
                      (NOT (NULL? (CDDR L)))
                      (NOT (FIXNUM? N))
                      (FX< N 0)
                      (FX>= N *NUMBER-OF-CHAR-CODES*))
                  (READ-ERROR STREAM "illegal syntax - #[Ascii ~s]" N))
                 (ELSE
                  (ASCII->CHAR N)))))))
  (SET (TABLE-ENTRY *READ-KEYWORD-TABLE* 'CHAR)  READ-ASCII)
  (SET (TABLE-ENTRY *READ-KEYWORD-TABLE* 'ASCII) READ-ASCII))

(SET (TABLE-ENTRY *READ-KEYWORD-TABLE* 'SYMBOL)
     (LAMBDA (L STREAM) (IGNORE STREAM) (STRING->SYMBOL (CADR L))))

(SET (TABLE-ENTRY *READ-KEYWORD-TABLE* 'FILENAME)
     (LAMBDA (L STREAM)
       (MAKE-FILENAME-FOR-READ L STREAM)))

(DEFINE (READ-SYNTAX-DESCRIPTOR L STREAM)
  (LET ((SYM (CADR L)))
    (COND ((OR (NULL? (CDR L))
               (NOT (NULL? (CDDR L)))
               (NOT (SYMBOL? SYM)))
           (READ-ERROR STREAM "illegal syntax - #[Syntax ~s]" SYM))
          ((XCASE (CAR L)       ;Kludge
                  ((SYNTAX)
                   (SYNTAX-TABLE-ENTRY *STANDARD-SYNTAX-TABLE* SYM))
                  ((INTERNAL-SYNTAX)    ;insufficient error checking
                   (*VALUE *T-IMPLEMENTATION-ENV* SYM))))
          (ELSE
           (READ-ERROR STREAM
                       "not a standard reserved word - #[Syntax ~s]"
                       SYM)))))

(SET (TABLE-ENTRY *READ-KEYWORD-TABLE* 'SYNTAX)
     READ-SYNTAX-DESCRIPTOR)

(SET (TABLE-ENTRY *READ-KEYWORD-TABLE* 'INTERNAL-SYNTAX)
     READ-SYNTAX-DESCRIPTOR)

;;; #.expression - read-time evaluation

(SET-DISPATCH-SYNTAX READ-DISPATCH #\.  ; Bletch!  What to do?
                     (LAMBDA (STREAM CH N RT)
                       (IGNORE CH N)
                       (EVAL (READ-OBJECT-REFUSING-EOF STREAM RT)
                             (MAKE-LOCALE *STANDARD-ENV* '\#.))))

;;; ## reads as (CAR (REPL-RESULTS)).  Experimental feature.

(SET-DISPATCH-SYNTAX READ-DISPATCH #\#
		     (ALWAYS '(CAR (REPL-RESULTS))))

(SET-IMMUTABLE *STANDARD-READ-TABLE*)
