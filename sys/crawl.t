(HERALD (TSYS CRAWL T 119)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Structure and stack crawler

(LSET *CRAWL-ENV*   NIL)
(LSET *CRAWL-STACK* NIL)
(LSET *CRAWL-QUIT*  NIL)
(LSET *CRAWL-ARGS*  NIL)

;;; Top-level entry for CRAWL.

(DEFINE (*CRAWL ENV . OBJECTS)
  (CATCH QUIT
         (BIND ((*PRINT-LEVEL* 4)
                (*PRINT-LENGTH* 6)
                (*CRAWL-QUIT* QUIT)     ; only for benefit of Q command
                (*CRAWL-STACK* '())
                (*CRAWL-ARGS* NIL)
                (*CRAWL-ENV* ENV))
           (WALK CRAWL-PUSH (REVERSE! OBJECTS))
           (**BREAKPOINT NIL CRAWL-COMMAND-LOOP))))

;;; Prompt before reading new command line.
;;; Don't prompt after an empty command line.
;;; Do all commands read on one line.

(DEFINE (CRAWL-COMMAND-LOOP IN OUT)
  (ITERATE NEXT-COMMAND-LINE ((PREVIOUS *CRAWLING-NOTHING*) (HACK T))
    (LET ((OBJ (COND ((NULL? *CRAWL-STACK*) *CRAWLING-NOTHING*)
                     (ELSE (CAR *CRAWL-STACK*)))))
      (COND (HACK
             (COND ((NEQ? OBJ PREVIOUS)
                    (CRAWL-SYNOPSIS OBJ (OUT))))
             (FRESH-LINE (OUT))
             (DISPLAY (CRAWL-PROMPT OBJ) (OUT))))
      (LET ((LINE (READ-LINE (IN))))
        (IF (EOF? LINE) *REPL-WONT-PRINT*       ; exit
          (LET ((L (READ-OBJECTS-FROM-STRING LINE)))
            (COND ((NULL? L)
                   (NEXT-COMMAND-LINE OBJ NIL))
                  (ELSE
                   (SET *CRAWL-ARGS* L)
                   (ITERATE LOOP ()
                     (COND ((NULL? *CRAWL-ARGS*)
                            (NEXT-COMMAND-LINE OBJ T))
                           (ELSE
                            (LET ((COMMAND (POP *CRAWL-ARGS*))
                                  (NEXT (CAR *CRAWL-STACK*)))
                              (COND ((GET-CRAWL-COMMAND COMMAND)
                                     => (LAMBDA (Z) (Z NEXT)))
                                    ((MAYBE-CRAWL-COMPONENT NEXT COMMAND))
                                    (ELSE
                                     (FORMAT (OUT) "Illegal command.~%")))
                              (LOOP)))))))))))))

(DEFINE *CRAWLING-NOTHING*
  (OBJECT NIL
          ((CRAWL-SYNOPSIS SELF OUT) (IGNORE OUT) NIL)  ; do nothing
          ((IDENTIFICATION SELF) '*CRAWLING-NOTHING*)))

(DEFINE-OPERATION (CRAWL-SYNOPSIS OBJ OUT)
  (PRINT-ONE-LINE OBJ OUT)
  (FRESH-LINE OUT)
  (COND ((FRAME? OBJ)
         (FRAME-PRINT-SYNOPSIS OBJ OUT))
        ((UNIT? OBJ)
         (FORMAT OUT " Source = ~A  Size = ~S"
                 (UNIT-SOURCE-FILE-NAME OBJ)
                 (CRAWL-UNIT-SIZE OBJ)))))

(DEFINE-OPERATION (CRAWL-PROMPT OBJ)
  (COND ((FRAME? OBJ) "debug: ")
        (ELSE         "crawl: ")))

;;; Predicate: returns true if a component with the given name was pushed.

(DEFINE-OPERATION (MAYBE-CRAWL-COMPONENT OBJ COMMAND)
  (COND ((AND (STRUCTURE? OBJ)
              (ANY (LAMBDA (X) (IF (EQ? COMMAND (SELECTOR-ID X)) X NIL))
                   (STYPE-SELECTORS (STRUCTURE-TYPE OBJ))))
         => (LAMBDA (SEL) (CRAWL-PUSH (SEL OBJ))))
        ((EXTEND? OBJ)
         (MAYBE-CRAWL-EXTEND-COMPONENT OBJ COMMAND))
        (ELSE NIL)))

(DEFINE (MAYBE-CRAWL-EXTEND-COMPONENT OBJ COMMAND)
  (LET ((INFO (EXTEND-SIZE-INFO OBJ)))
    (COND ((AND (FIXNUM? COMMAND)
                (OR (FRAME? OBJ)
                    (AND (PAIR? INFO)
                         (FX>= COMMAND 0)
                         (FX< COMMAND (CAR INFO)))
                    (AND (ATOM? INFO)
                         (FIXNUM? COMMAND))))
           (CRAWL-PUSH (XREF OBJ COMMAND)))
          (ELSE NIL))))

(DEFINE-METHODS HANDLE-PAIR
  ((MAYBE-CRAWL-COMPONENT PAIR COMMAND)
   (COND ((AND (NONNEGATIVE-FIXNUM? COMMAND)
               (FX< COMMAND (LENGTH PAIR)))
          (CRAWL-PUSH (NTH PAIR COMMAND)))
         (ELSE NIL))))

;;; The commands:

(DEFINE-LOCAL-SYNTAX (DEFINE-CRAWL-COMMAND PAT SYMBOL DOC . BODY)
  `(BLOCK (DEFINE-OPERATION ,PAT . ,BODY)
          (*DEFINE-CRAWL-COMMAND  ',SYMBOL
                                  ',DOC
                                  ,(CAR PAT))
          ',SYMBOL))

(DEFINE (*DEFINE-CRAWL-COMMAND SYMBOL DOC OP)
  (LET ((COM (OBJECT OP
                     ((PRINT-CRAWL-HELP SELF)
                      (FORMAT (TERMINAL-OUTPUT) "  ~A  ~A~%" SYMBOL DOC)))))
    (PUSH *THE-CRAWL-COMMANDS* COM)
    (SET (TABLE-ENTRY *CRAWL-COMMAND-TABLE* SYMBOL)
         COM)))

(LSET *THE-CRAWL-COMMANDS* '())

(DEFINE *CRAWL-COMMAND-TABLE* (MAKE-TABLE '*CRAWL-COMMAND-TABLE*))

(DEFINE (GET-CRAWL-COMMAND COMMAND)
  (IF (SYMBOL? COMMAND) (TABLE-ENTRY *CRAWL-COMMAND-TABLE* COMMAND) NIL))

(DEFINE-OPERATION (PRINT-CRAWL-HELP OBJ))

(DEFINE-CRAWL-COMMAND (CRAWL-HELP OBJ) ?
  "Print summary of inspector commands."
  (WALK PRINT-CRAWL-HELP *THE-CRAWL-COMMANDS*)
  (FORMAT (TERMINAL-OUTPUT)
          " The A, C, and E commands will prompt for an expression.~%"))

(DEFINE-CRAWL-COMMAND (CRAWL-APPLY OBJ) A
  "Apply a procedure to the current object."
  (LET ((Z (CRAWL-READ "Call what procedure? ")))
    (COND ((EOF? Z) NIL)
          (ELSE
	   (LET ((PROC (EVAL-IN-CRAWLED-ENV Z OBJ)))
	     (COND ((PROCEDURE? PROC)
		    (RECEIVE-VALUES CRAWL-RECEIVER
				    (LAMBDA () (PROC OBJ))))
		   (ELSE
		    (FORMAT (TERMINAL-OUTPUT) "~S is inapplicable.~%"
			    PROC))))))))

(DEFINE (CRAWL-RECEIVER . VALS)
  (COND ((NULL? VALS)
	 (FORMAT (TERMINAL-OUTPUT) "No values.~%"))
	((NOT (NULL? (CDR VALS)))
	 (FORMAT (TERMINAL-OUTPUT)
		 "~S values.  Successive U commands will inspect them.~%"
		 (LENGTH VALS))))
  (WALK CRAWL-PUSH (REVERSE! VALS)))

(DEFINE-CRAWL-COMMAND (CRAWL-BREAK OBJ) B
  "Enter a read-eval-print loop in an appropriate environment."
  (LET ((ENV (OR (GET-ENVIRONMENT OBJ)
                 (REPL-ENV))))
    (FORMAT (TERMINAL-OUTPUT) "~&Breakpoint in ~S~%  with *OBJ* = ~S~%"
	    ENV OBJ)
    (BREAKPOINT NIL
		(EVAL `((LAMBDA (*OBJ*) (THE-ENVIRONMENT)) ',OBJ)
		      ENV))))

(DEFINE-CRAWL-COMMAND (CRAWL-CRAWL OBJ) C
  "Inspect another object."
  (LET ((Z (CRAWL-READ "Inspect what object? ")))
    (COND ((NOT (EOF? Z))
           (RECEIVE-VALUES CRAWL-RECEIVER
			   (LAMBDA () (EVAL-IN-CRAWLED-ENV Z OBJ)))))))

(DEFINE-CRAWL-COMMAND (CRAWL-DOWN OBJ) D
  "Go to next deeper continuation (i.e. stack frame)."
  (COND ((FRAME? OBJ)
         (LET ((PREV (FRAME-PREVIOUS OBJ)))
           (COND ((NULL? PREV)
                  (FORMAT (TERMINAL-OUTPUT)
                          "You are at the bottom of the stack.~%"))
                 (ELSE
                  (CRAWL-PUSH PREV)))))
        (ELSE (BAD-CRAWL-COMMAND))))
                         
(DEFINE-CRAWL-COMMAND (CRAWL-EVAL OBJ) E
  "Evaluate an expression in current object's environment."
  (LET ((Z (CRAWL-READ "Evaluate what? ")))
    (COND ((NOT (EOF? Z))
           ((REPL-PRINT) (EVAL-IN-CRAWLED-ENV Z OBJ) (TERMINAL-OUTPUT))
           (NEWLINE (TERMINAL-OUTPUT))))))

(DEFINE-CRAWL-COMMAND (CRAWL-SHOW-ENV OBJ) L
  "List values of lexical variables out to nearest locale."
  (CRAWL-EXHIBIT-ENV (GET-CRAWL-ENV OBJ)))

(DEFINE-OPERATION (CRAWL-EXHIBIT-ENV ENV)
  (FORMAT (TERMINAL-OUTPUT) "No local variables.~%"))

(DEFINE-CRAWL-COMMAND (CRAWL-MACRO-EXPAND OBJ) M
  "Macro-expand current object, and pretty-print the expansion."
  (LET ((X (MACRO-EXPAND OBJ (ENV-SYNTAX-TABLE (GET-CRAWL-ENV OBJ)))))
    (CRAWL-PP X)
    (CRAWL-PUSH X)))

(DEFINE (CRAWL-PP OBJ)
  (LET ((OUT (TERMINAL-OUTPUT)))
    (FRESH-LINE OUT)
    (PRETTY-PRINT OBJ OUT)
    (FRESH-LINE OUT)))

(DEFINE-CRAWL-COMMAND (CRAWL-PRINT OBJ) P
  "Pretty-print current object."
  (CRAWL-PP (OR (COND ((FRAME? OBJ) (FRAME-DISCLOSE OBJ))
                      (ELSE (DISCLOSE OBJ)))
                OBJ)))

(DEFINE-CRAWL-COMMAND (CRAWL-QUIT OBJ) Q
  "Exit the inspector."
  (*CRAWL-QUIT* *REPL-WONT-PRINT*))

(DEFINE-CRAWL-COMMAND (CRAWL-RETURN OBJ) R
  "Return a value to a continuation, continuing execution at that point."
  (LET ((REALLY-RETURN
         (LAMBDA (FRAME)
           (LET ((VAL (CRAWL-READ "Return what value? (EOF to abort) ")))
             (COND ((EOF? VAL) NIL)
                   (ELSE (FRAME-THROW FRAME (EVAL-IN-CRAWLED-ENV VAL OBJ))))))))
    (COND ((FRAME? OBJ)
           (REALLY-RETURN OBJ))
          ((ESCAPE-PROCEDURE? OBJ)
           (REALLY-RETURN (ESCAPE-PROCEDURE-FRAME OBJ)))
          (ELSE
           (FORMAT (TERMINAL-OUTPUT) "R is meaningless here.~%")))))

(DEFINE-CRAWL-COMMAND (CRAWL-UP OBJ) U
  "Go back to inspecting previous object."
  (COND ((NULL? (CDR *CRAWL-STACK*))
         (FORMAT (TERMINAL-OUTPUT) "You can't go up from here.~%"))
        (ELSE
         (POP *CRAWL-STACK*))))

(DEFINE-CRAWL-COMMAND (CRAWL-TO-UNIT OBJ) V
  "Inspect current object's unit (compiled module)."
  (COND ((TEMPLATE? OBJ)
         (CRAWL-PUSH (TEMPLATE-UNIT OBJ)))
        ((EXTEND? OBJ)
         (CRAWL-PUSH (TEMPLATE-UNIT (EXTEND-TEMPLATE OBJ))))
        (ELSE (BAD-CRAWL-COMMAND))))

(DEFINE-CRAWL-COMMAND (CRAWL-WHERE-DEFINED OBJ) W
  "Give file name of current object's definition."
  (FORMAT (TERMINAL-OUTPUT) "~S~%" (WHERE-DEFINED OBJ)))

(DEFINE-CRAWL-COMMAND (CRAWL-EXHIBIT OBJ) X
  "Display object's contents or other relevant information."
  (CRAWL-EXHIBIT-RANDOM OBJ))

(DEFINE-CRAWL-COMMAND (CRAWL-DISPLAY OBJ) =
  "Print object, its hash, and its address."
  (FORMAT (TERMINAL-OUTPUT) " ~S~_=~_~S~_=~_(~S #x~X)~2_<~S>~%"
          OBJ
          `(UNHASH ,(OBJECT-HASH OBJ))
          'INTEGER->POINTER
          (POINTER->INTEGER OBJ)
          (OR (POINTS-TO-REASONABLE-MEMORY? OBJ)
              'RANDOM)))

;;; Utilities.

(DEFINE (CRAWL-PUSH OBJ)                ; Must return true.
  (PUSH *CRAWL-STACK* OBJ)
  T)

(DEFINE (GET-CRAWL-ENV OBJ)
  (OR (AND (NOT (STRUCTURE? OBJ)) (GET-ENVIRONMENT OBJ)) *CRAWL-ENV*))

;;; Careful!  This may return multiple values.

(DEFINE (EVAL-IN-CRAWLED-ENV FORM OBJ)
  ;; If stack frame, we should bind the dynamic context, yes?
  ((REPL-EVAL) FORM (GET-CRAWL-ENV OBJ)))

(DEFINE (CRAWL-READ PROMPT)
  (COND ((NULL? *CRAWL-ARGS*)
         (FORMAT (TERMINAL-OUTPUT) PROMPT)
         (READ (TERMINAL-INPUT)))
        (ELSE (POP *CRAWL-ARGS*))))

;;; The moby X command.

(DEFINE (CRAWL-EXHIBIT-RANDOM OBJ)
  (COND ((FRAME? OBJ)
         (COND ((STANDARD-FRAME? OBJ)
                (GET-FRAME-SIZE-INFO OBJ EXHIBIT-FRAME-CONTENTS))
               ((FAULT-FRAME? OBJ)
                (EXHIBIT-FAULT-FRAME OBJ))
               (ELSE
                (FORMAT (TERMINAL-OUTPUT)
                        "Template with nonstandard format.~%"))))
        ((UNIT? OBJ)
         (EXHIBIT-UNIT OBJ))
        ((VECTOR? OBJ)
         (EXHIBIT-STANDARD-EXTEND OBJ
                                  (VECTOR-LENGTH OBJ)
                                  0))
        ((STRUCTURE? OBJ)
         (EXHIBIT-STRUCTURE OBJ))
        ((EXTEND? OBJ)
         (LET ((SIZE-INFO (EXTEND-SIZE-INFO OBJ)))
           (COND ((PAIR? SIZE-INFO)
                  (EXHIBIT-STANDARD-EXTEND OBJ
                                           (CAR SIZE-INFO)
                                           (CDR SIZE-INFO)))
                 (ELSE
                  (FORMAT (TERMINAL-OUTPUT)
                          "Extend with nonstandard format.~%"))
                 )))
        ((TEMPLATE? OBJ)
         (EXHIBIT-TEMPLATE OBJ))
        (ELSE (BAD-CRAWL-COMMAND))))

(DEFINE (EXHIBIT-FRAME-CONTENTS FRAME X Y Z)
  (DO ((I (FX+ X Y) (FX+ I 1))
       (END (FX+ (FX+ X Y) Z)))
      ((FX>= I END) T)
    (CRAWL-PRINT-COMPONENT I (XREF FRAME I))))

(DEFINE (EXHIBIT-FAULT-FRAME OBJ)       ; Hair this up later!
  (FORMAT (TERMINAL-OUTPUT) "Fault frame.~%"))

(DEFINE (CRAWL-PRINT-COMPONENT SELECTOR OBJ)
  (LET ((OUT (TERMINAL-OUTPUT)))
    (FORMAT OUT " [~S] " SELECTOR)
    (PRINT-ONE-LINE OBJ OUT)
    (NEWLINE OUT)))

(DEFINE (EXHIBIT-STANDARD-EXTEND OBJ PTR-SIZE SCR-SIZE)
  (ITERATE LOOP ((I 0) (PREVIOUS NIL) (REPEATING? NIL))
    (COND ((FX>= I PTR-SIZE)
           (COND ((FX> SCR-SIZE 0)
                  (IF (FX> PTR-SIZE 0) (FORMAT (TERMINAL-OUTPUT) " and"))
                  (FORMAT (TERMINAL-OUTPUT) " ~S scratch slots~%" SCR-SIZE))))
          (ELSE
           (LET ((THING (XREF OBJ I)))
             (COND ((OR (NEQ? THING PREVIOUS)
                        (FX= I 0)
                        (FX= I (FX- PTR-SIZE 1)))
                    (CRAWL-PRINT-COMPONENT I THING)
                    (LOOP (FX+ I 1) THING NIL))
                   ((NOT REPEATING?)
                    (FORMAT (TERMINAL-OUTPUT) " ...~%")
                    (LOOP (FX+ I 1) THING T))
                   (ELSE
                    (LOOP (FX+ I 1) THING T))))))))

(DEFINE (EXHIBIT-UNIT UNIT)
   (LET ((SIZE (CRAWL-UNIT-SIZE UNIT)))
     (ITERATE LOOP ((I 0))
       (COND ((FX>= I SIZE) T)
             ((NOT (REASONABLE? (XREF UNIT I)))
              (LOOP (FX+ I (IF (ODD? I) 2 1))))
             ((TEMPLATE? (XREF UNIT I))
              (LOOP (FX+ I 1)))
             ;((AND (TEMPLATE? (XREF UNIT I))
                ;   (ODD? I)
                ;   (EQ? (XREF UNIT I)
                ;       (MAKE-POINTER (%XLOC UNIT (FX- I 2)) %%TEMPLATE-TAG)))
              ;(FORMAT (TERMINAL-OUTPUT) " Tproc ~S~%" (%XLOC UNIT (FX+ I 1)))
              ;(LOOP (FX+ I 1)))
             (ELSE
              (IF (NOT (FIXNUM? (XREF UNIT I)))
                  (CRAWL-PRINT-COMPONENT I (XREF UNIT I)))
              (LOOP (FX+ I 1)))))))


(DEFINE (CRAWL-UNIT-SIZE UNIT)
  (CODE-STRUCTURE-AREA-INDEX (UNIT-CODE UNIT)))

(DEFINE-METHODS HANDLE-STRING
  ((CRAWL-EXHIBIT STRING)
   (FORMAT (TERMINAL-OUTPUT) " Header addr = #x~X, base = #x~X, length = ~D~%"
           (POINTER->INTEGER STRING)
           (STRING-BASE STRING)
           (STRING-LENGTH STRING))
   (FORMAT (TERMINAL-OUTPUT) " Text addr = #x~X, length = ~D~%"
           (POINTER->INTEGER (STRING-POINTER STRING))
           ;; This may give completely nonsensical results on the VAX.
           (TEXT-LENGTH (STRING-POINTER STRING)))))

(DEFINE-METHODS HANDLE-FIXNUM
  ((CRAWL-EXHIBIT N)
   (FORMAT (TERMINAL-OUTPUT) " ~D = #x~X = #o~O = #b~B = (INTEGER->POINTER #x~X)"
           N N N N (POINTER->INTEGER N))
   (COND ((AND (FX>= N 0) (FX<= N *NUMBER-OF-CHAR-CODES*))
          (FORMAT (TERMINAL-OUTPUT) " = (CHAR->ASCII ~S)" (ASCII->CHAR N))))
   (NEWLINE (TERMINAL-OUTPUT))))

(DEFINE-METHODS HANDLE-CHAR
  ((CRAWL-EXHIBIT CH)
   (LET ((N (CHAR->ASCII CH)))
     (FORMAT (TERMINAL-OUTPUT) " ASCII: decimal ~D, hex ~X, octal ~O~%"
             N N N))))

;;; Definition of EXHIBIT-TEMPLATE has moved to CHUNK.

(DEFINE (BAD-CRAWL-COMMAND)
  (FORMAT (TERMINAL-OUTPUT)
          "There is no way to go in that direction.~%"))

;;; Reverse command list for prettiness in HELP.

(SET *THE-CRAWL-COMMANDS* (REVERSE! *THE-CRAWL-COMMANDS*))
