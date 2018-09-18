(HERALD (TSYS REPL T 136)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; System initialization and read-eval-print loop

;;; (BREAKPOINT [message [env]]) simply punts to *BREAKPOINT.

(DEFINE (BREAKPOINT . ARGS)
  (COND ((NULL? ARGS)
         (*BREAKPOINT NIL))
        ((OR (NULL? (CDR ARGS)) (NULL? (CADR ARGS)))
         (*BREAKPOINT (CAR ARGS)))
        (ELSE
         (BIND (((REPL-ENV) (CHECK-ARG ENVIRONMENT? (CADR ARGS) BREAKPOINT)))
           (*BREAKPOINT (CAR ARGS))))))

(DEFINE (*BREAKPOINT MESSAGE)
  (**BREAKPOINT MESSAGE READ-EVAL-PRINT-LOOP))

(DEFINE *STANDARD-TOP-LEVEL*
  (LAMBDA ()
    (SET *Z?* NIL)                      ; ?
    (RESET-STACK-GUARD)
    (BREAKPOINT *TOP-LEVEL-GREETING*)))

(SET *TOP-LEVEL* *STANDARD-TOP-LEVEL*)
    
(LSET *TOP-LEVEL-GREETING* "Top level")

(DEFINE (T-RESET)
  (SET *TOP-LEVEL* *STANDARD-TOP-LEVEL*)
  (**RESET** NIL))

;;; The most weird control structure in the system.

(DEFINE (**BREAKPOINT MESSAGE REPL)
  (CATCH RET
         (IF MESSAGE (FORMAT (REPL-OUTPUT) "~&~A" MESSAGE))
         (CATCH UP
                (LET ((PREVIOUS-UP **UP**))
                  (BIND ((*BREAK-LEVEL* (FX+ *BREAK-LEVEL* 1))
                         (**UP** UP)
                         (**RET** RET))
                    (REPL REPL-INPUT REPL-OUTPUT)
                    (PREVIOUS-UP NIL))))
         ;; A throw to UP comes here.
         (**BREAKPOINT NIL REPL)))

(LSET **UP** (LAMBDA (#F) (LUSER-TYPED-EOF-AT-TOP-LEVEL)))

;;; Entry from ERROR.  [Why doesn't ERROR just call BREAKPOINT directly?]

(DEFINE (ERROR-BREAKPOINT . STUFF) (APPLY BREAKPOINT STUFF))

(DEFINE (RET . MAYBE-VALUE)
  (**RET** (IF MAYBE-VALUE (CAR MAYBE-VALUE) NIL)))

;;; Read-eval-print loop.
;;; Typing end-of-file (^Z or ^D) is the only way this can ever return.

(DEFINE (READ-EVAL-PRINT-LOOP IN OUT)
  (ITERATE LOOP ()
    (FRESH-LINE (OUT))
    (DISPLAY ((REPL-PROMPT) *BREAK-LEVEL*) (OUT))
    (LET ((FORM ((REPL-READ) (IN))))
      (COND ((EOF? FORM) FORM)
            (ELSE
             (RECEIVE VALS
		      ;; Evaluate the user's form.
		      ((REPL-EVAL) FORM (REPL-ENV))
	       (COND ((NULL? VALS)
		      (FORMAT (OUT) ";No values."))
		     ((NOT (NULL? (CDR VALS)))
		      (SET (REPL-RESULTS) VALS)
		      (FORMAT (OUT) ";Multiple values:")
		      (DO ((L VALS (CDR L))
			   (I 0 (FX+ I 1)))
			  ((NULL? L))
			(FORMAT (OUT) "~% [~S] " I)
			((REPL-PRINT) (CAR L) (OUT))))
		     ((NOT (REPL-WONT-PRINT? (CAR VALS)))
		      ;; Single value
		      (SET (REPL-RESULTS) VALS)
		      ((REPL-PRINT) (CAR VALS) (OUT)))))
             (LOOP))))))

;;; Modularity wins again.

(DEFINE (SIMPLE-SWITCH TYPE ID)
  (LET* ((VAL '*UNDEFINED*)
         (S (LAMBDA (NEW)
              (SET VAL
                   (IF (EQ? NEW '*UNDEFINED*)        ;Make BIND work?
                       NEW
                       (CHECK-ARG TYPE NEW ID))))))
    (OBJECT (LAMBDA () VAL)
            ((SETTER SELF) S)
            ((IDENTIFICATION SELF) ID))))

(DEFINE REPL-RESULTS
  (SIMPLE-SWITCH LIST? 'REPL-RESULTS))

(DEFINE REPL-PROMPT
  (SIMPLE-SWITCH PROCEDURE? 'REPL-PROMPT))

(DEFINE REPL-READ
  (SIMPLE-SWITCH PROCEDURE? 'REPL-READ))

(DEFINE REPL-EVAL
  (SIMPLE-SWITCH PROCEDURE? 'REPL-EVAL))

(DEFINE REPL-PRINT
  (SIMPLE-SWITCH PROCEDURE? 'REPL-PRINT))

(DEFINE REPL-OUTPUT
  (SIMPLE-SWITCH OUTPUT-STREAM? 'REPL-OUTPUT))

(DEFINE REPL-INPUT
  (SIMPLE-SWITCH INPUT-STREAM? 'REPL-INPUT))

(DEFINE REPL-ENV
  (SIMPLE-SWITCH ENVIRONMENT? 'REPL-ENV))

(DEFINE (INITIALIZE-REPL ENV)
  (SET (REPL-RESULTS)    '(**))
  (SET (REPL-PROMPT)     STANDARD-PROMPT)
  (SET (REPL-READ)       READ)
  (SET (REPL-EVAL)       EVAL)
  (SET (REPL-PRINT)      PRINT)
  (SET (REPL-INPUT)      (TERMINAL-INPUT))
  (SET-DEPENDENT TERMINAL-INPUT REPL-INPUT)     ; see STANDARD.T
  (SET (REPL-OUTPUT)     (TERMINAL-OUTPUT))
  (SET-DEPENDENT TERMINAL-OUTPUT REPL-OUTPUT)
  (SET (REPL-ENV)        ENV))

;;; Random stuff.

(DEFINE (STANDARD-PROMPT LEVEL)         ; Arg is # of repls on stack.
  (CASE LEVEL
    ((1) "> ")
    ((2) ">> ")
    ((3) ">>> ")
    ((4) ">>>> ")
    (ELSE (STRING-APPEND (MAP-STRING! (ALWAYS #\>) (MAKE-STRING LEVEL)) " "))))

(DEFINE (ALTERNATE-PROMPT LEVEL)
  (CASE LEVEL
    ((1) "> ")
    ((2) "1: ")
    ((3) "2: ")
    ((4) "3: ")
    (ELSE (FORMAT NIL "~S: " (FX- LEVEL 1)))))

(DEFINE-PREDICATE REPL-WONT-PRINT?)

(DEFINE *REPL-WONT-PRINT*
  (OBJECT NIL
          ((REPL-WONT-PRINT? SELF) T)
          ((IDENTIFICATION SELF) '*REPL-WONT-PRINT*)))

;;; Some commands.

(DEFINE-INTEGRABLE (CURRENT-FRAME)
  (ESCAPE-PROCEDURE-FRAME **RET**))

(DEFINE (BACKTRACE)
  (*BACKTRACE (CURRENT-FRAME)))

(DEFINE (CRAWL . REST)
  (APPLY *CRAWL (REPL-ENV) REST))

(DEFINE (DEBUG)
  (*CRAWL (REPL-ENV) (CURRENT-FRAME)))

(DEFINE-SYNTAX (PP FORM)
  (COND ((SYMBOL? FORM)
         `(*PP-SYMBOL ',FORM (REPL-ENV)))
        (ELSE
         `(*PP ,FORM))))

;;; Initialize for availability during boot sequence.

(INITIALIZE-REPL *THE-BOOT-ENV*)
