(HERALD (TCOMP RANDOM T 241)
        (ENV TCOMP))

;;; Copyright (c) 1983, 1984 Yale University


;;;; Compiler Utilities

;;; To do:
;;;  - Fix environment stuff.
;;;  - Fix noise-output stuff.

;;; Incredibly disorganized.


(DEFINE (STATIC-APPLICABLE? STATIC)
  (LET ((PROBE (ENV-LOOKUP *STANDARD-ENV* (STATIC-IDENTIFIER STATIC) NIL NIL)))
    (AND PROBE (PROCEDURE? (CONTENTS PROBE)))))

(DEFINE (STATIC->PROCEDURE STATIC)
  (CONTENTS (ENV-LOOKUP *STANDARD-ENV* (STATIC-IDENTIFIER STATIC) NIL NIL)))

;;; Print values of non-zero statistics counters

(DEFINE (STATS)
  (BIND ((*NOISE-OUTPUT* *NOISE-MINUS-TERMINAL*))
    (WALK (LAMBDA (X)
            (LET ((VAL (CONTENTS (CAR X))))
              (COND ((FX> VAL 0)
                     (FORMAT *NOISE-OUTPUT*
                             ";;; ~-7S  ~A~%"
                             VAL
                             (CDR X))))))
          *STAT-COUNTERS*)
    (NEWLINE *NOISE-OUTPUT*)))

(IMPORT *T-IMPLEMENTATION-ENV* VCELL-HAS-VALUE?)    ;cheat

(DEFINE (*DEFDISPATCH LOC TYPE PROC)
  (COND ((NOT (VCELL-HAS-VALUE? LOC))
         (SET-CONTENTS LOC (MAKE-VECTOR (LENGTH *NODE-TYPES*)))))
  (VSET (CONTENTS LOC) (POSQ TYPE *NODE-TYPES*) PROC)
  (IDENTIFICATION PROC))

(DEFINE (*DEFINE-STAT-COUNTER LOC DOCUMENTATION)
  (LET ((PROBE (ASSQ LOC *STAT-COUNTERS*)))
    (COND (PROBE
           (SET (CDR PROBE) DOCUMENTATION))
          (ELSE
           (SET *STAT-COUNTERS*
                (APPEND! *STAT-COUNTERS*
                         (LIST (CONS LOC DOCUMENTATION)))))))
  (SET-CONTENTS LOC 0)
  LOC)

(DEFINE (*DEFINE-LAP-CONSTANT NAME VALUE)
  (DECLARE (SPECIAL *LAP-CONSTANTS*))
  (SET *LAP-CONSTANTS* (ADJOIN NAME *LAP-CONSTANTS*))
  (CPUT NAME 'CONSTANT VALUE)
  (PUT NAME 'SYM VALUE))

;;; A queue is implemented as a dotted pair (tail . head).

(DEFINE (MAKE-QUEUE)
  (LET ((QUEUE (CONS '() '())))
    (SET (CAR QUEUE) QUEUE)
    QUEUE))

(DEFINE-INTEGRABLE (QUEUE-EMPTY? QUEUE)
  (NULL? (CDR QUEUE)))

(DEFINE (ENQUEUE QUEUE OBJ)
  (LET ((CELL (LIST OBJ)))
    (SET (CDR (CAR QUEUE)) CELL)
    (SET (CAR QUEUE) CELL)
    T))

(DEFINE (DEQUEUE QUEUE)
  (COND ((QUEUE-EMPTY? QUEUE)
         (ERROR "queue ran out" QUEUE 'FAIL-ACT))
        (ELSE
         (LET ((CELL (CDR QUEUE)))
           (SET (CDR QUEUE) (CDR CELL))
           (IF (EQ? CELL (CAR QUEUE)) (SET (CAR QUEUE) QUEUE))
           (CAR CELL)))))


;;; Useful photons

(DEFINE (MAKE-PHOTON PNAME)
  (OBJECT NIL
          ((PRINT SELF STREAM) (IGNORE SELF) (DISPLAY PNAME STREAM))))

(DEFINE *PRINTS-AS-EMPTY-LIST* (MAKE-PHOTON "()"))

(DEFINE *PRINTS-AS-ELLIPSIS*   (MAKE-PHOTON "..."))

(DEFINE (MAKE-PRINTABLE-STRUCTURE X DEPTH) (IGNORE DEPTH) X)

(DEFINE (NODE-PARENT-CHECK SON PARENT)
  (COND ((NEQ? (NODE-PARENT SON) PARENT)
         (BUGLET ((*SON* SON) (*PARENT* PARENT))
                 "parent pointer is incorrect"
                 "will forcibly (SET (NODE-PARENT *SON*) *PARENT*)")
         (SET (NODE-PARENT SON) PARENT))))

(DEFINE-PREDICATE FUNNY-CONSTANT?)


(DEFINE (SPRIN1T Z STREAM)
  (FRESH-LINE STREAM)
  (PRETTY-PRINT Z STREAM)
  (NEWLINE STREAM))

(DEFINE (SX NODE)
  (SPRIN1T (SEXPRFY NODE) (TERMINAL-OUTPUT))
  '*)

(DEFINE (SXL NODE-LIST)
  (WALK (LAMBDA (NODE) (SPRIN1T (SEXPRFY NODE) (TERMINAL-OUTPUT)))
        NODE-LIST)
  '*)

(DEFINE (SXNOISE NODE)
  (SPRIN1T (SEXPRFY NODE) *NOISE-OUTPUT*)
  '*)

;;; Random stuff: WARN, BUG

(LSET *ERROR-COUNT* 0)

(DEFINE (WARN ERRORMSG FIXMSG . ITEMS)
  (BLOCK0 (WARN-MENTION-HANDLER "Error" *NOISE-PLUS-TERMINAL*
				ERRORMSG FIXMSG ITEMS)
          (INCR *ERROR-COUNT*)))

(DEFINE (MENTION ERRORMSG FIXMSG . ITEMS)
  (WARN-MENTION-HANDLER "Warning" *NOISE-OUTPUT*
			ERRORMSG FIXMSG ITEMS))

(DEFINE (WARN-MENTION-HANDLER WARN-OR-MENTION STREAM ERRORMSG FIXMSG ITEMS)
  (BIND ((*PRINT-LEVEL* 3) (*PRINT-LENGTH* 6))
    (APPLY FORMAT STREAM
           (LIST "~&;~A: " ERRORMSG
                 (PHASEMSG)
                 "~&;Action: " FIXMSG
                 "~&;Location:")
           WARN-OR-MENTION
           ITEMS))
  (COND ((NULL? *WHERE*)
         (FORMAT STREAM " at top level"))
        (ELSE
         (DO ((W *WHERE* (CDR W)))
             ((NULL? W))
           (FORMAT STREAM " in ~S" (OR (IDENTIFICATION (CAR W)) (CAR W))))))
  (NEWLINE STREAM)
  '**INTERNAL-WARN-OR-MENTION-VALUE-FALL-THROUGH-BUG**)

(DEFINE (PHASEMSG)
  (CASE *PHASE*
    ((ALPHATIZE) "")
    ((META-EVALUATE)
     "~&;   (This was discovered during code optimization; ~
      these are reconstructed forms.)~%")
    (ELSE
     "~&;   (This was discovered in some obscure part of the compiler.)~%")))

(LSET *BATCH-MODE?* NIL)

(DEFINE (BUG ERRORMSG FIXMSG . ITEMS)
  (BIND ((*PRINT-LEVEL* 3) 
         (*PRINT-LENGTH* 6)
         (*NOISE-OUTPUT* *NOISE-PLUS-TERMINAL*))
    (APPLY FORMAT
           *NOISE-OUTPUT*
           `("~%;>>>> Internal compiler error!~%"
             ";>>>> Report this bug to the compiler implementors.~%"
             ,@(if *batch-mode?*
                   '()
                   '(";>>>> Type (RET) to attempt corrective action.~%"))
             ";Error: "
             ,ERRORMSG
             "~%;Action: "
             ,FIXMSG
             "~%")
           ITEMS)
    (IF (NOT *BATCH-MODE?*) (BREAKPOINT 'COMPILER-BUG *TC-ENV*))
    '**INTERNAL-BUG-VALUE-FALL-THROUGH-BUG**))

;;; Support environments and STATIC structures.

;;; Cells are defined as normal structures.

(DEFINE (CREATE-STATIC SYMBOL SE)
  (CONS-A-STATIC PROPERTIES '()
                 IDENTIFIER SYMBOL
                 NAMESPACE SE))

(DEFINE (MAKE-EMPTY-SUPPORT-ENV ID)
  (LET ((TABLE (MAKE-TABLE ID))
	(SELF))
    (SET SELF
	 (OBJECT (LAMBDA (SYMBOL CREATE?)
		   (OR (TABLE-ENTRY TABLE SYMBOL)
		       (COND (CREATE?
			      (LET ((STATIC (CREATE-STATIC SYMBOL SELF)))
				(SET (TABLE-ENTRY TABLE SYMBOL) STATIC)
				STATIC))
			     (ELSE NIL))))
		 ((CREAM SELF)   SELF)
		 ((PRINT SELF STREAM)
		  (IGNORE SELF)
		  (FORMAT STREAM "#{Support-contour~_~S}" ID))))
    SELF))

(DEFINE (MAKE-SUPPORT-ENV SUPER ID)    ;Exported
  (LET ((CONTOUR (MAKE-EMPTY-SUPPORT-ENV ID)))
    (OBJECT (LAMBDA (SYMBOL CREATE?)
	      (OR (CONTOUR SYMBOL NIL)
		  (SUPER SYMBOL NIL)
		  (AND CREATE? (CONTOUR SYMBOL CREATE?))))
	    ((CREAM SELF)  (IGNORE SELF) CONTOUR)
	    ((PRINT SELF STREAM)
	     (IGNORE SELF)
	     (FORMAT STREAM "#{Support-env~_~S~_~S}" (OBJECT-HASH SELF) ID)))))

(DEFINE-OPERATION (CREAM SELF))		;skim off just one contour

(DEFINE SUPPORT-ENV-ENTRY
  (OBJECT (LAMBDA (SE IDENTIFIER)
	    (SE IDENTIFIER NIL))
	  ((SETTER SELF)
	   (LAMBDA (SE IDENTIFIER VAL)
	     (LET ((VAL (IF VAL (CHECK-ARG STATIC? VAL SUPPORT-ENV-ENTRY) NIL))
		   (STATIC (LOCAL-LOOKUP SE IDENTIFIER T)))
	       (IF (NOT (NULL? (STATIC-PROPERTIES STATIC)))
		   (MENTION "redefining support environment entry for ~S"
			    "none" IDENTIFIER))
	       (SET (STATIC-PROPERTIES STATIC)
		    (IF VAL (STATIC-PROPERTIES VAL) '())))))))

(DEFINE (FREE-LOOKUP SE IDENTIFIER CREATE?)
  (SE IDENTIFIER CREATE?))
  
(DEFINE (LOCAL-LOOKUP SE IDENTIFIER CREATE?)
  (LET ((PROBE (SE IDENTIFIER NIL))      ; l-to-r evaluation
        (NEW ((CREAM SE) IDENTIFIER CREATE?)))
    (IF (AND PROBE (NEQ? PROBE NEW))
        (MENTION "shadowing binding of ~S"
                 "none - the situation is probably harmless"
                 IDENTIFIER))
    NEW))

;;; EASSQ is like (CDR (ASSQ ...)) but returns *EMPTY* if no entry was found.

(DEFINE (EASSQ PROP L)
  (LET ((Z (ASSQ PROP L))) (IF Z (CDR Z) *EMPTY*)))

(DEFINE (->STATIC P CREATE?)
  (COND ((SYMBOL? P)
         (*NAMESPACE* P CREATE?))
        ((STATIC? P) P)
        (ELSE (BUG "~S bad arg to CGET or CPUT"
                   "will use the symbol ->STATIC-LOSER instead"
                   P)
              (->STATIC '->STATIC-LOSER CREATE?))))

(DEFINE (CGET P PROP)
  (LET ((P (CGET-EMPTY P PROP)))
    (IF (EMPTY P) NIL P)))

(DEFINE (CGET-EMPTY P PROP)
  (LET ((STATIC (->STATIC P NIL)))
    (COND ((NULL? STATIC) *EMPTY*)
          (ELSE (EASSQ PROP (STATIC-PROPERTIES STATIC))))))

(DEFINE (CPUT P PROP NEWVAL)
  (CPUT-STATIC (->STATIC P T) PROP NEWVAL))

(DEFINE (CPUT-STATIC STATIC PROP VAL)
  (LET ((PROBE (ASSQ PROP (STATIC-PROPERTIES STATIC))))
    (COND (PROBE
           (SET (CDR PROBE) VAL))
          (ELSE
           (PUSH (STATIC-PROPERTIES STATIC) (CONS PROP VAL)))))
  VAL)

;;; Dreadful awful wrong definition.

(DEFINE (CREM P PROP)
  (CPUT P PROP *EMPTY*))

;;; Ugh!!  Maclisp PRINT methods for nodes etc.

(DEFINE-LOCAL-SYNTAX (DEFINE-PRINT-METHOD STYPE ARGS . BODY)
  `(DEFINE-METHODS ,(CONCATENATE-SYMBOL 'HANDLE- STYPE)
     ((PRINT ,@ARGS) ,@BODY)))

(DEFINE-PRINT-METHOD STATIC (SELF STREAM)
  (FORMAT STREAM "#{Static~_~S~_~S}"
          (OBJECT-HASH SELF)
          (STATIC-IDENTIFIER SELF)))

(DEFINE-PRINT-METHOD VARIABLE (SELF STREAM)
  (FORMAT STREAM "#{Variable~_~S~_~S}"
          (OBJECT-HASH SELF)
          (VARIABLE-IDENTIFIER SELF)))

(DEFINE-PRINT-METHOD NODE (SELF STREAM)
  (FORMAT STREAM "#{~S-node~_~S}"
          (STYPE (NODE-FORM SELF))
          (OBJECT-HASH SELF)))

(DEFINE-PRINT-METHOD TN (SELF STREAM)
  (FORMAT STREAM "#{TN~_~S~_=~S}"
          (OBJECT-HASH SELF)
          (TN-ID SELF)))

(DEFINE-PRINT-METHOD REGION (SELF STREAM)
  (FORMAT STREAM "#{Region~_~S}"
          (OBJECT-HASH SELF)))

;;; Randomness

(IMPORT *T-IMPLEMENTATION-ENV*
	MAYBE-CRAWL-COMPONENT
	*THE-BUCK-STOPS-HERE*
	DEFAULT-METHOD)

(DEFINE-METHODS HANDLE-NODE
  ((MAYBE-CRAWL-COMPONENT NODE COMMAND)
   (COND ((EQ? COMMAND 'SX) (SX NODE) T)
         ((MAYBE-CRAWL-COMPONENT (NODE-FORM NODE) COMMAND))
         (ELSE ((DEFAULT-METHOD MAYBE-CRAWL-COMPONENT)
		NODE DEFAULT-METHOD *THE-BUCK-STOPS-HERE* NODE COMMAND))))
  ((DISCLOSE SELF) (SEXPRFY SELF)))
