(HERALD (TCOMP ALPHATIZE T 449)
        (ENV TCOMP))

;;; Copyright (c) 1979, 1980 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Alpha Conversion

;;; *ENV* is an a-list of (symbol . variable-struct)

(DEFINE (ENVPUSH SYMBOL THING)
  (PUSH *ENV* (CONS SYMBOL THING)))

;;; The MAGIC property of a static structure is a syntax descriptor.

(DEFINE (GET-SYNTAX SYMBOL)
  (SYNTAX-TABLE-ENTRY *SYNTAX-TABLE* SYMBOL))

(DEFINE (*DEFINE-LOCAL-SYNTAX SYMBOL DESCR)
  (SET (SYNTAX-TABLE-ENTRY *SYNTAX-TABLE* SYMBOL) DESCR))

;;; Node creation

(STAT-COUNTER *NODE-COUNT* "number of internal-format program nodes created")

(DEFINE (CREATE-NODE) (CONS-A-NODE))

(DEFINE (NODIFY FORM)
  (DECLARE (SPECIAL *NODE-POOL*))
  (INCR *NODE-COUNT*)
  (LET ((NODE (COND (*POOL?* (OBTAIN-FROM-POOL *NODE-POOL*))
                    (ELSE (CONS-A-NODE)))))
    (SET (NODE-FORM NODE) FORM)
    NODE))

;;; Random utility: CAREFUL-CALL
;;; Really not very careful at all right now.  Wait for condition system.

(DEFINE (CAREFUL-CALL PROC SEXPR WHAT ERRVAL)
  (IGNORE WHAT ERRVAL)
  (PROC SEXPR))

;;; Alphatize the SEXPR in the top-level environment.
;;; Used only for dealing with integrable procedure integration.

(DEFINE (ALPHA-TOPLEVEL SEXPR)
  (BIND ((*ENV* '())
         (*SYNTAX-TABLE* *STANDARD-SYNTAX-TABLE*)
	 (*NAMESPACE* (MAKE-SUPPORT-ENV *STANDARD-SUPPORT-ENV* NIL)))   ;??!??
    (ALPHATIZE SEXPR)))

;;; This is the central dispatch for the alphatization phase of the compiler.
;;; *ENV*, *NAMESPACE*, and *SYNTAX-TABLE* are implicit parameters.

(DEFINE (ALPHATIZE SEXPR)
  (COND ((NOT (PAIR? SEXPR)) (ALPHA-ATOM SEXPR))
        ((SYMBOL? (CAR SEXPR))
         (LET ((PROBE (GET-SYNTAX (CAR SEXPR))))
           (COND ((NOT PROBE)
                  (ALPHA-COMBINATION (ALPHATIZE (CAR SEXPR))
                                     (CDR SEXPR)))
                 (ELSE
		  (CHECK-RESERVED-WORD-AMBIGUITY (CAR SEXPR))
                  (ALPHA-SPECIAL-FORM PROBE SEXPR)))))
        ((SYNTAX-DESCRIPTOR? (CAR SEXPR))
         (ALPHA-SPECIAL-FORM (CAR SEXPR) SEXPR))
        ((EQ? (CAR SEXPR) *INTERNAL*) (CDR SEXPR))   ;hack for meta-evaluator
        (ELSE
         (ALPHA-COMBINATION (ALPHATIZE (CAR SEXPR)) (CDR SEXPR)))))

(DEFINE (CHECK-RESERVED-WORD-AMBIGUITY SYMBOL)
  (IF (ASSQ SYMBOL *ENV*)
      (MENTION "a form begins with ~S, which is both a reserved word and bound"
	       "will treat the form as a special form, not as a call"
	       SYMBOL)))

(DEFINE (ALPHA-ATOM SEXPR)
  (COND ((SYMBOL? SEXPR)
         (ALPHA-VARIABLE SEXPR))
        ((OR (NUMBER? SEXPR)
             (STRING? SEXPR)
             (CHAR? SEXPR))
         (ALPHA-CONSTANT SEXPR))
	(ELSE
	 (ALPHATIZE ((ATOM-EXPANDER *SYNTAX-TABLE*) SEXPR)))))

(DEFINE (ALPHA-SPECIAL-FORM DESCR SEXPR)
  (LET ((NEW-SEXPR (CHECK-SPECIAL-FORM-SYNTAX DESCR SEXPR)))
    (COND ((NEQ? NEW-SEXPR SEXPR)
           (ALPHATIZE SEXPR))
	  ((TABLE-ENTRY *ALPHATIZER-TABLE* DESCR)
	   => (LAMBDA (PROC) (PROC NEW-SEXPR)))
          ((MACRO-EXPANDER? DESCR)
	   (BIND ((*WHERE* (CONS (CAR SEXPR) *WHERE*)))
	     (ALPHATIZE (EXPAND-MACRO-FORM DESCR SEXPR *SYNTAX-TABLE*))))
	  (ELSE
	   (WARN "don't know how to alpha-convert ~S special form"
		 "will ignore the form and use () in its stead"
		 (CAR SEXPR))
	   (ALPHA-CONSTANT NIL)))))

;;; The following is for compatibility with old support files.
;;; It can go away in a little while.  (14 Nov 83)

(DEFINE (MAKE-MACRO-EXPANDER PROC)
  (MAKE-MACRO-DESCRIPTOR PROC '(0 . T) (IDENTIFICATION PROC)))

(IMPORT *T-IMPLEMENTATION-ENV*
  OBTAIN-SYNTAX-TABLE-ENTRY
  check-special-form-syntax)

(DEFINE *ALPHATIZER-TABLE* (MAKE-TABLE '*ALPHATIZER-TABLE*))

(DEFINE (SET-ALPHATIZER SYMBOL PROC SPECT)
  (LET ((DESC (OBTAIN-SYNTAX-TABLE-ENTRY *STANDARD-SYNTAX-TABLE*
                                         SYMBOL
					 SPECT)))
    (SET (TABLE-ENTRY *ALPHATIZER-TABLE* DESC) PROC)
    DESC))

;;; SET-MACRO-DEFINITION is only called by DEFINE-COMPILER-MACRO forms.

(DEFINE (SET-MACRO-DEFINITION SYMBOL PROC SPECT)
  (SET-ALPHATIZER SYMBOL
		  (LAMBDA (SEXPR)
		    (BIND ((*WHERE* (CONS (CAR SEXPR) *WHERE*)))
		      (ALPHATIZE (PROC SEXPR))))
		  DESC))

;;; DEFINE-ALPHATIZER is used exclusively by this file.

(DEFINE-LOCAL-SYNTAX (DEFINE-ALPHATIZER SYMBOL ARGS SPECT . BODY)
  `(SET-ALPHATIZER ',SYMBOL (LAMBDA ,ARGS . ,BODY) ',SPECT))

;;; Primitive special forms

;;; (QUOTE constant)

(DEFINE-ALPHATIZER QUOTE (SEXPR) (1 . 1)
  (ALPHA-CONSTANT (CADR SEXPR)))

(DEFINE (ALPHA-CONSTANT SEXPR)
  (DECLARE (SPECIAL *NULL-LIST-CONSTANT*))
  (NODIFY (COND ((NULL? SEXPR) *NULL-LIST-CONSTANT*)
                ((SYMBOL? SEXPR)
                 (OR (GET SEXPR 'CONSTANT-STRUCT)
                     (PUT SEXPR 'CONSTANT-STRUCT
                          (CONS-A-CONSTANT VALUE SEXPR))))
                (ELSE (CONS-A-CONSTANT VALUE SEXPR)))))

(DEFINE *NULL-LIST-CONSTANT* (CONS-A-CONSTANT VALUE '()))

(DEFINE-ALPHATIZER IGNORE (SEXPR) (0 . T)
  (ALPHA-IGNORE (CDR SEXPR) 'IGNORED))

(DEFINE-ALPHATIZER IGNORABLE (SEXPR) (0 . T)
  (ALPHA-IGNORE (CDR SEXPR) 'IGNORABLE))

(DEFINE (ALPHA-IGNORE VARS HOW)
  (WALK (LAMBDA (VAR)
          (IF (SYMBOL? VAR)
              (LET ((Z (ALPHA-FREE-VAR-REF VAR NIL)))
                (IF (NOT (STATIC? Z))
                    (SET (VARIABLE-EVER-READ-REFD Z) HOW)))))
        VARS)
  (ALPHA-CONSTANT NIL))

;;; (VARIABLE-VALUE variable-name)

(DEFINE-ALPHATIZER VARIABLE-VALUE (SEXPR) (1 . 1)
  (ALPHA-VARIABLE (CADR SEXPR)))

;(DEFINE-ALPHATIZER %VAR (SEXPR)              ; hack for BIND
;  (NODIFY (ALPHA-FREE-VAR-REF (CADR SEXPR) T)))

(DEFINE (ALPHA-VARIABLE IDENTIFIER)
  (LET ((VAR (ALPHA-FREE-VAR-REF IDENTIFIER NIL)))
    (COND ((STATIC? VAR)
           (LET ((K (CONSTANT-DEFINITION VAR)))
             (COND ((EMPTY K) (NODIFY VAR))
                   (ELSE (ALPHA-CONSTANT K)))))
          (ELSE (NODIFY VAR)))))

(DEFINE (CONSTANT-DEFINITION VAR) (CGET-EMPTY VAR 'CONSTANT))

;;; Returns a VARIABLE or STATIC structure

(DEFINE (ALPHA-FREE-VAR-REF IDENTIFIER RESERVED-WORD-OK?)
  (ALPHA-VAR-REF IDENTIFIER RESERVED-WORD-OK? NIL))

(DEFINE (ALPHA-LOCAL-VAR-REF IDENTIFIER RESERVED-WORD-OK?)
  (ALPHA-VAR-REF IDENTIFIER RESERVED-WORD-OK? T))

(DEFINE (ALPHA-VAR-REF IDENTIFIER RESERVED-WORD-OK? LOCAL?)
  (LET ((PROBE (ASSQ IDENTIFIER *ENV*)))
    (COND (PROBE (CDR PROBE))           ; Lexical variable
          ((AND (NOT RESERVED-WORD-OK?)
                (GET-SYNTAX IDENTIFIER))
           (MAGIC-CONTEXT-ERROR IDENTIFIER))
          (LOCAL? (LOCAL-LOOKUP *NAMESPACE* IDENTIFIER T))
          (ELSE   (FREE-LOOKUP  *NAMESPACE* IDENTIFIER T)))))

(DEFINE (MAGIC-CONTEXT-ERROR IDENTIFIER)
  (MENTION "~S reserved word referenced as variable"
	   "will treat it as a variable reference"
	   IDENTIFIER)
  (ALPHA-FREE-VAR-REF IDENTIFIER T))

(DEFINE-ALPHATIZER PRIMOP (SEXPR) (1 . 1)
  (NODIFY (LET ((SYMBOL (CADR SEXPR)))
            (COND ((FREE-LOOKUP *PRIMOP-NAMESPACE* SYMBOL NIL))
                  (ELSE
                   (WARN "~S isn't a PRIMOP"
                         "will use PRIMOP-LOSSAGE instead of the PRIMOP"
                         SYMBOL)
                   (ALPHA-FREE-VAR-REF 'PRIMOP-LOSSAGE T))))))

;;; (SET-VAR variable-name value), and friends

(DEFINE-ALPHATIZER SET-VARIABLE-VALUE (SEXPR) (2 . 2)    ; for SET expansion
  (ALPHA-SET-VAR-FORM SEXPR NIL NIL))

(DEFINE-ALPHATIZER LSET    (SEXPR) (2 . 2)
  (ALPHA-SET-VAR-FORM SEXPR T NIL))

(DEFINE-ALPHATIZER DEFINE-VARIABLE-VALUE (SEXPR) (2 . 2)       ; for DEFINE
  (ALPHA-SET-VAR-FORM SEXPR T T))

(DEFINE (ALPHA-SET-VAR-FORM SEXPR LOCAL? DEFINE?)
  (ALPHA-SET-VAR (COND (LOCAL? (ALPHA-LOCAL-VAR-REF (CADR SEXPR) NIL))
                       (ELSE   (ALPHA-FREE-VAR-REF  (CADR SEXPR) NIL)))
                 (ALPHATIZE (CADDR SEXPR))
                 LOCAL?
                 DEFINE?))

(DEFINE (ALPHA-SET-VAR VAR BODY LOCAL? DEFINE?)
  (COND ((NOT (STATIC? VAR)))
        ((NOT (EMPTY (CONSTANT-DEFINITION VAR)))
         (WARN "the constant ~S is being assigned"
               "will compile an assignment, but you may be losing"
               (STATIC-IDENTIFIER VAR)))
        ((NOT (EMPTY (INTEGRABLE-PROCEDURE-DEFINITION VAR)))
         (WARN "integrable procedure ~S is being assigned"
               "will compile an assignment, but you may be losing"
               (STATIC-IDENTIFIER VAR)))
        ((CGET VAR 'DEFINED)
         (WARN "~S is already defined"
               "will compile a normal assignment"
               (STATIC-IDENTIFIER VAR))))
  (LET ((NODE (NODIFY (CONS-A-SETQ VAR VAR
                                   BODY BODY
                                   DEFINITIONP DEFINE?  ; yug
                                   LOCALP LOCAL?))))
    (SET (NODE-PARENT BODY) NODE)
    NODE))

;;; (BLOCK . forms)

(DEFINE-ALPHATIZER BLOCK (SEXPR) (0 . T)
  (ALPHA-BLOCKIFY (CDR SEXPR)))

(DEFINE (ALPHA-BLOCKIFY FORMLIST)
  (COND ((NULL? (CDR FORMLIST)) (ALPHATIZE (CAR FORMLIST)))
        (ELSE (ALPHA-BLOCK-1 (MAP ALPHATIZE FORMLIST)))))

(DEFINE (ALPHA-BLOCK-1 ARGS)
  (LET ((NODE (NODIFY (CONS-A-BLOCK ARGS ARGS))))
    (WALK (LAMBDA (A) (SET (NODE-PARENT A) NODE)) ARGS)
    NODE))  

;;; ... ALPHA-CASE

(DEFINE-ALPHATIZER CASE (SEXPR) (1 . T)
  (BIND ((*WHERE* (CONS 'CASE *WHERE*)))
    (DO ((CLAUSES (CDDR SEXPR) (CDR CLAUSES))
         (MODE '())
         (NEWCLAUSES '())
         (ALLKEYS '())
         (ELSE-CLAUSE '()))
        ((OR ELSE-CLAUSE (NULL? CLAUSES))
         (LET ((FINALKEY (ALPHATIZE (CADR SEXPR)))
               (FINALELSE (OR ELSE-CLAUSE (ALPHA-CONSTANT '())))
               (FINALCLAUSES (REVERSE! NEWCLAUSES))     ;REVERSE! mostly for prettiness
               (TYPE MODE))
           (LET ((NODE (NODIFY (CONS-A-CASE KEY FINALKEY
                                            CLAUSES FINALCLAUSES
                                            ELSE FINALELSE
                                            TYPE TYPE))))
             (SET (NODE-PARENT FINALKEY) NODE)
             (SET (NODE-PARENT FINALELSE) NODE)
             (WALK (LAMBDA (C) (SET (NODE-PARENT (CDR C)) NODE))
                   FINALCLAUSES)
             NODE)))
      ----
      (COND ((CASE-ELSE-P (CAAR CLAUSES))
             (COND ((NULL? (CDR CLAUSES))
                    (SET ELSE-CLAUSE (ALPHA-BLOCKIFY (CDAR CLAUSES))))
                   (ELSE (WARN "~S else clause not at end of CASE"
                               "will ignore the illegal clause"
                               (CAR CLAUSES)))))
            (ELSE
             (LET ((KEYS (COND ((NULL? (CAAR CLAUSES)) '())
                               ((ATOM? (CAAR CLAUSES)) (LIST (CAAR CLAUSES)))
                               (ELSE (CAAR CLAUSES)))))
               (DO ((K KEYS (CDR K))
                    (NEWKEYS '()))
                   ((NULL? K)
                    (COND (NEWKEYS    ;no keys => flush clause
                           (SET ALLKEYS (APPEND NEWKEYS ALLKEYS))
                           ;; REVERSE! is purely cosmetic
                           (PUSH NEWCLAUSES
                                 (CONS (REVERSE! NEWKEYS)
                                       (ALPHA-BLOCKIFY (CDAR CLAUSES)))))))
                 ----
                 (COND ((NULL? MODE)
                        (SET MODE (CASE-GET-MODE (CAR K)))
                        (PUSH NEWKEYS (CAR K)))
                       (ELSE
                        (COND ((NEQ? (CASE-GET-MODE (CAR K)) MODE)
                               (SET MODE 'ANY)))
                        (COND ((MEMBER (CAR K) NEWKEYS)
                               (MENTION "~S duplicated key within a single CASE clause"
                                        "will ignore the duplicate key (it doesn't matter)"
                                        (CAR K)))
                              ((MEMBER (CAR K) ALLKEYS)
                               (MENTION "~S duplicated key in more than one CASE clause"
                                        "will ignore all but first occurrence"
                                        (CAR K)))
                              (ELSE (PUSH NEWKEYS (CAR K))))))
                 ---))))
      ---)))

(DEFINE (CASE-ELSE-P KEY) (OR (EQ? KEY 'T) (EQ? KEY 'ELSE)))

(DEFINE (CASE-GET-MODE OBJ)
  (COND ((CHAR? OBJ) 'CHARACTER)
        ((TARGET:FIXNUM? OBJ) 'FIXNUM)
        ((SYMBOL? OBJ) 'SYMBOL)
        (ELSE 'T)))

;;; ... CATCH

(DEFINE-ALPHATIZER CATCH (SEXPR) (1 . T)
  (LET ((CNODE
         (NODIFY (BIND ((*ENV* *ENV*))
                   (LET* ((VARSPEC (CADR SEXPR))
                          (VARSTRUCT (CONS-A-VARIABLE IDENTIFIER VARSPEC)))
                     ;; Push an entry onto *ENV*.
                     (ENVPUSH VARSPEC VARSTRUCT)
                     (CONS-A-CATCH VAR VARSTRUCT
                                   BODY (ALPHA-BLOCKIFY (CDDR SEXPR))))))))
    (SET (VARIABLE-BINDER (CATCH-VAR (NODE-FORM CNODE))) CNODE)
    (SET (NODE-PARENT (CATCH-BODY (NODE-FORM CNODE))) CNODE)
    CNODE))

;;; ... ALPHA-LAMBDA

(DEFINE-ALPHATIZER LAMBDA (SEXPR) (1 . T)
  (ALPHA-LAMBDA (CDR SEXPR)))

(DEFINE-ALPHATIZER NAMED-LAMBDA (SEXPR) (2 . T)
  (COND ((SYMBOL? (CADR SEXPR))
         (BIND ((*WHERE* (CONS (CADR SEXPR) *WHERE*)))
           (LET ((NODE (ALPHA-LAMBDA (CDDR SEXPR)))
                 (VAR (ALPHA-FREE-VAR-REF (CADR SEXPR) T)))
             (SET (LAMBDA-NAME (NODE-FORM NODE)) VAR)
             NODE)))
        (ELSE
         (ALPHA-LAMBDA (CDDR SEXPR)))))

(DEFINE (ALPHA-LAMBDA ARGS+BODY)
  (BIND ((*ENV* *ENV*)
         (*VARS-SO-FAR '()))
    (DO ((V (CAR ARGS+BODY) (CDR V))
         (VARS '() (CONS (ALPHA-LAMBDA-BINDING (CAR V)) VARS)))
        ((ATOM? V)
         ;; RESTVAR should exist before BODY is alphatized.
         (LET* ((RESTVAR (IF V (ALPHA-LAMBDA-BINDING V) *EMPTY*))
                (BODY (ALPHA-BLOCKIFY (CDR ARGS+BODY)))
                (NEWVARS (REVERSE! VARS))
                (FM (CONS-A-LAMBDA VARS NEWVARS
                                   RESTVAR RESTVAR
                                   BODY BODY))
                (NODE (NODIFY FM)))
           (SET (NODE-PARENT BODY) NODE)
           (WALK (LAMBDA (V)
                   (IF V (SET (VARIABLE-BINDER V) NODE)))
                 NEWVARS)
           (IF (AND (NOT (EMPTY RESTVAR)) RESTVAR)
               (SET (VARIABLE-BINDER RESTVAR) NODE))
           NODE)))))

;;; Process a lambda variable.  Returns a VARIABLE structure (or () for
;;; an ignored position).
;;; As a side effect, this function will push an entry onto *ENV*.
;;; Also, the name of the variable is pushed onto *VARS-SO-FAR;
;;; this is used for error-checking.

(DEFINE (ALPHA-LAMBDA-BINDING VARSPEC)
  (DECLARE (SPECIAL *VARS-SO-FAR))
  (COND ((NULL? VARSPEC) '())
        (ELSE
         ;; Check for duplicated variables in the binding list.
         (IF (MEMBER VARSPEC *VARS-SO-FAR)
             (WARN "~S multiply bound variable in LAMBDA list"
                   "references will refer to the last such binding"
                   VARSPEC))
         ;; Make an entry on *VARS-SO-FAR for future error-checking.
         (PUSH *VARS-SO-FAR VARSPEC)
         ;; Figure out what VARIABLE structure to return.
         (LET ((VARSTRUCT (CONS-A-VARIABLE IDENTIFIER VARSPEC)))
           ;; Push an entry onto *ENV*.
           (ENVPUSH VARSPEC VARSTRUCT)
           VARSTRUCT))))

;;; ALPHA-LABELS
;;; (LABELS ((F (LAMBDA (A B) ...)) ...) ...) or
;;; (LABELS (((F A B) ...) ...) ...)
;;; Should be checking for duplicates here, using ALPHA-LAMBDA-BINDING.

(DEFINE-ALPHATIZER LABELS (SEXPR) (1 . T)
  (BIND ((*WHERE* (CONS 'LABELS *WHERE*))
         (*ENV* *ENV*))
    (LET* ((LNODE (NODIFY (CONS-A-LABELS)))
           (FM (NODE-FORM LNODE)))
      (SET (LABELS-VARS FM)
           (MAP (LAMBDA (LABELS-FORM)
                  (LET ((V (IF (ATOM? (CAR LABELS-FORM))
                               (CAR LABELS-FORM)
                             (CAAR LABELS-FORM))))
                    (LET ((VS (CONS-A-VARIABLE IDENTIFIER V
                                               BINDER LNODE)))
                      (ENVPUSH V VS)
                      VS)))
                (CADR SEXPR)))
      (SET (LABELS-VALS FM)
           (MAP (LAMBDA (X) (ALPHA-LABELS-FORM X)) (CADR SEXPR)))
      (WALK (LAMBDA (VAR VAL)
              (SET (NODE-PARENT VAL) LNODE)
              (IGNORE VAR)
              (COND ((LAMBDA-NODE? VAL)
                     (SET (LAMBDA-NAME (NODE-FORM VAL)) VAR))))
            (LABELS-VARS FM)
            (LABELS-VALS FM))
      (SET (LABELS-BODY FM) (ALPHA-BLOCKIFY (CDDR SEXPR)))
      (SET (NODE-PARENT (LABELS-BODY FM)) LNODE))))

(DEFINE (ALPHA-LABELS-FORM LFORM)
  (ALPHATIZE
   (COND ((ATOM? (CAR LFORM))
          (IF (NOT (FX= (LENGTH LFORM) 2))
              (WARN "LABELS binding spec with atomic CAR does not have 2 forms"
                    "will ignore extras, or supply () to fill omission"))
          (CADR LFORM))
         (ELSE
          `(LAMBDA ,(CDAR LFORM)
             ,@(CDR LFORM))))))

;;; (IF predicate consequent . alternates)

(DEFINE-ALPHATIZER IF (SEXPR) (2 . 3)
  (DESTRUCTURE (((PRED CON . MAYBE-ALT) (CDR SEXPR)))
    (ALPHA-IF PRED
	      CON
	      (COND ((NULL? MAYBE-ALT) 0)
		    (ELSE (CAR MAYBE-ALT))))))

(DEFINE (ALPHA-IF PREDSEXPR CONSEXPR ALTSEXPR)
  (LET ((PRED (ALPHATIZE PREDSEXPR))
        (CON (ALPHATIZE CONSEXPR))
        (ALT (ALPHATIZE ALTSEXPR)))
    (LET ((NODE (NODIFY (CONS-A-IF PRED PRED CON CON ALT ALT))))
      (SET (NODE-PARENT PRED) NODE)
      (SET (NODE-PARENT CON) NODE)
      (SET (NODE-PARENT ALT) NODE)
      NODE)))
      
(DEFINE (CHECK-NUMBER-OF-ARGS VAR ARGS)
  (LET* ((FNNAME (STATIC-IDENTIFIER VAR))
         (PARGSPROP (CGET VAR 'PRIMOP-NUMBER-OF-ARGS))
         (ARGSPROP (OR PARGSPROP (CGET VAR 'DEFINED)))
         (NARGS (LENGTH ARGS)))
    (COND ((ATOM? ARGSPROP) ARGS)
          ((FX< NARGS (OR (CAR ARGSPROP) (CDR ARGSPROP)))
           (WARN "~D is too few arguments for the function ~S (which requires at least ~D)"
                 (COND (PARGSPROP "will assume missing arguments to be ()")
                       (ELSE "none"))
                 NARGS FNNAME (OR (CAR ARGSPROP) (CDR ARGSPROP)))
           (COND (PARGSPROP
                  (DO ((I (OR (CAR ARGSPROP) (CDR ARGSPROP)) (FX- I 1))
                       (A ARGS (CDR A))
                       (Z '() (CONS (CAR A) Z)))      ;(CAR '()) => ()
                      ((FX= I 0) (REVERSE! Z))))
                 (ELSE ARGS)))
          ((AND (CDR ARGSPROP) (FX> NARGS (CDR ARGSPROP)))
           (WARN "~D is too many arguments for the function ~S (which permits at most ~D)"
                 (COND (PARGSPROP "will ignore extra arguments")
                       (ELSE "none"))
                 NARGS FNNAME (CDR ARGSPROP))
           (COND (PARGSPROP
                  (DO ((I (CDR ARGSPROP) (FX- I 1))
                       (A ARGS (CDR A))
                       (Z '() (CONS (CAR A) Z)))
                      ((FX= I 0) (REVERSE! Z))))
                 (ELSE ARGS)))
          (ELSE ARGS))))

;;; (CALL function . arguments)

;(DEFINE-ALPHATIZER CALL (SEXPR)
;  (ALPHA-COMBINATION (ALPHATIZE (CADR SEXPR)) (CDDR SEXPR)))

(DEFINE (ALPHA-COMBINATION PROCNODE ARGFORMS)
  ;; Number-of-args check should be done by optimizer, due to changes incurred
  ;; by substitutions and other optimizations.
  (COND ((STATIC-NODE? PROCNODE)
         (LET* ((VAR (NODE-FORM PROCNODE))
                (NEWARGS (CHECK-NUMBER-OF-ARGS VAR ARGFORMS))
                (PROBE (INTEGRABLE-PROCEDURE-DEFINITION VAR)))
           (COND ((EMPTY PROBE)
                  (ALPHA-CALL PROCNODE NEWARGS))
                 (ELSE
                  (ALPHA-COMBINATION (ALPHA-TOPLEVEL PROBE)
                                     NEWARGS)))))
        (ELSE (ALPHA-CALL PROCNODE ARGFORMS))))
                             
(DEFINE (ALPHA-CALL PROC ARGFORMS)
  (LET ((ARGS (MAP ALPHATIZE ARGFORMS)))
    (LET ((NODE (NODIFY (CONS-A-CALL FUNCTION PROC ARGS ARGS))))
      (SET (NODE-PARENT PROC) NODE)
      (WALK (LAMBDA (A) (SET (NODE-PARENT A) NODE)) ARGS)
      NODE)))

;;; Utility for special forms like DEFINE and DEFINE-MACRO which
;;; establish things in the support output file.  This
;;; definition is obviously quite crude, and is subject to namespace
;;; screws.  Fix later.

(DEFINE (ESTABLISH-SUPPORT VAR PROP VAL)
  (CPUT VAR PROP VAL)
  (ESTABLISH-SUPPORT-1 VAR
                       PROP
                       (COND ((NOT VAL) 'NIL)   ;Hack for prettiness
                             ((EQ? VAL T) 'T)
                             (ELSE `',VAL))))

(DEFINE (ESTABLISH-SYNTAX-SUPPORT SYMBOL DESCR)
  (ESTABLISH-SUPPORT-1 SYMBOL 'MAGIC DESCR))            ; UGH.

(DEFINE (ESTABLISH-SUPPORT-1 VAR PROP VAL)
  (LET ((SYM (COND ((SYMBOL? VAR) VAR)
                   ((STATIC? VAR) (STATIC-IDENTIFIER VAR))
                   (ELSE (BUG)))))
    (FORMAT *SUPPORT-OUTPUT*
            "~&(CPUT '~S '~S ~S)~%"
            SYM PROP VAL)))

(DEFINE-ALPHATIZER DEFINE (SEXPR) (2 . T)
  (ALPHA-DEFINE SEXPR NIL))

(DEFINE-ALPHATIZER DEFINE-CONSTANT (SEXPR) (2 . T)
  (ALPHA-DEFINE SEXPR 'CONSTANT))

(DEFINE-ALPHATIZER DEFINE-INTEGRABLE (SEXPR) (2 . T)
  (ALPHA-DEFINE SEXPR 'INTEGRABLE-PROCEDURE))

(DEFINE (ALPHA-DEFINE SEXPR WIRED?)
  (LET ((PAT (CADR SEXPR))
        (BODY (CDDR SEXPR)))
    (COND ((SYMBOL? PAT)
           (IF (NOT (NULL? (CDR BODY)))
               (WARN "~S peculiar DEFINE syntax"
                     "will ignore the DEFINE's CDDDR"
                     SEXPR))
           (ALPHA-DEFINE-1 PAT (CAR BODY) WIRED?))
          ((OR (NOT (PAIR? PAT)) (NOT (SYMBOL? (CAR PAT))))
           (WARN "~S peculiar DEFINE syntax"
                 "will ignore the DEFINE entirely"
                 SEXPR)
           (ALPHA-CONSTANT '()))
          (ELSE
           (ALPHA-DEFINE-1 (CAR PAT)
                           ;; `(NAMED-LAMBDA ,(CAR PAT) ,(CDR PAT) . ,BODY)
                           `(LAMBDA ,(CDR PAT) . ,BODY)
                           WIRED?)))))

(DEFINE (ALPHA-DEFINE-1 IDENTIFIER VALUE WIRED?)
  (COND ((MEMQ IDENTIFIER *DEFINE-BREAK-IN*)
         (CTRACE 'ALL)
         (SET *PROBE?* T)
         (FORMAT *NOISE-PLUS-TERMINAL* ";Compiling ~S.~%" IDENTIFIER)))
  (LET ((VAR (ALPHA-LOCAL-VAR-REF IDENTIFIER NIL)))
    (BIND ((*WHERE* (CONS IDENTIFIER *WHERE*)))
      ;; First, create a normal definition without optimization, and return it.
      (LET* ((NODE (ALPHATIZE VALUE))
             (FM (NODE-FORM NODE))
             (DEFNODE (ALPHA-SET-VAR VAR NODE
                                     T  ; LOCAL? = T
                                     (IF WIRED? 'WIRED T)       ; DEFINE? = T
                                     )))
        ;; Next, handle integrability by optimizing the body and
        ;; transforming (LAMBDA (X) (F X)) to just F.
        (IF WIRED?
            (ALPHA-DEFINE-WIRED VAR VALUE WIRED?))
        ;; Lastly, set the procedure's DEFINED property.  This must be
        ;; done at the end in order to avoid complaints by SET-VAR.
        (COND ((EQ? (STYPE FM) 'LAMBDA)
               (SET (LAMBDA-NAME FM) VAR)
               (LET ((LEN (LENGTH (LAMBDA-VARS FM))))
                 (ESTABLISH-SUPPORT VAR
                                    'DEFINED
                                    (COND ((EMPTY (LAMBDA-RESTVAR FM))
                                           (CONS LEN LEN))
                                          (ELSE (CONS LEN 9999.))))))
              (ELSE
               (ESTABLISH-SUPPORT VAR 'DEFINED T)))
        DEFNODE))))

;;; Need to re-alphatize to permit inconsistent expansion of IF-INTEGRATED's.

(DEFINE (ALPHA-DEFINE-WIRED VAR VALUE WIRED?)
  (LET* ((NODE (BIND ((*INTEGRATED?* T))
                 (FORMAT *NOISE-OUTPUT*
                         "~&;Processing ~S definition of ~S"
                         WIRED?
                         (STATIC-IDENTIFIER VAR))
                 (PHASE!META-EVALUATE (PHASE!PROPAGATE (ALPHATIZE VALUE)))))
         (FM (NODE-FORM NODE)))
    (CASE WIRED?
      ((CONSTANT)
       (COND ((EQ? (STYPE FM) 'CONSTANT)
              (ESTABLISH-SUPPORT VAR
                                 'CONSTANT
                                 (CONSTANT-VALUE FM)))))
      ((INTEGRABLE-PROCEDURE)
       (LET ((NEWNODE (CONVERT-LAMBDA-TO-VARIABLE NODE)))
         (COND ((INTEGRABLE-PROCEDURE-CANDIDATE? NEWNODE VAR)
                (ESTABLISH-SUPPORT VAR
                                   'INTEGRABLE-FUNCTION
                                   (SEXPRFY NEWNODE)))
               ((ENTITY-NODE? NEWNODE)
                (LET ((D (CAR (CALL-ARGS (NODE-FORM NEWNODE)))))
                  (COND ((INTEGRABLE-PROCEDURE-CANDIDATE? D VAR)
                         (ESTABLISH-SUPPORT VAR
                                            'INTEGRABLE-FUNCTION
                                            (SEXPRFY D))))))))))))
                
(DEFINE (INTEGRABLE-PROCEDURE-CANDIDATE? NODE VAR)
  (OR (LAMBDA-NODE? NODE)
      (AND (STATIC-NODE? NODE)
           (NEQ? (NODE-FORM NODE) VAR))))

(DEFINE (INTEGRABLE-PROCEDURE-DEFINITION VAR)
  (CGET-EMPTY VAR 'INTEGRABLE-FUNCTION))

(DEFINE (INTEGRABLE-FUNCTION-DEFINITION VAR)
  (INTEGRABLE-PROCEDURE-DEFINITION VAR))

(DEFINE-ALPHATIZER DEFINE-LOCAL-SYNTAX (SEXPR) (2 . T)
  (ALPHA-DEFINE-SYNTAX SEXPR NIL))

(DEFINE-ALPHATIZER DEFINE-MACRO (SEXPR) (2 . T)     ; Flush some day.
  (ALPHA-DEFINE-SYNTAX SEXPR T))

(DEFINE (ALPHA-DEFINE-SYNTAX SEXPR LOAD-TIME?)
  (LET ((PAT (CADR SEXPR))
        (BODY (CDDR SEXPR)))
    (COND ((PAIR? PAT)
           (ALPHA-DEFINE-SYNTAX-1 (CAR PAT)
                                  `(MACRO-EXPANDER ,PAT . , BODY)
                                  LOAD-TIME?))
          (ELSE
           (COND ((NOT (NULL? (CDR BODY)))
                  (WARN "too many ~S body subforms"
                        "will ignore all but the first"
                        (CAR SEXPR))))
           (ALPHA-DEFINE-SYNTAX-1 PAT
                                  (CAR BODY)
                                  LOAD-TIME?)))))

(DEFINE (ALPHA-DEFINE-SYNTAX-1 SYMBOL DESCR LOAD-TIME?)
  (COND ((NOT (SYMBOL? SYMBOL))
         (WARN "illegal DEFINE-LOCAL-SYNTAX syntax"
               "will ignore the DEFINE-LOCAL-SYNTAX entirely")
         (ALPHA-CONSTANT NIL))
        (ELSE
         (COND ((FREE-LOOKUP *NAMESPACE* SYMBOL NIL)
                => (LAMBDA (STATIC)
                     (COND ((CGET STATIC 'VARIABLE-STRUCT)
                            (MENTION "variable ~S is being defined as reserved word"
                                     "none"
                                     SYMBOL)
                            ;; Suppress further error msgs
                            (CREM STATIC 'VARIABLE-STRUCT))))))
         ;; This is really unsatisfactory.
         (COND ((GET-SYNTAX SYMBOL)
                (MENTION "redefining special form ~S"
                         "none"
                         SYMBOL)))
         (*DEFINE-LOCAL-SYNTAX SYMBOL (EVAL DESCR *TC-MACRO-DEFINITION-ENV*))
         (COND (LOAD-TIME?
                (ESTABLISH-SYNTAX-SUPPORT SYMBOL DESCR)       ;ugh
                (ALPHATIZE `(*DEFINE-SYNTAX (THE-ENVIRONMENT)
					    ,SYMBOL
					    ,DESCR)))   ;see CMACROS
               (ELSE
                (ALPHA-CONSTANT NIL))))))

(DEFINE-ALPHATIZER LET-SYNTAX (SEXPR) (1 . T)
  (BIND ((*SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *SYNTAX-TABLE* NIL)))
    (WALK (LAMBDA (SPEC)
            (LET ((DOIT
                   (LAMBDA (SYMBOL DESCR)
                     (*DEFINE-LOCAL-SYNTAX SYMBOL
                          (EVAL DESCR *TC-MACRO-DEFINITION-ENV*)))))
              (COND ((PAIR? (CAR SPEC))
                     (DOIT (CAAR SPEC) `(MACRO-EXPANDER . ,SPEC)))
                    (ELSE
                     (DOIT (CAR SPEC) (CADR SPEC))))))
          (CADR SEXPR))
    (ALPHA-BLOCKIFY (CDDR SEXPR))))

;;; Temporary hack?  The lookup logic in ALPHATIZE is truly chaotic.

(DEFINE-ALPHATIZER VAR-LOCATIVE (SEXPR) (1 . 1)
  (LET ((NAME (CADR SEXPR)))
    (COND ((ASSQ NAME *ENV*)
           (LET ((VAL (GENERATE-SYMBOL 'VAL)))
             (ALPHATIZE `(OBJECT NIL
                                 ((CONTENTS SELF) ,NAME)
                                 (((SETTER CONTENTS) () ,VAL)
                                  (SET ,NAME ,VAL))
                                 ((LOCATIVE? SELF) T)))))
          (ELSE (ALPHA-STATIC-LOCATIVE (FREE-LOOKUP *NAMESPACE* NAME T))))))

;(DEFINE-ALPHATIZER LOCAL-VAR-LOCATIVE (SEXPR) (1 . 1)
; (COND ((ASSQ (CADR SEXPR) *ENV*)
;        (WARN "can't get a local locative to local variable ~S"
;              "will compile () in place of the locative"
;              (CADR SEXPR))
;        (ALPHA-CONSTANT '()))
;       (ELSE
;        (ALPHA-STATIC-LOCATIVE (LOCAL-LOOKUP *NAMESPACE* (CADR SEXPR) T)))))

(DEFINE (ALPHA-STATIC-LOCATIVE STATIC)
  (ALPHA-CONSTANT (QLOZURE (STATIC)
                    (LAMBDA ()
                      (GEN-STATIC-LOCATIVE STATIC))
                    ((FUNNY-CONSTANT? SELF) T)
                    ((PRINT SELF STREAM)
                     (FORMAT STREAM "#{Locative~_~S}"
                             (STATIC-IDENTIFIER STATIC)))
                    )))

(define-alphatizer the-environment (sexpr) (0 . 0)
  (ignore sexpr)
  (bind ((*namespace* *primop-namespace*))
    (alphatize (cond ((null? *env*)
		      '(%the-unit-environment))
		     (else
		      `(*the-environment (%the-unit-environment)
					 ',(map car *env*)
					 ,@(map (lambda (z)
						  `(var-locative ,(car z)))
						*env*)))))))

(DEFINE-ALPHATIZER DECLARE-SETTER (SEXPR) (2 . 2)
  (ESTABLISH-SUPPORT (CADR SEXPR) 'SETTER (CADDR SEXPR))
  (ALPHA-CONSTANT 'DECLARE-SETTER))

;;; An inverse to ALPHA-CONVERT for print purposes: SEXPRFY,
;;; SEXPRFY-LIST, SEXPRFY1

(DEFINE (SEXPRFY NODE) (SEXPRFY1 NODE 1000000.))

(DEFINE (SEXPRFY-LIST NODELIST) (SEXPRFY1-LIST NODELIST 1000000.))

;;; DEPTH is like PRINLEVEL: it gives the maximum depth desired in the
;;; S-expression structure to be returned.

(DEFINE (SEXPRFY1 NODE DEPTH)
  (COND ((AND (ZERO? DEPTH)
              (EQ? (STYPE NODE) 'NODE)
              (NOT (CONSTANT-NODE? NODE))
              (NOT (VARIABLE-NODE? NODE))
              (NOT (STATIC-NODE? NODE)))
         *PRINTS-AS-ELLIPSIS*)
        ((EQ? (STYPE NODE) 'NODE)
         (NODE-DISPATCH SEXPRFY1 NODE (FX- DEPTH 1)))
        (ELSE
         (SEXPRFY1 (ERROR "not a NODE - SEXPRFY1" NODE 'WRNG-TYPE-ARG) DEPTH))))

(DEFDISPATCH SEXPRFY1 CONSTANT (NODE FM DEPTH)
  (IGNORE NODE DEPTH)
  (LET ((VAL (CONSTANT-VALUE FM)))
    (COND ((NUMBER? VAL) (CONSTANT-VALUE FM))
          (ELSE `',VAL))))

(DEFDISPATCH SEXPRFY1 STATIC (NODE FM DEPTH)
  (IGNORE NODE DEPTH)
  (STATIC-IDENTIFIER FM))

(DEFDISPATCH SEXPRFY1 VARIABLE (NODE FM DEPTH)
  (IGNORE NODE DEPTH)
  (VARIABLE-IDENTIFIER FM))

(DEFDISPATCH SEXPRFY1 SETQ (NODE FM DEPTH)
  (IGNORE NODE)
  `(,(COND ((SETQ-LOCALP FM)
            (IF (SETQ-DEFINITIONP FM) 'DEFINE 'LSET))
           (ELSE
            'SET))
    ,(IF (STATIC? (SETQ-VAR FM))
         (STATIC-IDENTIFIER (SETQ-VAR FM))
       (VARIABLE-IDENTIFIER (SETQ-VAR FM)))
    ,(SEXPRFY1 (SETQ-BODY FM) DEPTH)))

(DEFDISPATCH SEXPRFY1 LAMBDA (NODE FM DEPTH)
  (IGNORE NODE)
  `(LAMBDA
    ,(APPEND (MAP (LAMBDA (V) (IF V (VARIABLE-IDENTIFIER V) '()))
                  (LAMBDA-VARS FM))
             (COND ((EMPTY (LAMBDA-RESTVAR FM)) '())
                   ((NULL? (LAMBDA-RESTVAR FM)) 'IGNORED)
                   (ELSE (VARIABLE-IDENTIFIER (LAMBDA-RESTVAR FM)))))
    ,(SEXPRFY1 (LAMBDA-BODY FM) DEPTH)))

(DEFDISPATCH SEXPRFY1 LABELS (NODE FM DEPTH)
  (IGNORE NODE)
  `(LABELS
    ,(MAP (LAMBDA (VAR VAL)
            (LIST (VARIABLE-IDENTIFIER VAR) (SEXPRFY1 VAL DEPTH)))
          (LABELS-VARS FM)
          (LABELS-VALS FM))
    ,(SEXPRFY1 (LABELS-BODY FM) DEPTH)))

(DEFDISPATCH SEXPRFY1 CATCH (NODE FM DEPTH)
  (IGNORE NODE)
  `(CATCH ,(LET ((V (CATCH-VAR FM)))
	     (IF V (VARIABLE-IDENTIFIER V) NIL))
          ,(SEXPRFY1 (CATCH-BODY FM) DEPTH)))

(DEFDISPATCH SEXPRFY1 BLOCK (NODE FM DEPTH)
  (IGNORE NODE)
  `(BLOCK . ,(SEXPRFY1-LIST (BLOCK-ARGS FM) DEPTH)))

(DEFDISPATCH SEXPRFY1 IF (NODE FM DEPTH)
  (IGNORE NODE)
  `(IF ,(SEXPRFY1 (IF-PRED FM) DEPTH)
       ,(SEXPRFY1 (IF-CON FM) DEPTH)
     ,(SEXPRFY1 (IF-ALT FM) DEPTH)))

(DEFDISPATCH SEXPRFY1 CASE (NODE FM DEPTH)
  (IGNORE NODE)
  `(CASE
    ,(SEXPRFY1 (CASE-KEY FM) DEPTH)
    ,@(MAP (LAMBDA (C)
             (LIST (CAR C)
                   (SEXPRFY1 (CDR C) DEPTH)))
           (CASE-CLAUSES FM))
    (ELSE ,(SEXPRFY1 (CASE-ELSE FM) DEPTH))))

(DEFDISPATCH SEXPRFY1 CALL (NODE FM DEPTH)
  (IGNORE NODE)
  (LET ((PROC (NODE-FORM (CALL-FUNCTION FM))))
    (COND ((AND (EQ? (STYPE PROC) 'LAMBDA) (EMPTY (LAMBDA-RESTVAR PROC)))
           `(LET ,(MAP (LAMBDA (VAR ARG)
                         (LIST (IF VAR (VARIABLE-IDENTIFIER VAR) '())
                               (SEXPRFY1 ARG DEPTH)))
                       (LAMBDA-VARS PROC)
                       (CALL-ARGS FM))
              ,(SEXPRFY1 (LAMBDA-BODY PROC) DEPTH)))
          (ELSE (CONS (SEXPRFY1 (CALL-FUNCTION FM) DEPTH)
                      (SEXPRFY1-LIST (CALL-ARGS FM) DEPTH))))))

(DEFINE (SEXPRFY1-LIST NODELIST DEPTH)
  (MAP (LAMBDA (X) (SEXPRFY1 X DEPTH)) NODELIST))
