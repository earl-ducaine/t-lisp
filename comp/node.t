(HERALD NODE
        (ENV T (TCOMP MACHAX)))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;; This file has been split off from DEFS for the sole purpose of
;;; having files small enough to compile using the T cross-compiler.


;;;; Node types

;;; Substructures of nodes and their uses.

(DEFINE-LOCAL-SYNTAX (DEFNODETYPE TYPE . COMPONENTS)
  (LET ((PREDNAME (SYMBOLCONC TYPE '-NODE?)))
    `(PROGN 'COMPILE
            (OR (MEMQ? ',TYPE *NODE-TYPES*)
                (SET *NODE-TYPES* (APPEND *NODE-TYPES* '(,TYPE))))
            (DEFVST ,TYPE . ,COMPONENTS)
            (DEFINE-INTEGRABLE (,PREDNAME X)
              (,(SYMBOLCONC TYPE '?) (NODE-FORM X)))
            )))

(LSET *NODE-TYPES* '())         ; how to pass this through to support file?

;;(CEVAL (PRINT '(SETQ *NODE-TYPES* '()) *SUPPORT-OUTPUT*))  ;Puke.

;;; Sacred type order:
;;;     Leaf node types:
;;;         CONSTANT
;;;         STATIC
;;;         VARIABLE
;;;     Environment:
;;;         SETQ                SETQ should go near VARIABLE.
;;;         LAMBDA              The three which hack bindings go together.
;;;         LABELS
;;;         CATCH
;;;     Control:
;;;         BLOCK
;;;         IF                  IF and CASE are similar.
;;;         CASE
;;;         CALL                Comes last, because it's usually the hairiest.

(DEFNODETYPE CONSTANT
  VALUE           ;S-expression value of constant
  )

(DEFNODETYPE VARIABLE     
  IDENTIFIER      ;The user's name for the variable
  (READ-REFS '()) ;List of VARIABLE nodes which refer to this variable for
                  ;  reading
  (WRITE-REFS '())     ;List of SETQ nodes which refer to this variable
  (KNOWN-FUNCTION NIL) ;If non-NIL, the node for an open LAMBDA expression
                  ;  to which this variable is known to be bound
  (CLOSURE-REFS '()) ;List of closures which close over this variable
  (TN NIL)        ;The TN for this variable
  BINDER          ;The LAMBDA-, LABELS-, or CATCH-node that binds this
                  ;  variable
  (EVER-READ-REFD NIL) ;Non-null if ever referenced (for error checking only)
  (EVER-WRITE-REFD NIL) ;Non-null if ever setq'd (for error checking only)
  REP             ;The representation chosen for the variable
  )
(DEFPROP VARIABLE
         (READ-REFS WRITE-REFS KNOWN-FUNCTION BINDER)
         SUPPRESSED-COMPONENT-NAMES)

;;; Static ("global") variable.
(DEFNODETYPE STATIC
  PROPERTIES      ;An a-list.  This is where CGET etc. look.
  IDENTIFIER      ;Symbol under which this variable is entered in its namespace
  NAMESPACE       ;Namespace which owns this variable
  )

(DEFNODETYPE SETQ
  VAR             ;A VARIABLE or STATIC structure
  BODY            ;Node for the value to set to
  DEFINITIONP     ;True iff this is a DEFINE
  LOCALP          ;True iff this is an LSET instead of a SET
  )

;;; Need to worry about actors (i.e. procedures with handlers) at some point?
;;; What about the corresponding thing for CATCH?

(DEFNODETYPE LAMBDA
  VARS            ;A list of VARIABLE structures (NIL means ignored arg
                  ;  position)
  (RESTVAR *EMPTY*)  ;VARIABLE structure for rest-arg, or NIL if ignored
                  ; rest-arg, or *EMPTY* if no rest-arg.
                  ; [BUG: how do we say that the rest position is ignored?]
  BODY            ;The node for the body
  EFFECTS         ;Side effects caused by the body plus the init forms
  AFFECTED        ;Side effects by which the body plus the init 
                  ;  forms are affected
  STRATEGY        ;The compilation technique to be used.  One of:
                  ;  NONE    used only in the LAMBDA of a CALL-LAMBDA
                  ;  JUMP    everyone who calls it can call it tail-recursively
                  ;  JSP     everyone can call it as a little subroutine
                  ;  EZCLOSE no code pointer needed, only environment
                  ;  PROC    it must be compiled as a full procedure
                  ;  TPROC   full procedure with no closed env (T is mnemonic
                  ;           for "toplevel" or "trivial"...?)
  ARGTNS          ;List of TNS for places where args are delivered on calls
  (HAS-CALLS NIL) ;Non-NIL iff body has any non-tail-recursive calls to general
                  ;  procedures (this is a hack for the code generator)
  REGION          ;Figure this out later... we may need several...
  (GENTAG *EMPTY*)  ;Assembler tag for function entry point
  (NAME NIL)      ;Structure for a VARIABLE or STATIC which holds this LAMBDA
                  ;  (debugging use only!)
  (FIRST-CALL NIL)  ;CALL node for "first" call to this procedure
                    ; (JUMP strategy only!)
  )

(DEFNODETYPE LABELS
  VARS            ;the list of VARIABLEs bound by this LABELS
  VALS            ;the values those variables are bound to
  BODY            ;the node for the body of the LABELS
  REFS            ;variables referred to by jump-strategy labels
  SETQS           ;variables assigned, similarly
  )

(DEFNODETYPE CATCH
  VAR             ;VARIABLE structure for tag bound by this catch
  BODY            ;Node for the body
  STRATEGY        ;Compilation strategy to be used:
                  ;  EXIT  if no procedure needs to be consed for the esc fun
                  ;  PROC  if a full function needs to be consed
                  ;  TAIL  like PROC but occurs in a tail-recursive position
  )

(DEFNODETYPE BLOCK
  ARGS            ;A list of nodes for the arguments
  )

(DEFNODETYPE IF
  PRED            ;the node for the predicate
  CON             ;the node for the consequent
  ALT             ;the node for the alternative
  )

(DEFNODETYPE CASE
  KEY             ;Node for the selector value
  CLAUSES         ;List of items "((-values-) . result)"
                  ;  Values have been enlisted and checked for duplications
                  ;  Result has been blockified if necessary
  ELSE            ;Node for fall-through form
  TYPE            ;One of FIXNUM, SYMBOL, or CHARACTER (generalize this?)
  )

(DEFNODETYPE CALL
  FUNCTION        ;The node for the function
  ARGS            ;A list of nodes
  STRATEGY        ;One of the following:
                  ;  LET     the function is a LAMBDA
                  ;  JUMP    function is a VARIABLE known to be bound to
                  ;          a LAMBDA whose STRATEGY in turn is JUMP
                  ;  EXIT    function is an EXIT stategy CATCH variable
                  ;  PROC    a call to a full procedure
                  ;  TAIL    like PROC but tail-recursive
                  ;  PRIMOP  open-coded
  )
