(HERALD DEFS
        (ENV T (TCOMP MACHAX)))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Declarations and macro support

;;; This file provides the compilation environment for compiling the
;;; various modules of the compiler.


;;;; Structure definitions

;;; Region structure...

;;; A region is the compile-time representation of a contiguously allocated
;;; chunk of memory: either a stack "frame" or a chunk of heap-allocated
;;; lexical environment.

(DEFVST REGION
  OWNER           ;Node for LAMBDA-exp which owns this region (debugging only?)
  TYPE            ;One of HEAP or STACK
  POINTER-MEM-SIZE  ;Number of temporary pointer locations used
  SCRATCH-MEM-SIZE  ;Number of temporary scratch locations used
  SIZE            ;Sum of POINTER-MEM-SIZE and SCRATCH-MEM-SIZE, plus 1 to
                  ; cause oddness, if needed.
; LINKS           ;A-list of (region tn index) for links from this region to
                  ;  other regions
; VARS            ;A-list of (variable tn) for variables of which there are
                  ;  copies in this region
; POINTER-BUCKETS ;List of buckets for rootable quantities
; SCRATCH-BUCKETS ;List of buckets for unrootable quantities
  )
  

;;; TN structure...

(DEFVST TN
  REGION          ;Region which owns this TN
                  ;  (maybe this should be a BUCKET instead?)
  (WEIGHT 0)      ;The importance of this TN, used to sort them
  (LONFC NIL)     ;Linear order number of first creation
  (LONLU -1)      ;Linear order number of last use
  (FONFC NIL)     ;Flow order number of first creation
  (FONLU -1)      ;Flow order number of last use
  (WANTLOC 'ANY)  ;RT, ARGUMENT, SCRATCH-REG, POINTER-REG,
                  ; SCRATCH-MEM, POINTER-MEM, STACK, or ANY
  ISLOC           ;Actual location of the temporary:
                  ;  WANTLOC          ISLOC
                  ;    SCRATCH-REG      a register name
                  ;    POINTER-REG      a register name
                  ;    SCRATCH-MEM      an index within the unmarked stack area
                  ;    POINTER-MEM      an index within the markable stack area
                  ;    STACK            an index within the stack (!)
                  ;(After the packing phase, ANY TN's have had their
                  ; WANTLOC's altered)
  (PREFERENCES '()) ;list of TNs to which this TN has been preferenced.
  (OWNER NIL)     ;The node that owns this TN (highest one that refers to it)
  (SIZE 1)
  (PTRP T)
  ID              ;A serial identification number
  (REASON NIL)    ;The reason this TN got packed in the place it did
                  ;  (for debugging only)
  (CONFLICTS '()) ; a list filled in during the pack phase (for debugging only)
                  ; If the switch *RECORD-TN-CONFLICTS* is set, then every time
                  ; a packing attempt fails the TN which caused the conflict
                  ; is pushed onto this list.
  ORDER           ;The order in which this TN was sorted before packing
                  ;  (for debugging only)
  (VAR NIL)       ;VARIABLE structure if this TN belongs to one, else nil
  REP             ;The represention of the object the TN holds
  (COMPATIBLES '())  ;List of TN's which, despite lifetime info to contrary,
                     ; may actually be packed in same bucket as this one
  )

(DEFPROP TN (PREFERENCES OWNER REASON) SUPPRESSED-COMPONENT-NAMES)

;;; Fields marked with * are only relevant if (EQ? NODE (NODE-LEVEL NODE)),
;;; i.e. this node's value is actually used by its parent rather than simply
;;; passed along in a tail-recursive way.

(DEFVST NODE
  (METAP NIL)    ;Non-nil iff this node has been examined by the meta-evaluator
  SUBSTP         ;Non-nil iff meta-evaluator made a substitution at or below
                 ;  here
  ;EXITP         ;If non-null, there is a GO or RETURN at or below this node.
                 ; ANY means there is a GO or RETURN somewhere below this node.
                 ;  ALL means that all legitimate control paths end in a GO
                 ;  or RETURN.
                 ;  LOSER means that there is an illegitimate GO or RETURN.
  (PARENT NIL)   ;The parent node of this one
  (REFS *EMPTY*) ;A list of all lexical variables bound above and referenced
                 ;  below
  (SETQS *EMPTY*) ;SETQ'd lexical variables (not necessarily a subset of REFS!)
  (EFFECTS *EMPTY*) ;Side effects caused at or below this node
  (AFFECTED *EMPTY*) ;Side effects by which this code may be affected
  (LEVEL *EMPTY*) ;The node to which this node's value will be delivered.
                 ;  If two levels are the same then the two pieces
                 ;  of code are at the same tail-recursion level, sort of
  (COMPLEXITY *EMPTY*)  ;A VERY rough guess at how big the code for this
                 ;  node will be, in bytes
  (DEPTH *EMPTY*) ;Number of parent pointers to follow to get to root of tree
  CONSENV        ;The consed environment at point when this node is evaluated
  (MIGRATIONS '()) ;New environment layer to cons in when node is evaluated
  WANTREP        ;The a posteriori representation for the result desired by the
                 ;  parent (*)
  ISREP          ;The representation which will actually be delivered by the
                 ;  node a priori
  ;TARGETP       ;True iff this node is on the target path for its parent
  RESTRICTION    ;Describes kind of operand required by parent.  NIL means
                 ; any operand is acceptable; other possibilities are
                 ; POINTER-REG, SCRATCH-REG, and INDIRECTABLE. (*)
  (WANTTN NIL)   ;The TN for the node when in rep desired by parent;
                 ;  should be NIL iff WANTREP is NONE (*)
  ISTN           ;The TN for the node as produced by the node code;
                 ;  should be NIL iff ISREP is NONE
  (SPARETNS '()) ;An a-list of identifiers and TN's for random purposes
  ETN            ;TN for consed environment during evaluation of this node
                 ; (that is, AFTER any migrations have happened)
  (HOLDP NIL)    ;True if value can't be aliased while other stuff happens (*)
  (RESULTTNS '()) ;List of TN's for values involved in delivering
                 ; this node's value (*)
  STACKNUM       ;Stack level at which to yield value (*)
  (GENTAG *EMPTY*) ;Place to go to yield the value of this node (*)
  FORM           ;One of the below types
  RETURNERS      ;List of nodes which yield values to this node (*)
  LOC            ;Operand which accesses node's value (*)
  )
(DEFPROP NODE (PARENT) SUPPRESSED-COMPONENT-NAMES)

(DEFINE-INTEGRABLE NODE-ALIASP NODE-HOLDP)
(DEFINE-INTEGRABLE NODE-ALIASTNS NODE-RESULTTNS)

(DEFINE-INTEGRABLE (LEVEL-ALIASP NODE) (NODE-ALIASP (NODE-LEVEL NODE)))

;;;; Macros

;;; NODE-DISPATCH is for use in the main function of each phase.
;;; It performs a dispatch of the type of a node, and calls a different
;;; function for each type.  Arbitrary arguments can be passed.
;;; --- Hack someday to do a vector lookup.

(DEFINE-MACRO (NODE-DISPATCH NAME NODE . ARGS)
  ;;`(FUNCALL (VREF ,(CONCATENATE-SYMBOL '* NAME '-DISPATCH-VECTOR*)
  ;;                (NODE-DISPATCH-INDEX ,NODE))
  ;;          ,NODE
  ;;          (NODE-FORM ,NODE)
  ;;          ,@ARGS)
  (LET ((GENVARS (MAP (LAMBDA (ARG) (IGNORE ARG) (GENERATE-SYMBOL 'DISPATCH))
                      ARGS)))
    `(LET ((FM (NODE-FORM ,NODE))
           ,@(MAP LIST GENVARS ARGS))
       (XCASE (STYPE FM)
         ,@(MAP (LAMBDA (TYPE)
                  `((,TYPE) (,(CONCATENATE-SYMBOL NAME '- TYPE)
                             ,NODE FM ,@GENVARS)))
                *NODE-TYPES*)))))

(DEFINE-MACRO (DEFDISPATCH NAME TYPE ARGS . BODY)
  (LET ((DEFDISPATCH-AUX
          (LAMBDA (NAME TYPE ARGS DEC BODY)
            (LET ((PROC (CONCATENATE-SYMBOL NAME '- TYPE)))
              `(BLOCK (DEFINE (,PROC ,@ARGS)
                        ,@DEC
                        (IGNORABLE ,@ARGS)
                        . ,BODY)
                      (*DEFDISPATCH (VAR-LOCATIVE
                                     ,(CONCATENATE-SYMBOL '* NAME
							  '-DISPATCH-VECTOR*))
                                    ',TYPE
                                    ,PROC))))))
    (COND ((AND (PAIR? (CAR BODY)) (EQ? (CAAR BODY) 'DECLARE))
           (DEFDISPATCH-AUX NAME TYPE ARGS
                    (LIST (CAR BODY)) (CDR BODY)))
          (ELSE
           (DEFDISPATCH-AUX NAME TYPE ARGS
                    '() BODY)))))

;;; Meta-evaluation macro

(DEFINE-MACRO (MEVAL NODE FROB)
  `(DO ((%%%%FROB%%%% ,FROB %%%%NEWFROB%%%%)
        (%%%%NEWFROB%%%% (META-EVALUATE ,FROB)
                         (META-EVALUATE %%%%NEWFROB%%%%)))
       ((EQ? %%%%NEWFROB%%%% %%%%FROB%%%%))
     ----
     (SET ,FROB %%%%NEWFROB%%%%)
     (NODE-PARENT-CHECK %%%%NEWFROB%%%% ,NODE)
     (REPROPAGATE %%%%NEWFROB%%%%)
     ---))

;;; Utility macros...

(DEFINE-MACRO (INCR FORM)
  `(MODIFY-LOCATION ,FORM (LAMBDA (FETCH STORE) (STORE (FX+ 1 (FETCH))))))

(DEFINE-INTEGRABLE (EMPTY X) (EQ? X *EMPTY*))

(DEFINE-INTEGRABLE (INT X) (CONS *INTERNAL* X))

(DEFINE-MACRO (ADJOINF VALUE SLOT)              ;?
  `(SET ,SLOT (ADJOIN ,VALUE ,SLOT)))

(DEFINE-MACRO (BUGLET BINDINGS ERRORMSG FIXMSG . ITEMS)
  `(BIND ,BINDINGS
     (BUG ',(FORMAT NIL "~A ~A" ERRORMSG        ;Losing FORMAT returns |...|
                        (CONS "look at" (MAP CAR BINDINGS)))
          ,FIXMSG . ,ITEMS)))

;;; Declaration stuff

(DEFINE-MACRO (STAT-COUNTER COUNTER DOCUMENTATION)
  `(BLOCK (LSET ,COUNTER 0)
          (*DEFINE-STAT-COUNTER (VAR-LOCATIVE ,COUNTER) ',DOCUMENTATION)))

(DEFINE-MACRO (COMPILATION-GLOBAL NAME)
  `(BLOCK (LSET ,NAME '*UNDEFINED*)
          (SET *COMPILATION-GLOBALS*
               (ADJOIN (VAR-LOCATIVE ,NAME) *COMPILATION-GLOBALS*))))

(DEFINE-MACRO (FUNREP ISREP WANTREP . FUNS)
  `(BLOCK (WALK (LAMBDA (F)
                  ,@(AND ISREP `((CPUT F 'ISREP ',ISREP)))
                  ,@(AND WANTREP `((CPUT F 'WANTREP ',WANTREP))))
                ',FUNS)
          ',FUNS))

;;; Stuff for debugging output

(DEFINE-MACRO (DUMP-INTO-DEBUGGING-FILE FILE . WHAT)
  `(BIND ((*NOISE-OUTPUT* (MAKE-BROADCAST-STREAM ,FILE *NOISE-MINUS-TERMINAL*)))
     . ,WHAT))

(DEFINE-MACRO (DUMP-CRUFT PLACE . WHAT)
  `(COND ((EQ? ,PLACE T) . ,WHAT)
         ((EQ? ,PLACE 'D)
          (BIND ((*NOISE-OUTPUT* *NOISE-MINUS-TERMINAL*))
            . ,WHAT))
         ((NOT (NULL? ,PLACE))
          (DUMP-INTO-DEBUGGING-FILE (CDR ,PLACE) . ,WHAT))))

;;; Stuff for code emission

(DEFINE-MACRO (EMITFORMAT . REST)
  `(IF *ENABLE-LAP-COMMENTARY?* (EMITREMARK (FORMAT NIL . ,REST))))

(LSET *LAP-CONSTANTS* '())

(DEFINE-MACRO (DEFINE-LAP-CONSTANT NAME VALUE)
  `(BLOCK (DEFINE-CONSTANT ,(CONCATENATE-SYMBOL 'TARGET: NAME) ,VALUE)
          (*DEFINE-LAP-CONSTANT ',NAME ,VALUE)))

;;; This hack makes it work to compile and load support files in the TC env.

(DEFINE-MACRO (SYSBUILD-ITEM . REST) (IGNORE REST) ''FOO)
