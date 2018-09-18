(HERALD (TCOMP TARGETIZE T 45)
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University


;;;; Temporary name and flow number assignment

;;; There should be a separate operand-aliasing analysis phase.

;;; Note that *STACKNUM* represents the number of words pushed onto the
;;; stack so far, thus "points" to the location just past where the actual
;;; stack pointer points.  It starts out being 0.  Targeting a node for
;;; WANTLOC=STACK and ISLOC=*STACKNUM* results in a push in the ultimate
;;; object code.

(LSET *TARGET-ANNOTATE-NODE* NIL)
(LSET *REGION* NIL)
(LSET *TNS* NIL)
(LSET *CALL-FON-FONS* NIL)
(LSET *STACKNUM* NIL)
(LSET *LON* NIL)
(LSET *FON* NIL)

(DEFINE (TARGET-ANNOTATE NODE)
  (BIND ((*TARGET-ANNOTATE-NODE* NODE))
    (IF (NOT (LAMBDA-NODE? NODE))
        (BUGLET ((*NODE* NODE))
                "attempt to use TARGET-ANNOTATE on a non-LAMBDA"
                "can't possibly win here"))
    (BIND ((*REGION* (CONS-A-REGION TYPE 'STACK
                                    OWNER NODE))
           (*TNS* '())
           (*CALL-LON-FONS* '())
           (*STACKNUM* 0)
           (*LON* 0)
           (*FON* 0))
      (SET (LAMBDA-REGION (NODE-FORM NODE)) *REGION*)
      (TARGET-ANNOTATE-1 NODE (NODE-FORM NODE))
      (COND ((NOT (NULL? *CALL-LON-FONS*))
             ;; LAMBDA-HAS-CALLS: hack for GENERATE-CALL-PROC
             (SET (LAMBDA-HAS-CALLS (NODE-FORM NODE)) T)
             (LET ((CALL-LON-FONS (BALANCE-CALL-LON-FONS *CALL-LON-FONS*)))
               (WALK (LAMBDA (TN)
                       (FORCE-TN-INTO-MEMORY-IF-ACROSS-CALL TN CALL-LON-FONS))
                     *TNS*)
               )))
      (SET *TNS* (SORT-TNS *TNS*))
      (PACK-TNS NODE)
      )))

(DEFINE (TARGET-ANNOTATE-1 NODE FM)
  (LET ((FTN (COND ((NULL? (NODE-CONSENV NODE)) NIL)
                   (ELSE
                    (XCASE (LAMBDA-STRATEGY FM)
                        ((TPROC) NIL)
                        ((PROC EZCLOSE STACK)
                         (GENTN 'POINTER-REG *FUN* NODE))
                        ((HANDLER METHOD)
                         ;; By special dispensation, methods and handlers are
                         ;;  passed their environments as their first arg.
                         (GENTN 'POINTER-MEM 0 NODE)))))))
    (COND (FTN (SET (TN-OWNER FTN) NODE)
               (SETSPARETN NODE 'FTN FTN)
               (RECORD-TN-USE FTN)))
    ;; First, set up ARGTNS for the places in which we're passed our
    ;; arguments.
    (DO ((J 0 (FX+ J 1))
         (V (LAMBDA-VARS FM) (CDR V))
         (Z '() (CONS (LET ((ARGTN (GENTN 'POINTER-MEM J NODE)))
                        (SET (TN-OWNER ARGTN) NODE)
                        (RECORD-TN-USE ARGTN)   ; in case the variable is ()
                        ARGTN)
                      Z)))
        ((NULL? V)
         (SET (LAMBDA-ARGTNS FM) (REVERSE! Z))))
    ;; Next, the rest-arguments get popped off the stack into the restvar.
    (COND ((NOT (EMPTY (LAMBDA-RESTVAR FM)))
           (LET ((RTN (GENTN 'POINTER-REG *VAL* NODE)))
             (SET (TN-OWNER RTN) NODE)
             (RECORD-TN-USE RTN)
             (COND ((LAMBDA-RESTVAR FM)
                    (LET ((TN (GENTN 'ANY NIL NODE)))
                      (SET (VARIABLE-TN (LAMBDA-RESTVAR FM)) TN)
                      (SET (TN-VAR TN) (LAMBDA-RESTVAR FM))
                      (PREFTN RTN TN)
                      (RECORD-TN-USE TN)))))))
    ;; Now the parameters get stowed in the homes they'll have in the body.
    (WALK (LAMBDA (V ARGTN)
            (COND (V (RECORD-TN-USE ARGTN)
                     (LET ((VTN (GENTN 'ANY NIL NODE)))
                       (SET (VARIABLE-TN V) VTN)
                       (SET (TN-VAR VTN) V)
                       (RECORD-TN-USE VTN)
                       (PREFTN VTN ARGTN)))))
          (LAMBDA-VARS FM)
          (LAMBDA-ARGTNS FM))
    ;; At last, process the procedure's body.
    (TARGETIZE (LAMBDA-BODY FM)
               (GENTN 'POINTER-REG *VAL* NODE)
               (COND (FTN (LET ((ETN (GENTN 'ANY NIL NODE)))
                            (SETSPARETN NODE 'BODY-ETN ETN)
                            (RECORD-TN-USE FTN)
                            (IF (EQ? NODE (CAR (NODE-CONSENV (LAMBDA-BODY FM))))
                                (PREFTN FTN ETN))
                            (RECORD-TN-USE ETN)
                            ETN))
                     (ELSE NIL)))
    (TNUSE (LAMBDA-BODY FM))))

;;; PREFTN causes two TN's to be preferenced to each other.

(DEFINE (PREFTN TN1 TN2)
  (COND (*PREFTN-TRACE*
         (NEWLINE *NOISE-OUTPUT*)
         (WRITES *NOISE-OUTPUT* ";$$$$$ Preferencing this TN:")
         (DEBUGTN TN1)
         (WRITES *NOISE-OUTPUT* ";$$$$$ to this TN:")
         (DEBUGTN TN2)))
  (COND ((OR (NULL? TN1) (NULL? TN2))
         (BUGLET ((*TN1* TN1) (*TN2* TN2))
                 "an argument to PREFTN was null"
                 "will fail to do any TN preferencing"))
        (ELSE
         (PUSH (TN-PREFERENCES TN2) TN1)
         (PUSH (TN-PREFERENCES TN1) TN2))))


(STAT-COUNTER *TN-SERIAL-NUMBER* "number of TN's generated")

;;; GENTN generates a new TN, which is added to the global list *TNS* of
;;; all generated TN's and returned.

(DEFINE (GENTN WANTLOC ISLOC NODE)
  (LET ((TN (CONS-A-TN ID *TN-SERIAL-NUMBER*
                       REGION *REGION*
                       WANTLOC WANTLOC
                       ISLOC ISLOC
                       OWNER NODE)))
    (AND *TN-BREAK*
         (MEMBER *TN-SERIAL-NUMBER* *TN-BREAK*)
         (*BREAK T `(GENERATING TN NUMBER ,*TN-SERIAL-NUMBER*)))
    (INCR *TN-SERIAL-NUMBER*)
    (PUSH *TNS* TN)
    TN))

;;; REMTN removes a TN from existence.
;;;? do we want this counter?
(STAT-COUNTER *REMTN-COUNT* "number of TN's removed by REMTN")

(DEFINE (REMTN TN)
  (COND (*REMTN-TRACE*
         (NEWLINE *NOISE-OUTPUT*)
         (WRITES *NOISE-OUTPUT* ";$$$$$ Removing this TN:")
         (DEBUGTN TN)))
  (DO ((P (TN-PREFERENCES TN) (CDR P)))
      ((NULL? P))
    ----
    (SET (TN-PREFERENCES (CAR P)) (DELQ TN (TN-PREFERENCES (CAR P))))
    ---)
  (SET *TNS* (DELQ TN *TNS*))
  ;;? do we want this counter?
  (INCR *REMTN-COUNT*))

;;; This is for debugging purposes.

(DEFINE (DEBUGTN TN)
  (DEBUGTN1 TN)
  (BIND ((*PRINT-LEVEL* 3)
         (*PRINT-LENGTH* 6))
    (DO ((P (TN-PREFERENCES TN) (CDR P)))
        ((NULL? P))
      ----
      (DEBUGTN1 (CAR P)))
    (NEWLINE *NOISE-OUTPUT*)
    '*))

(DEFINE (DEBUGTN1 TN)
  (FORMAT *NOISE-OUTPUT*
          "~%ID=~D PREFERENCES=~S "
          (TN-ID TN)
          (TN-ID (TN-PREFERENCES TN)))
  (COND ((NULL? (TN-OWNER TN)) (WRITES *NOISE-OUTPUT* "<no owner>"))
        (ELSE
         (COND ((EQ? TN (NODE-WANTTN (TN-OWNER TN)))
                (COND ((EQ? TN (NODE-ISTN (TN-OWNER TN)))
                       (FORMAT *NOISE-OUTPUT* "~A WANTTN/ISTN"
                               (NODE-ISREP (TN-OWNER TN))))
                      (ELSE (FORMAT *NOISE-OUTPUT* "~A WANTTN"
                                 (NODE-WANTREP (TN-OWNER TN))))))
               ((EQ? TN (NODE-ISTN (TN-OWNER TN)))
                (FORMAT *NOISE-OUTPUT* "~A ISTN"
                        (NODE-ISREP (TN-OWNER TN))))
               ((TN-VAR TN)
                (FORMAT *NOISE-OUTPUT* "Variable ~A"
                        (VARIABLE-IDENTIFIER (TN-VAR TN))))
               (ELSE (WRITES *NOISE-OUTPUT* "Random TN")))
         (FORMAT *NOISE-OUTPUT* " of OWNER=~S" (SEXPRFY (TN-OWNER TN))))))


(DEFINE (SETSPARETN NODE NAME TN)
  (LET ((PROBE (ASSQ NAME (NODE-SPARETNS NODE))))
    (COND ((NULL? PROBE)
           (SET (TN-OWNER TN) NODE)
           (PUSH (NODE-SPARETNS NODE) (CONS NAME TN)))
          ((NEQ? (CDR PROBE) TN)
	   (COMMENT
           (BUGLET ((*NODE* NODE))
                   "TN ~D and TN ~D both have the spare name ~S"
                   "will assume TN ~D retains the name and discard TN ~D"
                   (TN-ID (CDR PROBE))
                   (TN-ID TN)
                   NAME
                   (TN-ID (CDR PROBE))
                   (TN-ID TN)))) )
    TN))

(DEFINE (GETSPARETN NODE NAME)
  (CDR (ASSQ NAME (NODE-SPARETNS NODE))))

;Used to do this if it didn't find the sparetn.
;      (BLOCK (BUGLET ((*NODE* NODE))
;                    "node has no spare TN named ~S"
;                    "will return a bogus TN [what else can I do?]"
;                    NAME)
;            (LET ((*TNS* '())) (GENBOGUSTN)))


;;; TNYIELD is to TARGETIZE as YIELD is to GENERATE.

;;; Doing (TNCREATE FOO) causes FOO's ISTN to be annotated as being created
;;; at that time (with respect to *LON* and *FON*).  If the WANTTN is distinct
;;; from the ISTN then the ISTN is immediately used and the WANTTN created.

(DEFINE (TNYIELD NODE ISTN)
  (LET ((WANTTN (NODE-WANTTN (NODE-LEVEL NODE))))
    (COND (ISTN
           (SET (NODE-ISTN NODE) ISTN)
           (IF (NULL? (TN-OWNER ISTN)) (SET (TN-OWNER ISTN) NODE))
           (TNREP ISTN (NODE-ISREP NODE))
           (RECORD-TN-USE ISTN)))
    ;; Shouldn't this be more discriminating per different kinds of rep
    ;;  conversion?
    (COND ((AND ISTN WANTTN (NEQ? ISTN WANTTN))
           (PREFTN ISTN WANTTN)))
    (COND (WANTTN
           ;; Create a TN to use in converting representations, if needed
           ;; This really needs to be much hairier.
           (LET ((CTN (TN-REQUIRED-FOR-REP-CONVERSION NODE)))
             (COND (CTN
                    (SETSPARETN NODE 'CONVERT-REP CTN)
                    (RECORD-TN-USE CTN)
                    (COND (ISTN (RECORD-TN-USE ISTN)
                                ;; (PREFTN CTN ISTN) ;Sigh.
                                ))
                    (PREFTN CTN WANTTN))))
           ;; Now use the WANTTN if it is there and distinct from the ISTN
;;; WRONG WRONG WRONG
;;;        (RECORD-TN-USE WANTTN)
           ))))

(DEFINE (TN-REQUIRED-FOR-REP-CONVERSION NODE)
  (LET ((ISREP (NODE-ISREP NODE))
        (WANTREP (NODE-WANTREP (NODE-LEVEL NODE))))
    (COND ((OR (NULL? (NODE-WANTTN (NODE-LEVEL NODE))) (EQ? ISREP WANTREP))
           NIL)
          (ELSE (LET ((FOO (REP-CONVERTER ISREP WANTREP)))
               (COND ((CDR FOO)
                      (LET ((TN (GENTN (CADR FOO) (CADDR FOO) NODE)))
                        (TNREP TN ISREP)
                        TN))
                     (ELSE NIL)))))))

;;; Doing (TNUSE FOO) causes FOO's WANTTN to be annotated as being used
;;; at that time (with respect to *LON* and *FON*).

(DEFINE (TNUSE NODE)
  (WALK RECORD-TN-USE (NODE-ALIASTNS NODE))
  (COND ((NOT (NULL? (NODE-WANTTN NODE)))
         (RECORD-TN-USE (NODE-WANTTN NODE)))))


;;; (ONMAX (TN-LONLU FOO) N), for example, sets (TN-LONLU FOO) to the greater
;;; of its current value and N.  If the current value is (), it means minus
;;; infinity; i.e. (TN-LONLU FOO) is simply set to N.

(DEFINE-MACRO (ONMAX ON VAL)
  `(SET ,ON (COND ((NULL? ,ON) ,VAL)
                   (ELSE (FIXNUM-MAX ,ON ,VAL)))))

;;; This low-level primitive just does some error-checking and then installs
;;; the *LON* and *FON* values as known use times for the given TN.

(DEFINE (RECORD-TN-USE TN)
  (COND ((NULL? TN)
;        (BUG "trying to note use of a null TN"
;             "will ignore the attempt")
         )
        ((EQ? (TN-REGION TN) *REGION*)
         (COND ((NULL? (TN-LONFC TN))
                (SET (TN-LONFC TN) *LON*)
                (SET (TN-FONFC TN) *FON*)))
         (ONMAX (TN-LONLU TN) *LON*)
         (ONMAX (TN-FONLU TN) *FON*)
         (INCR *LON*)
         (INCR *FON*))
        (ELSE
         (BUGLET ((*TN* TN))
                 "TN doesn't belong to current region"
                 "will not update its lifetimes"))))

;;; Record use of a variable's TN.
(DEFINE (RECORD-VARIABLE-TN-USE V)
  (RECORD-TN-USE (VARIABLE-TN V)))


;;; Restructure *THE-LONS* to be a balanced binary tree.

(DEFINE (BALANCE-CALL-LON-FONS THE-LONS)
  (BIND ((*THE-LONS* THE-LONS))
    (DO ((HEIGHT 0 (FX+ HEIGHT 1))
         (TREE (BALANCE-CALL-LON-FONS-1 0)
               (RPLACD (BALANCE-CALL-LON-FONS-1 0)
                       (CONS (BALANCE-CALL-LON-FONS-1 HEIGHT) TREE))))
        ((NULL? *THE-LONS*) TREE))))

(DEFINE (BALANCE-CALL-LON-FONS-1 HEIGHT)
  (DECLARE (SPECIAL *THE-LONS*))
  (COND ((NULL? *THE-LONS*) (LIST (CONS -1 -1)))
        ((FX= HEIGHT 0)
         (LET ((FOO *THE-LONS*))
              (SET *THE-LONS* (CDR *THE-LONS*))
              (RPLACD FOO '())))
        (ELSE
         (LET ((FOO (BALANCE-CALL-LON-FONS-1 (FX- HEIGHT 1))))
            (RPLACD (BALANCE-CALL-LON-FONS-1 0)
                    (CONS (BALANCE-CALL-LON-FONS-1 (FX- HEIGHT 1)) FOO))))))

;;; Predicate -- returns T iff it did a forcing.

(DEFINE (FORCE-TN-INTO-MEMORY-IF-ACROSS-CALL TN CALL-LON-FONS)
  (COND ((FX> (TN-LONFC TN) (CAAR CALL-LON-FONS))
         (AND (CDR CALL-LON-FONS)
              (FORCE-TN-INTO-MEMORY-IF-ACROSS-CALL TN (CDDR CALL-LON-FONS))))
        ((FX< (TN-LONLU TN) (CAAR CALL-LON-FONS))
         (AND (CDR CALL-LON-FONS)
              (FORCE-TN-INTO-MEMORY-IF-ACROSS-CALL TN (CADR CALL-LON-FONS))))
        ((OR (FX> (TN-FONFC TN) (CDAR CALL-LON-FONS))
             (FX< (TN-FONLU TN) (CDAR CALL-LON-FONS)))
         (AND (CDR CALL-LON-FONS)
              (OR (FORCE-TN-INTO-MEMORY-IF-ACROSS-CALL TN (CADR CALL-LON-FONS))
                  (FORCE-TN-INTO-MEMORY-IF-ACROSS-CALL TN (CDDR CALL-LON-FONS)))))
        (ELSE
         (XCASE (TN-WANTLOC TN)
           ((ANY)
            (SET (TN-WANTLOC TN)
                  (COND ((TN-PTRP TN) 'POINTER-MEM)
                        (ELSE         'SCRATCH-MEM)))
            (SET (TN-REASON TN) 'ACROSS-CALL))
           ((STACK POINTER-MEM SCRATCH-MEM) NIL)
           ((POINTER-REG SCRATCH-REG)
            ;; Whenever this situation has come up, it has been because
            ;; the LON-FON lifetime characterization was too coarse, and in
            ;; fact there was nothing wrong at all.  (In particular, file
            ;; REPL.T lost on this account.)  So there's really no reason
            ;; to produce a bug report here, because correct code will come
            ;; out in the end.
            ;(BUGLET ((*TN* TN))
            ;        "TN can't be forced into memory"
            ;        "will not try to force it, but something's fishy here")
            ))
         T)))

(DEFINE (TNREP TN REP)
  (COND ((NULL? TN)
         (OR (EQ? REP 'NONE)
             (BUG
              "a null TN was used, but the desired rep was ~S instead of NONE"
              "will not set the TN's PTRP, SIZE, and REP fields"
              REP)))
        (ELSE
         (SET (TN-REP TN) REP)
         (SET (TN-PTRP TN)
               (COND ((MEMQ REP '(POINTER JUMP NONE)) T)
                     ((EQ? REP 'SWFIX) 'MAYBE)
                     (ELSE NIL)))
         (SET (TN-SIZE TN)
               (OR (GET REP 'REP-SIZE) 1)))))

(PUT 'QFIX  'REP-SIZE 2)
(PUT 'DWFIX 'REP-SIZE 2)
(PUT 'DWFLO 'REP-SIZE 2)


(DEFINE (MAYBE-SET-ALIASP NODE ALIASP)
  (SET (NODE-ALIASP NODE)
        (COND ((NULL? (NODE-RETURNERS NODE)) 'NO-RETURNERS)
              ((NULL? (CDR (NODE-RETURNERS NODE))) ALIASP)
              (ELSE NIL))))

;;; ... TARGETIZE

;;; Values are "delivered" by constant, variable, call, lambda,
;;; and setq nodes.  This "delivery" is implemented by TNYIELD.

;;; WANTTARGET should be non-NIL iff the parent node is using the node's value;
;;; i.e. (AND (EQ? NODE (NODE-LEVEL NODE))
;;;           (NEQ? (NODE-WANTREP NODE) 'NONE))

;;; ETN is either () or a TN for the consed environment.
;;; The ETN passed to TARGETIZE corresponds to the environment BEFORE any
;;;  migrations performed at the node, whereas the ETN slot in the node
;;;  corresponds to the environment of the node itself.

;;; - Fix so that the WANTTN refers specifically to the place where the
;;;   value is "kept" until the parent of the node's LEVEL wants it?
;;;   Then have a TN for the LEVEL.

(DEFINE (TARGETIZE NODE WANTTARGET ETN)
  (COND (WANTTARGET
         (SET (NODE-WANTTN NODE) WANTTARGET)
         (IF (NULL? (TN-OWNER WANTTARGET))
             (SET (TN-OWNER WANTTARGET) NODE))
         (TNREP WANTTARGET (NODE-WANTREP NODE))))
  (LET ((ETN (COND ((NODE-MIGRATIONS NODE)
                    (TARGETIZE-MIGRATIONS NODE ETN))
                   (ELSE ETN))))
    (SET (NODE-ETN NODE) ETN)
    (NODE-DISPATCH TARGETIZE NODE ETN)
    ;; The node's STACKNUM may get clobberred later by code for parent node
    (SET (NODE-STACKNUM NODE) *STACKNUM*)
    (COND ((AND (EQ? NODE (NODE-LEVEL NODE)) WANTTARGET)
           ;; Now use the WANTTN if it is there and distinct from the ISTN
           (RECORD-TN-USE WANTTARGET)
           ))))

(DEFINE (TARGETIZE-MIGRATIONS NODE ETN)
  (LET ((YTN (GENTN 'POINTER-REG NIL NODE))     ;For consing the env.
        (ZTN (GENTN 'ANY NIL NODE)))    ;Env will live here in body of node.
    (SETSPARETN NODE 'ENVCONS YTN)
    (RECORD-TN-USE YTN)
    (WALK (LAMBDA (VAR) 
            (IF (OR (EMPTY (NODE-DEPTH (VARIABLE-BINDER VAR)))
                    (FX< (NODE-DEPTH (VARIABLE-BINDER VAR))
                         (NODE-DEPTH NODE)))    ; avoid LABELS screw case
                (RECORD-TN-USE (VARIABLE-TN VAR))))
          (NODE-MIGRATIONS NODE))
    (COND ((PAIR? (CADR (NODE-CONSENV NODE)))
           (SETSPARETN NODE 'PARENT-ETN ETN)
           (RECORD-TN-USE ETN)))
    (RECORD-TN-USE YTN)
    (PREFTN YTN ZTN)
    (RECORD-TN-USE ZTN)
    ZTN))

(DEFDISPATCH TARGETIZE CONSTANT (NODE FM ETN)
  (IGNORE FM ETN)
  ;; No ISTN required.
  (TNYIELD NODE NIL))

;;; Common utility for nodes which refer to the consed env: var refs, setq's,
;;; closure creation, and EZCLOSE calls.  REF is either a VARIABLE structure
;;; or a consenv list, and is given simply for the purpose of deciding whether
;;; or not to preference the register TN for the reference to the TN for the
;;; env.  Gross hackery.
;;; NAME is the SPARETN tag under which any necessary sparetn should be stowed.

(DEFINE (TARGETIZE-CONSENV-REF NODE REF ETN NAME)
  (RECORD-TN-USE ETN)
  (INCR (TN-WEIGHT ETN))
  (LET ((BTN (GENTN 'POINTER-REG NIL NODE)))
    (SETSPARETN NODE NAME BTN)
    (IF (MEMQ REF (CDR (NODE-CONSENV NODE)))
        (PREFTN ETN BTN))
    (RECORD-TN-USE BTN)))

;;; The following is the common code for VARIABLE and SETQ cases.

;;; True if a ref to this variable here will be through the consed env.

(DEFINE (CONSENV-REF? VAR)              ; *REGION*
  (AND (VARIABLE-TN VAR)
       (VARIABLE-CLOSURE-REFS VAR)
       (OR (NEQ? (TN-REGION (VARIABLE-TN VAR)) *REGION*)
           (VARIABLE-WRITE-REFS VAR))))

(DEFINE (VARIABLE-REFTYPE VAR)
  (COND ((NULL? (VARIABLE-TN VAR)) 'NONE)
        ((AND (VARIABLE-CLOSURE-REFS VAR)
              (OR (NEQ? (TN-REGION (VARIABLE-TN VAR)) *REGION*)
                  (VARIABLE-WRITE-REFS VAR)))
         'CONSENV)
        (ELSE 'STACK-OR-REG)))

(DEFINE (TARGETIZE-VARIABLE-REF NODE VAR ETN)
  (XCASE (VARIABLE-REFTYPE VAR)
    ((CONSENV)
     (TARGETIZE-CONSENV-REF NODE VAR ETN 'ENVREF))
    ((STACK-OR-REG)
     (RECORD-TN-USE (VARIABLE-TN VAR))
     (INCR (TN-WEIGHT (VARIABLE-TN VAR))))
    ((NONE) NIL)))

(DEFDISPATCH TARGETIZE STATIC (NODE FM ETN)
  (IGNORE FM ETN)
  (IF (NOT *TARGET-INDIRECTION?*)
      (LET ((STN (GENTN 'POINTER-REG NIL NODE)))
        (RECORD-TN-USE STN)
        (SETSPARETN NODE 'STATIC-REF STN)
        (IF (NODE-ALIASP (NODE-LEVEL NODE))
            (SET (NODE-ALIASTNS (NODE-LEVEL NODE)) (LIST STN)))))
  (TNYIELD NODE NIL))

;;; Should clobber the WANTTN to be the variable's TN, if reps are the same.

(DEFDISPATCH TARGETIZE VARIABLE (NODE FM ETN)
  (TARGETIZE-VARIABLE-REF NODE FM ETN)
  ;; Kludgey.  Will look much better when we have variant records.
  (LET ((BTN (GETSPARETN NODE 'ENVREF)))
    (COND (BTN
           ;; Permit aliasing for closure-refd vars.
           (IF (NODE-ALIASP (NODE-LEVEL NODE))
               (SET (NODE-ALIASTNS (NODE-LEVEL NODE)) (LIST BTN)))
           (TNYIELD NODE NIL))
          (ELSE
           (IF (NODE-ALIASP (NODE-LEVEL NODE))
               (SET (NODE-ALIASTNS (NODE-LEVEL NODE))
                     (LIST (VARIABLE-TN FM))))
           (TNYIELD NODE (VARIABLE-TN FM))))))

;;; Why not use the variable's TN as the target for the body?

(DEFDISPATCH TARGETIZE SETQ (NODE FM ETN)
  ;; This is pretty horrible.
  (LET ((VAR (SETQ-VAR FM)))
    (MAYBE-SET-ALIASP (SETQ-BODY FM)
                      (AND (NULL? (NODE-WANTTN (NODE-LEVEL NODE)))
                           (IF (STATIC? VAR)
                               *TARGET-INDIRECTION?*
                             (EQ? (VARIABLE-REFTYPE (SETQ-VAR FM))
                                  'STACK-OR-REG))))
    (LET ((TARGET (GENTN 'ANY NIL NODE)))
      (TARGETIZE (SETQ-BODY FM) TARGET ETN)
      (COND ((STATIC? VAR)
             (IF (NOT *TARGET-INDIRECTION?*)
                 (LET ((STN (GENTN 'POINTER-REG NIL NODE)))
                   (RECORD-TN-USE STN)
                   (SETSPARETN NODE 'STATIC-REF STN))))
            (ELSE
             (TARGETIZE-VARIABLE-REF NODE (SETQ-VAR FM) ETN)))
      (TNUSE (SETQ-BODY FM))
      (TNYIELD NODE TARGET))))          ; ?

(DEFDISPATCH TARGETIZE LAMBDA (NODE FM ETN)
  (CASE (LAMBDA-STRATEGY FM)
    ((JUMP)                             ;Body gets targeted later.
     (SET (LAMBDA-ARGTNS FM)
           (MAP GEN-VARIABLE-TN (LAMBDA-VARS FM))))
    ((EZCLOSE TPROC HANDLER METHOD)
     (TNYIELD NODE NIL)
     (TARGET-ANNOTATE NODE))
    ((PROC)
     (COND ((NULL? (NODE-CONSENV NODE))
            (TNYIELD NODE NIL))
           (T
            (TARGETIZE-PROC-LAMBDA NODE ETN)))
     (TARGET-ANNOTATE NODE))))

;;; Note that the closure's environment has already been consed at this point.

(DEFINE (TARGETIZE-PROC-LAMBDA NODE ETN)
  (TNYIELD NODE
           (COND ((EQ? (CAR (NODE-CONSENV NODE)) NODE)
                  ETN)
                 (T
                  (LET ((TARGET (GENTN 'POINTER-REG NIL NODE)))
                    (RECORD-TN-USE TARGET)
                    (RECORD-TN-USE ETN)
                    TARGET)))))

(DEFDISPATCH TARGETIZE CATCH (NODE FM ETN)
  (COND ((MEMQ (CATCH-STRATEGY FM) '(PROC TAIL))
         (LET ((SPARETN (GENTN 'POINTER-REG NIL NODE))
               (VTN (GEN-VARIABLE-TN (CATCH-VAR FM))))
           (SETSPARETN NODE 'CATCH SPARETN)
           (RECORD-TN-USE SPARETN)
           (RECORD-TN-USE VTN)
           (PREFTN SPARETN VTN))))
  (COND ((EQ? (CATCH-STRATEGY FM) 'PROC) 
         (BIND ((*STACKNUM* *STACKNUM*))
           (push *CALL-LON-FONS* (CONS *LON* *FON*))
           (LET ((TARGET (GENTN 'POINTER-REG *VAL* NODE)))
             (IF (ODD? *STACKNUM*) (INCR *STACKNUM*))
             (INCR *STACKNUM*)  ;Slot for return address
             (TARGETIZE (CATCH-BODY FM) TARGET ETN)
             (TNYIELD NODE TARGET))))
        (T
         (TARGETIZE (CATCH-BODY FM) NIL ETN))))

(DEFDISPATCH TARGETIZE LABELS (NODE FM ETN)
  ;; Create TN's, if appropriate, for the variables.
  (WALK GEN-VARIABLE-TN (LABELS-VARS FM))
  ;; Process the LABELS values.
  (WALK (LAMBDA (A V)
          (TARGETIZE A (IF V (VARIABLE-TN V)) ETN)
          (IF (NODE-MIGRATIONS A)
              (BUGLET ((*NODE* A))
                      "a LABEL has migrations - what to do?"
                      "will proceed to generate wrong code"))
          (IF (MEMQ V (NODE-MIGRATIONS NODE))
              (TARGETIZE-CONSENV-REF A V ETN 'LABEL)))
        (LABELS-VALS FM)
        (LABELS-VARS FM))
  (WALK TNUSE (LABELS-VALS FM))
  (TARGETIZE (LABELS-BODY FM) NIL ETN)
  (TARGETIZE-JUMP-LAMBDAS (LABELS-VALS FM) ETN) 
  (TARGETIZE-NOTE-REFS (LABELS-REFS FM) ETN)
  (TARGETIZE-NOTE-REFS (LABELS-SETQS FM) ETN)
  (RECORD-TN-USE ETN))

(DEFINE (TARGETIZE-NOTE-REFS REFS ETN)
  (WALK (LAMBDA (VAR)
          (COND ((NOT (VARIABLE-TN VAR)))
                ((CONSENV-REF? VAR)
                 (RECORD-TN-USE ETN))
                (T
                 (RECORD-VARIABLE-TN-USE VAR))))        ;! for looping
        REFS))

(DEFDISPATCH TARGETIZE BLOCK (NODE FM ETN)
  (IGNORE NODE)
  (WALKCDR (LAMBDA (A)
             (TARGETIZE (CAR A) NIL ETN))
           (BLOCK-ARGS FM)))

(DEFDISPATCH TARGETIZE IF (NODE FM ETN)
  (IGNORE NODE)
  (MAYBE-SET-ALIASP (IF-PRED FM) T)     ; Compare takes any operand type (?)
  (TARGETIZE (IF-PRED FM)
             (IF (EQ? (NODE-WANTREP (IF-PRED FM)) 'NONE) NIL
               (GENTN 'ANY NIL (IF-PRED FM)))
             ETN)
  (LET ((OLDFON *FON*))
    (TARGETIZE (IF-CON FM) NIL ETN)
    (LET ((NEWFON *FON*))
      (SET *FON*
            (MAX NEWFON
                 (BIND ((*FON* OLDFON))
                   (TARGETIZE (IF-ALT FM) NIL ETN)
                   *FON*))))))

(DEFDISPATCH TARGETIZE CASE (NODE FM ETN)
  (IGNORE NODE)
  (TARGETIZE (CASE-KEY FM) (GENTN 'ANY NIL (CASE-KEY FM)) ETN)
  (LET ((OLDFON *FON*))
    (TARGETIZE (CASE-ELSE FM) NIL ETN)
    (WALK (LAMBDA (C) 
            (LET ((NEWFON *FON*))
              (SET *FON*
                    (MAX NEWFON
                         (BIND ((*FON* OLDFON))
                           (TARGETIZE (CDR C) NIL ETN)
                           *FON*)))))
          (CASE-CLAUSES FM))))

;;; EEK!  Must distinguish those EZCLOSE calls that are tail-recursive from
;;; those that aren't.

(DEFDISPATCH TARGETIZE CALL (NODE FM ETN)
  (XCASE (CALL-STRATEGY FM)
    ((LET)  (TARGETIZE-CALL-LET  NODE FM ETN))
    ((JUMP) (TARGETIZE-CALL-JUMP NODE FM ETN))
    ((EXIT) (TARGETIZE-CALL-EXIT NODE FM ETN))
    ((PROC) (TARGETIZE-CALL-PROC NODE FM ETN))
    ((TAIL) (TARGETIZE-CALL-TAIL NODE FM ETN))
    ((ENTITY) (TARGETIZE-CALL-ENTITY NODE FM ETN))
    ((PRIMOP) (TARGETIZE-CALL-PRIMOP NODE FM ETN)))
  (TARGETIZE-JUMP-LAMBDAS (CALL-ARGS FM) ETN))

;;; Utility for targeting JUMP strategy LAMBDA's.  These occur as arguments
;;; in a CALL, as well as in the values list of a LABELS.
;;; Called by TARGETIZE-LABELS as well as by TARGETIZE-CALL.

(DEFINE (TARGETIZE-JUMP-LAMBDAS NODELIST ETN)
  (LET ((OLDFON *FON*))
    (WALK (LAMBDA (A)
            (COND ((JUMP-LAMBDA-NODE? A)
                   (LET ((NEWFON *FON*))
                     (SET *FON*
			  (MAX NEWFON
			       (BIND ((*FON* OLDFON))
				 (TARGETIZE-JUMP-LAMBDA-CODE A ETN)
				 *FON*)))))))
          NODELIST)))

;;; We have a choice of separating out TN's for the argument positions
;;; as delivered as opposed to the places where the variable values
;;; are kept within the function body.  This decision is actually made by
;;; the LAMBDA code in TARGETIZE, and the code below should be able to
;;; abide by whatever's decided.  In reality (this may change), we choose to
;;; keep the two the same, mostly for the sake of LABELS but also for the
;;; sake of REP-ANNOTATE, which would prefer to know that there's an extra
;;; value kicking around whose representation should be decided.

(DEFINE (TARGETIZE-JUMP-LAMBDA-CODE NODE ETN)
  (LET ((FM (NODE-FORM NODE)))
    (WALK (LAMBDA (V ARGTN)
            (RECORD-TN-USE ARGTN)
            (COND ((AND V (NULL? (VARIABLE-TN V)))
                   (GEN-VARIABLE-TN V)
                   (CREATE-VARIABLE-TN V)
                   (PREFTN ARGTN (VARIABLE-TN V)))))
          (LAMBDA-VARS FM)
          (LAMBDA-ARGTNS FM))
    (TARGETIZE (LAMBDA-BODY FM) NIL ETN)))

(DEFINE (TARGETIZE-CALL-LET NODE FM ETN)
  (IGNORE NODE)
  (LET* ((FNNODE (CALL-FUNCTION FM))
         (FN (NODE-FORM FNNODE)))
    ;; Process the actual parameters.
    (WALK (LAMBDA (A V)
            (TARGETIZE A (GEN-VARIABLE-TN V) ETN))
          (CALL-ARGS FM)
          (LAMBDA-VARS FN))
    (WALK TNUSE (CALL-ARGS FM))
    (TARGETIZE (LAMBDA-BODY FN) NIL ETN)))

(DEFINE (TARGETIZE-CALL-JUMP NODE FM ETN)
  (IGNORE NODE)
  (LET* ((VAR (NODE-FORM (CALL-FUNCTION FM)))
         (FNNODE (VARIABLE-KNOWN-FUNCTION VAR))
         (FN (NODE-FORM FNNODE)))
    (WALK (LAMBDA (A) (TARGETIZE A (GENTN 'ANY NIL A) ETN))
          (CALL-ARGS FM))
    (WALK (LAMBDA (A ARGTN)
            (TNUSE A)
            (COND (ARGTN
                   (RECORD-TN-USE ARGTN)
                   (PREFTN (NODE-WANTTN A) ARGTN))))
          (CALL-ARGS FM)
          (LAMBDA-ARGTNS FN))))

(DEFINE (TARGETIZE-CALL-EXIT NODE FM ETN)
  (IGNORE NODE)
  (TARGETIZE (CAR (CALL-ARGS FM)) NIL ETN))

(DEFINE (TARGETIZE-CALL-PROC NODE FM ETN)
  (BIND ((*STACKNUM* *STACKNUM*))
    ;; Get double alignment
    (IF (ODD? *STACKNUM*) (INCR *STACKNUM*))
    (INCR *STACKNUM*)           ;Slot for return address
    (WALK (LAMBDA (A)
            (TARGETIZE A (GENTN 'STACK *STACKNUM* A) ETN)       ;Weird push hack
            (INCR *STACKNUM*)
            (SET (NODE-STACKNUM A) *STACKNUM*))        ; Retroactive patch?
          (CALL-ARGS FM))
    (TARGETIZE (CALL-FUNCTION FM)
               (GENTN 'POINTER-REG *FUN* (CALL-FUNCTION FM))
               ETN)
    (WALK TNUSE (CALL-ARGS FM))
    (TARGETIZE-POINT-OF-CALL (CALL-FUNCTION FM) ETN)
    ;; This is the point of the call.
    (PUSH *CALL-LON-FONS* (CONS *LON* *FON*))
    (INCR *LON*)
    (INCR *FON*)
    (TNYIELD NODE (GENTN 'POINTER-REG *VAL* NODE))))

;;; Tail-recursive call to general procedure.  The trick here is to create
;;; TN's for the actual places the arguments will finally end up, and
;;; create unrestricted WANTTN's for the argument nodes; these get preferenced
;;; to each other.  This accomplishes the parallel assignment without getting
;;; lifetime screwups in cases like (DEFINE (F A B) (G B A)).

(DEFINE (TARGETIZE-CALL-TAIL NODE FM ETN)
  (IGNORE NODE)
  (WALK (LAMBDA (A)
          (TARGETIZE A (GENTN 'ANY NIL A) ETN))
        (CALL-ARGS FM))
  (TARGETIZE (CALL-FUNCTION FM)
             (GENTN 'POINTER-REG *FUN* (CALL-FUNCTION FM))
             ETN)
  (DO ((A (CALL-ARGS FM) (CDR A))
       (J 0 (FX+ J 1)))
      ((NULL? A)
       (WALK (LAMBDA (A) (RECORD-TN-USE (GETSPARETN A 'ARG)))
             (CALL-ARGS FM))
       (TARGETIZE-POINT-OF-CALL (CALL-FUNCTION FM) ETN))
    (LET ((ARGTN (GENTN 'POINTER-MEM J (CAR A))))
      (SETSPARETN (CAR A) 'ARG ARGTN)
      (TNUSE (CAR A))
      (RECORD-TN-USE ARGTN)
      (PREFTN ARGTN (NODE-WANTTN (CAR A))))))

;;; Handle EZCLOSE calls.

(DEFINE (TARGETIZE-POINT-OF-CALL NODE ETN)
  (LET ((FM (NODE-FORM NODE)))
    (COND ((AND (EQ? (STYPE FM) 'VARIABLE)
                (VARIABLE-KNOWN-FUNCTION FM)
                ;; (EQ? (LAMBDA-STRATEGY (NODE-FORM (VARIABLE-KNOWN-FUNCTION FM)))
                ;;      'EZCLOSE)
                )
           (LET ((DESTENV (NODE-CONSENV (VARIABLE-KNOWN-FUNCTION FM))))
             (COND (DESTENV
                    (IF (AND (NEQ? DESTENV (NODE-CONSENV NODE))
                             (NOT (CONSENV-LOOKUP DESTENV (NODE-CONSENV NODE))))
                        (BUGLET ((*NODE* NODE))
                                "can't find env for called EZCLOSE procedure"
                                "will dawdle down to dubious disaster"))
                    (TARGETIZE-CONSENV-REF NODE (NODE-CONSENV NODE) ETN
                                           'ENVREF)))))
          (ELSE
           (TNUSE NODE)))))

(DEFINE (TARGETIZE-CALL-ENTITY NODE FM ETN)
  (LET ((ARG1 (CAR  (CALL-ARGS FM)))
        (ARG2 (CADR (CALL-ARGS FM))))
    (SET (NODE-LEVEL ARG1) (NODE-LEVEL NODE))  ; INCREDIBLY GROSS BEYOND WORDS!
    (TARGETIZE ARG1 NIL ETN)
    (TARGETIZE ARG2 NIL ETN)
    ;; (TNYIELD NODE NIL)
    ))

;;; The following needs a LOT of work.
;;; Need to account for many weird things:
;;; (a) One of the arguments is returned as the value of the call.

(DEFINE (TARGETIZE-CALL-PRIMOP NODE FM ETN)
  (LET* ((FN (NODE-FORM (CALL-FUNCTION FM)))
         (RR (CGET FN 'RESULT-RESTRICTION))
         (TARGET (COND ((EQ? (NODE-ISREP NODE) 'NONE) NIL)
                       (RR (GENTN (CAR RR) (CADR RR) NODE))
                       (ELSE (GENTN 'ANY NIL NODE)))))
    (SET (NODE-ISTN NODE) TARGET)      ; why? (can't hurt)
    (HACK-CALL-ARG-ALIASES NODE)
    (DO ((A (CALL-ARGS FM) (CDR A))
         (R (CGET FN 'ARGUMENT-RESTRICTIONS) (IF R (CDR R) NIL)))
        ((NULL? A)
         (WALK (LAMBDA (A)
                 (LET ((ARGTN (GETSPARETN A 'ARG)))
                   (COND (ARGTN
                          (TNUSE A)
                          (RECORD-TN-USE ARGTN)
                          (PREFTN ARGTN (NODE-WANTTN A))))))
               (CALL-ARGS FM))
         (LET ((FOO (CGET FN 'TN-SCRIPT)))
           (COND (FOO
                  (WALK (LAMBDA (Z)
                          (PROCESS-PRIMOP-TNUSE-ITEM NODE FM TARGET Z))
                        FOO))
                 (ELSE
                  (WALK (LAMBDA (A)
                          (LET ((ARGTN (GETSPARETN A 'ARG)))
                            (COND (ARGTN (RECORD-TN-USE ARGTN))
                                  (ELSE (TNUSE A)))))
                        (CALL-ARGS FM)))))
         (TNYIELD NODE TARGET))
      ----
      (COND ((EQ? (NODE-WANTREP (CAR A)) 'NONE)
             (TARGETIZE (CAR A) NIL ETN))
            ((NULL? (CAR R))
             (TARGETIZE (CAR A) (GENTN 'ANY NIL (CAR A)) ETN))
            (ELSE
             (IF (NODE-ALIASP (CAR A)) (SET (NODE-ALIASP (CAR A)) (CAAR R)))
             (LET ((RTN (GENTN (IF (EQ? (CAAR R) 'INDIRECTABLE) 'ANY (CAAR R))
                               (CADAR R)
                               (CAR A))))
               (TNREP RTN (NODE-WANTREP (CAR A)))
               (COND ((NULL? (CDR A))
                      (TARGETIZE (CAR A) RTN ETN))      ; Hack
                     (ELSE
                      (SETSPARETN (CAR A) 'ARG RTN)
                      (TARGETIZE (CAR A) (GENTN 'ANY NIL (CAR A)) ETN))))))
      ---)))

;;; Hmm.  Returns the TN that got used, for purpose of PREF crock.

(DEFINE (PROCESS-PRIMOP-TNUSE-ITEM NODE FM TARGET Z)
  (XCASE (CAR Z)
    ((RESULT) (RECORD-TN-USE TARGET) TARGET)
    ((ARG)
     (LET* ((A (NTH (CALL-ARGS FM) (FX- (CADR Z) 1)))
            (ARGTN (GETSPARETN A 'ARG)))
       (COND (ARGTN
              (RECORD-TN-USE ARGTN)
              ARGTN)
             (ELSE (TNUSE A)
                   (NODE-WANTTN A)))))
    ((SPARE)
     (LET ((SPARETN
            (OR (GETSPARETN NODE (CADR Z))
                (SETSPARETN NODE (CADR Z) (GENTN (CADDR Z) (CADDDR Z) NODE)))))
       (RECORD-TN-USE SPARETN)
       SPARETN))
    ((PREF)
     (LET ((TN1 (PROCESS-PRIMOP-TNUSE-ITEM NODE FM TARGET (CADR Z)))
           (TN2 (PROCESS-PRIMOP-TNUSE-ITEM NODE FM TARGET (CADDR Z))))
       (IF (AND TN1 TN2) (PREFTN TN1 TN2))
       TN1))
    ((REP)
     (LET ((SPARETN (GETSPARETN NODE (CADR Z))))
       (TNREP SPARETN (CADDR Z))
       (RECORD-TN-USE SPARETN)
       SPARETN))
    ))

;;; Set the ALIASP slot for args in a PRIMOP or TAIL strategy call.
;;; Two main constraints guide this:
;;; - Aliasing must be acceptable from a semantic point of view.  (If we do not
;;;   guarantee left-to-right order of evaluation then this is much easier
;;;   (I think).)
;;; - Aliases in terms of registers must be subject to the constraint that
;;;   computation of subsequent arguments won't have to clobber those registers.
;;; (a) Constants can always be aliased.
;;; (b) Static vars can be aliased if no sparetn is involved, and they pass
;;;     subsequent arguments.
;;; (c) Similarly for local variables; closure refs need sparetns, others
;;;     don't.
;;; (d) Any arg can be aliased if it passes all subsequent args and they, in
;;;     turn, are all variables or constants.
;;; (e) The last arg can always be aliased.

(DEFINE (HACK-CALL-ARG-ALIASES NODE)
  (LET ((FM (NODE-FORM NODE)))
    (WALKCDR (LAMBDA (A)
               (IF (OR (CONSTANT-NODE? (CAR A))
                       (AND (NULL? (CDR A))
                            (NOT (CGET (NODE-FORM (CALL-FUNCTION FM))
                                       'ARGUMENT-RESTRICTIONS))
                            (NOT (CGET (NODE-FORM (CALL-FUNCTION FM))
                                       'TN-SCRIPT)))
;;; Code temporarily disabled until fixed.
;;; Consider (SET-CAR X (CDR Y)).
;                  (NULL? (CDR A))
;                  (AND (EVERY (LAMBDA (AA) (PASSABLE (CAR A) AA))
;                              (CDR A))
;                       (OR (EVERY (LAMBDA (AA)
;                                    (OR (CONSTANT-NODE? AA)
;                                        (STATIC-NODE? AA)
;                                        (VARIABLE-NODE? AA)))
;                                  (CDR A))
;                           ;; Foo.  Strategy ought to be stored someplace.
;                           (AND (VARIABLE-NODE? (CAR A)) ;WRONG WRONG
;                                (NOT (CONSENV-REF? (NODE-FORM (CAR A))))
;                                (OR (NOT (STATIC? (NODE-FORM (CAR A))))
;                                    *TARGET-INDIRECTION*))))
                       )
                   (MAYBE-SET-ALIASP (CAR A) T)))
             (CALL-ARGS FM))))

;;; Preference actual and formal parameters together.

(DEFINE (PREF-CALL-ARGS VARIABLES ARGS)
  (WALK (LAMBDA (V A)
          (IF V (PREFTN (VARIABLE-TN V) (NODE-WANTTN A))))
        VARIABLES
        ARGS))

;;; Set up a variable's TN.  Used by LAMBDA, LABELS, CATCH, etc.

(DEFINE (GEN-VARIABLE-TN VAR)
  (COND ((AND (NOT (NULL? VAR))
              (NEQ? (VARIABLE-REP VAR) 'NONE))
         (LET ((NEWTN (GENTN 'ANY NIL (VARIABLE-BINDER VAR))))
           (SET (VARIABLE-TN VAR) NEWTN)
           (SET (TN-VAR NEWTN) VAR)
           (TNREP NEWTN (VARIABLE-REP VAR))
           NEWTN))
        (ELSE NIL)))

;;; Record creation of TN's for a LAMBDA's bound lexical variables.

(DEFINE (CREATE-VARIABLE-TN VAR)
  (COND ((AND VAR (VARIABLE-TN VAR))
         (RECORD-TN-USE (VARIABLE-TN VAR)))))
