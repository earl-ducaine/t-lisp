(HERALD ANNOTATE
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Annotation phases

(DEFINE-INTEGRABLE (JUMP-LAMBDA-NODE? NODE)
  (AND (LAMBDA-NODE? NODE) (EQ? (LAMBDA-STRATEGY (NODE-FORM NODE)) 'JUMP)))

(DEFINE (CALL-KNOWN-FUNCTION FM)        ; does anyone use this?
  (LET ((FNNODE (CALL-FUNCTION FM)))
    (IF (OR (NOT (VARIABLE-NODE? FNNODE))
            (NOT (VARIABLE-KNOWN-FUNCTION (NODE-FORM FNNODE))))
        (BUG "illegal attempt to extract a KNOWN-FUNCTION from a CALL"
             "will return NIL but that won't win at all"))
    (VARIABLE-KNOWN-FUNCTION (NODE-FORM FNNODE))))

;;;; Flow annotation

;;; Fill in the RETURNERS slot of every node.

;;; This really wants to be an analysis phase (maybe as part of what
;;; LEVEL-PROPAGATE does), but I don't want to figure out right now just
;;; how to integrate it with the optimizer, when the reanalysis needs to
;;; happen, etc.  Also, this wants to use strategy information (currently
;;; set up by BIND-ANNOTATE).  The data dependencies are somewhat obscure.

(DEFINE (RETURNER NODE)
  (COND ((EMPTY (NODE-LEVEL NODE)))
        ((MEMQ NODE (NODE-RETURNERS (NODE-LEVEL NODE)))
         (BUGLET ((*NODE* NODE))
                 "redundant notice of node yield"
                 "will ignore redudnant notice, but something's strange here"))
        (ELSE (PUSH (NODE-RETURNERS (NODE-LEVEL NODE)) NODE))))

(DEFINE (FLOW-ANNOTATE NODE)
  (SETF (NODE-RETURNERS NODE) '())
  (NODE-DISPATCH FLOW-ANNOTATE NODE))

(DEFDISPATCH FLOW-ANNOTATE CONSTANT (NODE FM)
  (RETURNER NODE))

(DEFDISPATCH FLOW-ANNOTATE STATIC (NODE FM)
  (RETURNER NODE))

(DEFDISPATCH FLOW-ANNOTATE VARIABLE (NODE FM)
  (RETURNER NODE))

(DEFDISPATCH FLOW-ANNOTATE SETQ (NODE FM)
  (FLOW-ANNOTATE (SETQ-BODY FM))
  (RETURNER NODE))

(DEFDISPATCH FLOW-ANNOTATE CATCH (NODE FM)
  (FLOW-ANNOTATE (CATCH-BODY FM))
  (IF (EQ? (CATCH-STRATEGY FM) 'PROC)
      (RETURNER NODE)))

(DEFDISPATCH FLOW-ANNOTATE LAMBDA (NODE FM)
  (FLOW-ANNOTATE (LAMBDA-BODY FM))
  (RETURNER NODE))

(DEFDISPATCH FLOW-ANNOTATE LABELS (NODE FM)
  (WALK (LAMBDA (A) (FLOW-ANNOTATE A)) (LABELS-VALS FM))
  (FLOW-ANNOTATE (LABELS-BODY FM)))

(DEFDISPATCH FLOW-ANNOTATE BLOCK (NODE FM)
  (WALK (LAMBDA (A) (FLOW-ANNOTATE A)) (BLOCK-ARGS FM)))

(DEFDISPATCH FLOW-ANNOTATE IF (NODE FM)
  (FLOW-ANNOTATE (IF-PRED FM))
  (FLOW-ANNOTATE (IF-CON FM))
  (FLOW-ANNOTATE (IF-ALT FM)))

(DEFDISPATCH FLOW-ANNOTATE CASE (NODE FM)
  (FLOW-ANNOTATE (CASE-KEY FM))
  (FLOW-ANNOTATE (CASE-ELSE FM))
  (WALK (LAMBDA (C) (FLOW-ANNOTATE (CDR C))) (CASE-CLAUSES FM)))

(DEFDISPATCH FLOW-ANNOTATE CALL (NODE FM)
  (FLOW-ANNOTATE (CALL-FUNCTION FM))
  (WALK (LAMBDA (A) (FLOW-ANNOTATE A)) (CALL-ARGS FM))
  ;; Should also handle primop continuations someday.
  (CASE (CALL-STRATEGY FM)
    ((PROC ENTITY PRIMOP)
     (RETURNER NODE))))

;;;; Representation annotation

;;; Fill in the WANTREP and ISREP of each node.
;;;
;;; To do:
;;; - Divide this into WANTREP-ANNOTATE and ISREP-ANNOTATE, as Steele
;;;   et al have done.
;;; - This could be a lot simpler if we made use of the new RETURNERS list.

(DEFINE (REP-ANNOTATE NODE WANT)
  (SETF (NODE-WANTREP NODE) (IF (ATOM? WANT) WANT 'NONE))
  (SETF (NODE-ISREP NODE)
        (NODE-DISPATCH REP-ANNOTATE NODE WANT))
  (COND ((AND (EQ? NODE (NODE-LEVEL NODE))
              (OR (CDR (NODE-RETURNERS NODE))
                  (NEQ? NODE (CAR (NODE-RETURNERS NODE)))))  ; (CAR '()) => ()
         (SETF (NODE-ISREP NODE)
               (COND ((EQ? WANT 'NONE) 'NONE)
                     (ELSE
                      (DO ((REP NIL (MERGE-REPS REP (NODE-ISREP (CAR R))))
                           (R (NODE-RETURNERS NODE) (CDR R)))
                          ((NULL? R) (OR REP 'NONE))))))
         ;; The following is redundant but reassuring.
         (WALK (LAMBDA (R)
                 (IF (NEQ? R NODE)
                     (SETF (NODE-WANTREP R) (NODE-ISREP NODE))))
               (NODE-RETURNERS NODE))))
  (IF (EQ? (NODE-ISREP NODE) 'NONE)
      (SETF (NODE-WANTREP NODE) 'NONE)) ; why?
  (NODE-ISREP NODE)) 

(DEFDISPATCH REP-ANNOTATE CONSTANT (NODE FM WANT)
  (LET ((VAL (CONSTANT-VALUE FM)))
    (CASE WANT
      ((NONE POINTER))
      ((MFIX MFIX*4 SWFIX DWFIX HALFPESO BYTE)  ; choke
       (IF (NOT (TARGET:FIXNUM? VAL))
           (REP-ANNOTATE-CONSTANT-BUG NODE "fixnum" 0)))
      ((DWFLO)
       (IF (NOT (TARGET:FLONUM? VAL))
           (REP-ANNOTATE-CONSTANT-BUG NODE "flonum" 0.0)))
      ((CHAR)
       (IF (NOT (CHAR? VAL))
           (REP-ANNOTATE-CONSTANT-BUG NODE "character" *NULL-CHARACTER*)))))
  WANT)

(DEFINE (REP-ANNOTATE-CONSTANT-BUG NODE FOO NEWVAL)
  (BUG "a ~A was expected, but got ~S instead (parent = ~S)"
       "will use the value ~S instead"
       FOO (SEXPRFY NODE) (SEXPRFY (NODE-PARENT NODE)) NEWVAL)
  (SETF (CONSTANT-VALUE (NODE-FORM NODE)) NEWVAL))

(DEFDISPATCH REP-ANNOTATE STATIC (NODE FM WANT)
  'POINTER)

(DEFDISPATCH REP-ANNOTATE VARIABLE (NODE FM WANT)
  ;; Will figure out lexical var reps at binding point.
  (COND ((AND (VARIABLE-KNOWN-FUNCTION FM)
              (MEMQ (LAMBDA-STRATEGY (NODE-FORM (VARIABLE-KNOWN-FUNCTION FM)))
                    '(JUMP EZCLOSE)))
         'NONE)
        (ELSE 'POINTER)))

(DEFDISPATCH REP-ANNOTATE SETQ (NODE FM WANT)
  (REP-ANNOTATE (SETQ-BODY FM)
                (COND ((STATIC? (SETQ-VAR FM))
                       'POINTER)
                      ((EQ? WANT 'NONE) 'POINTER)
                      (ELSE WANT))))

(DEFDISPATCH REP-ANNOTATE LAMBDA (NODE FM WANT)
  (COND ((NEQ? (LAMBDA-STRATEGY FM) 'JUMP)
         (REP-ANNOTATE (LAMBDA-BODY FM) 'POINTER)
         (WALK REP-ANNOTATE-VAR (LAMBDA-VARS FM))))
  (XCASE (LAMBDA-STRATEGY FM)
    ((NONE JUMP EZCLOSE) 'NONE)
    ((PROC TPROC HANDLER METHOD) 'POINTER)))

(DEFDISPATCH REP-ANNOTATE CATCH (NODE FM WANT)
  (XCASE (CATCH-STRATEGY FM)
    ;; Really want to go through the same merge-reps business with
    ;; EXIT frobs too...
    ((EXIT)
     (REP-ANNOTATE (CATCH-BODY FM) WANT) WANT)
    ((PROC TAIL)
     (REP-ANNOTATE (CATCH-BODY FM) 'POINTER)
     (REP-ANNOTATE-VAR (CATCH-VAR FM))
     'POINTER)))

(DEFDISPATCH REP-ANNOTATE LABELS (NODE FM WANT)
  (REP-ANNOTATE (LABELS-BODY FM) WANT)
  (WALK (LAMBDA (X) (REP-ANNOTATE X 'POINTER)) (LABELS-VALS FM))
  (REP-ANNOTATE-JUMP-LAMBDAS (LABELS-VALS FM) WANT)
  (WALK REP-ANNOTATE-VAR (LABELS-VARS FM))
  'LOSE)

(DEFDISPATCH REP-ANNOTATE BLOCK (NODE FM WANT)
  (DO ((A (BLOCK-ARGS FM) (CDR A)))
      ((NULL? (CDR A))
       (REP-ANNOTATE (CAR A) WANT))
    ----
    (REP-ANNOTATE (CAR A) 'NONE)
    ---))

(DEFDISPATCH REP-ANNOTATE IF (NODE FM WANT)
  (REP-ANNOTATE (IF-PRED FM)
                (COND ((PRIMOP-PREDICATE-CALL? (IF-PRED FM)) 'NONE)
                      (ELSE 'POINTER)))
  (REP-ANNOTATE (IF-CON FM) WANT)
  (REP-ANNOTATE (IF-ALT FM) WANT)
  'LOSE)

(DEFDISPATCH REP-ANNOTATE CASE (NODE FM WANT)
  (REP-ANNOTATE (CASE-KEY FM)
                (CASE (CASE-TYPE FM)
                  ((FIXNUM) 'SWFIX)             ; should be MFIX if dense.
                  ((CHARACTER) 'CHAR)
                  (ELSE 'POINTER)))
  (REP-ANNOTATE (CASE-ELSE FM) WANT)
  (WALK (LAMBDA (C) (REP-ANNOTATE (CDR C) WANT)) (CASE-CLAUSES FM))
  'LOSE)

(DEFDISPATCH REP-ANNOTATE CALL (NODE FM WANT)
  (BLOCK0 (CASE (CALL-STRATEGY FM)
            ((LET)      (REP-ANNOTATE-CALL-LET NODE WANT))
            ((JUMP)     (REP-ANNOTATE-CALL-JUMP NODE WANT))
            ((EXIT)     (REP-ANNOTATE-CALL-EXIT NODE WANT))
            ((PROC TAIL ENTITY) (REP-ANNOTATE-CALL-PROC NODE WANT))
            ((PRIMOP)    (REP-ANNOTATE-CALL-PRIMOP NODE WANT)))
          (REP-ANNOTATE-JUMP-LAMBDAS (CALL-ARGS FM) WANT)))

;; @@@ This loses on (LET ((X Y)) (IF Z (* X 3) 'BAZ)) because X will be forced
;; to be SWFIX, causing possible type-check error even if Z is NIL.

(DEFINE (REP-ANNOTATE-CALL-LET NODE WANT)
  (LET* ((FM (NODE-FORM NODE))
         (FN (NODE-FORM (CALL-FUNCTION FM))))
    (BLOCK0 (REP-ANNOTATE (LAMBDA-BODY FN) WANT)
            (WALK (LAMBDA (V A)
                    (REP-ANNOTATE A (REP-ANNOTATE-VAR V)))
                  (LAMBDA-VARS FN)
                  (CALL-ARGS FM)))))

(DEFINE (REP-ANNOTATE-CALL-JUMP NODE WANT)
  (IGNORE WANT)
  (LET ((FM (NODE-FORM NODE)))
    (WALK (LAMBDA (A) (REP-ANNOTATE A 'POINTER))        ;@@@ lose
          (CALL-ARGS FM))
    'NONE))

;;; This will change.
(DEFINE (REP-ANNOTATE-CALL-EXIT NODE WANT)
  (IGNORE WANT)
  (LET ((FM (NODE-FORM NODE)))
    (REP-ANNOTATE (CAR (CALL-ARGS FM))
                  (NODE-WANTREP (VARIABLE-BINDER
                                 (NODE-FORM (CALL-FUNCTION FM)))))
    'NONE))

(DEFINE (REP-ANNOTATE-CALL-PROC NODE WANT)
  (IGNORE WANT)
  (LET ((FM (NODE-FORM NODE)))
    (REP-ANNOTATE (CALL-FUNCTION FM) 'POINTER)
    (WALK (LAMBDA (A) (REP-ANNOTATE A 'POINTER))
          (CALL-ARGS FM))
    (IF (EQ? (CALL-STRATEGY FM) 'TAIL) 'NONE 'POINTER)))

(DEFINE (REP-ANNOTATE-JUMP-LAMBDAS NODELIST WANT)
  (DO ((A NODELIST (CDR A)))
      ((NULL? A) 'LOSE)
    ----
    (COND ((JUMP-LAMBDA-NODE? (CAR A))
           (LET ((FM (NODE-FORM (CAR A))))
             (REP-ANNOTATE (LAMBDA-BODY FM) WANT)
             (WALK REP-ANNOTATE-VAR (LAMBDA-VARS FM)))))
    ---))

(DEFINE (REP-ANNOTATE-CALL-PRIMOP NODE WANT)
  (IGNORE WANT)
  (LET ((FM (NODE-FORM NODE)))
    (DO ((A (CALL-ARGS FM) (CDR A))
         (W (OR (CGET (NODE-FORM (CALL-FUNCTION FM)) 'WANTREP)
                'POINTER)
            (COND ((ATOM? W) W) (ELSE (CDR W)))))
        ((NULL? A))
      ----
      (COND ((ATOM? W) (REP-ANNOTATE (CAR A) W))
            ((ATOM? (CAR W)) (REP-ANNOTATE (CAR A) (CAR W)))
;           ((EQ? (CAAR W) 'CONT)
;            (REP-ANNOTATE-PRIMOP-CONTINUATION (CAR A) (CDAR W)))
            (ELSE
             (BUGLET ((*NODE* NODE))
                     "peculiar PRIMOP rep info"
                     "treat it as if it were POINTER")
             (REP-ANNOTATE (CAR A) 'POINTER)))
      ---)
    (OR (CGET (NODE-FORM (CALL-FUNCTION FM)) 'ISREP) 'POINTER)))

(COMMENT
(DEFINE (REP-ANNOTATE-PRIMOP-CONTINUATION NODE WANT)
  (LET ((FM (NODE-FORM NODE)))
    (IF (NOT (EQ? (STYPE FM) 'LAMBDA))
        (BUGLET ((*NODE* NODE))
                "argument is supposed to be a LAMBDA"
                "burn and die horribly"))
    (SETF (NODE-ISREP NODE) 'NONE)
    (BUG)                               ;Not yet written!!
    ))
)

;;; Returns the preferred representation of the variable.
;;; @@@ Still buggy.

(DEFINE (REP-ANNOTATE-VAR V)
  (COND ((NULL? V) 'NONE)
        (ELSE
         (SETF (VARIABLE-REP V)
               (COND ((VARIABLE-KNOWN-FUNCTION V)
                      (COND ((MEMQ (LAMBDA-STRATEGY
                                    (NODE-FORM (VARIABLE-KNOWN-FUNCTION V)))
                                   '(JUMP EZCLOSE))
                             'NONE)
                            (ELSE 'POINTER)))
                     ((LABELS-NODE? (VARIABLE-BINDER V))
                      'POINTER)       ;Patch!
                     ((AND (LAMBDA-NODE? (VARIABLE-BINDER V))
                           (NOT (MEMQ (LAMBDA-STRATEGY
                                       (NODE-FORM (VARIABLE-BINDER V)))
                                      '(NONE JUMP))))
                      'POINTER)
                     (ELSE (REP-ANNOTATE-LEXVAR V)))))))

;;; This should take into account all the nodes which deliver values INTO
;;; the variable at the point where it's bound.
;;; --- Someday this should be able to handle LABELS variables too!!!

(DEFINE (REP-ANNOTATE-LEXVAR VAR)
  (DO ((R (VARIABLE-READ-REFS VAR) (CDR R))
       (FOOW NIL (MERGE-REPS W (NODE-WANTREP (CAR R))))
       (BODY-LEVEL
        (NODE-LEVEL (LAMBDA-BODY (NODE-FORM (VARIABLE-BINDER VAR)))))
       (W NIL
          ;; This is an incredible hack to make it so that
          ;;  references to a variable which are used as
          ;;  the return value from the lambda form that
          ;;  binds the variable do not count in making 
          ;;  the representation decision unless there 
          ;;  are no other references.  This tends to
          ;;  allow a numeric variable to have a numeric
          ;;  representation within a loop even though
          ;;  on return from the loop it must be consed.
          (COND ((EQ? (NODE-LEVEL (CAR R)) BODY-LEVEL)
                 W)
                (ELSE (MERGE-REPS W (NODE-WANTREP (CAR R)))))))
      ((NULL? R)
       (DO ((Z (VARIABLE-WRITE-REFS VAR) (CDR Z))
            (WW (OR W FOOW)
                (MERGE-REPS WW (NODE-WANTREP (CAR Z)))))
           ((NULL? Z)
            (LET ((WWW (OR WW 'NONE)))
              (WALK (LAMBDA (N) (SETF (NODE-ISREP N) WWW))
                    (VARIABLE-READ-REFS VAR))
              (WALK (LAMBDA (N)
                      (SETF (NODE-ISREP N) WWW)
                      (SETF (NODE-WANTREP (SETQ-BODY (NODE-FORM N)))
                            WWW))
                    (VARIABLE-WRITE-REFS VAR))
              WWW))))))

(DEFINE (MERGE-REPS REP1 REP2)
  (COND ((NULL? REP1) REP2)
        ((OR (EQ? REP1 REP2) (EQ? REP2 'NONE)) REP1)
        (ELSE 'POINTER)))
