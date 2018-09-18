(HERALD SUBST
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University


;;;; SUBSTITUTE

(STAT-COUNTER *META-CALL-LAMBDA-REST-FLUSH-COUNT*
              "&REST parameter of a LAMBDA in a CALL is made &REQUIRED")
(STAT-COUNTER *META-CALL-LAMBDA-PARAMETER-FLUSH-COUNT*
              "unreferenced argument with no side effects is flushed")
(STAT-COUNTER *META-CALL-LAMBDA-BETA-CONVERSION-COUNT*
              "((LAMBDA () FOO)) becomes FOO")

(DEFINE (META-CALL-LAMBDA NODE)
  (LET ((FM (NODE-FORM NODE)))
    (LET ((FN (NODE-FORM (CALL-FUNCTION FM))))
      (MEVAL (CALL-FUNCTION FM) (LAMBDA-BODY FN))
      (REANALYZE NODE)                  ;for benefit of SUBST-CANDIDATE
      ;; Deal with rest parameter and wrong number of arguments.
      (COND ((OR (NOT (EMPTY (LAMBDA-RESTVAR FN)))
                 (NOT (FX= (LENGTH (CALL-ARGS FM))
                         (LENGTH (LAMBDA-VARS FN)))))
             (META-BEFORE-TRACE NODE)
             (META-CALL-LAMBDA-FLUSH-REST NODE)
             (META-AFTER-TRACE NODE 'META-CALL-LAMBDA-FLUSH-REST)))
      ;; Mark arguments as not substituted for.
      (WALK (LAMBDA (A) (SETF (NODE-SUBSTP A) NIL))
            (CALL-ARGS FM))
      ;; Attempt to substitute arguments for matching parameters in the
      ;;  lambda body.
      (DO ((V (LAMBDA-VARS FN) (CDR V))
           (A (CALL-ARGS FM) (CDR A))
           (BODY (LAMBDA-BODY FN)
                 (COND ((AND (CAR V)
                             (SUBST-CANDIDATE (CAR A) (CAR V))
                             (D0 ((Z (CDR A) (CDR Z)))
                                 ((NULL? Z) T)
                               ----
                               (OR (PASSABLE (CAR A) (CAR Z))
                                   (RETURN NIL))
                               ---))
                        (META-SUBSTITUTE (CAR A) (CAR V) BODY))
                       (ELSE BODY))))
          ((NULL? V)
           (SETF (LAMBDA-BODY FN) BODY)
           (SETF (NODE-PARENT BODY) (CALL-FUNCTION FM))
           (REPROPAGATE BODY)
           (MEVAL (CALL-FUNCTION FM) (LAMBDA-BODY FN))
           ;; Now attempt to flush unreferenced parameters.
           (FLUSH-UNREFERENCED-PARAMETERS (CALL-FUNCTION FM))
           ;; Flush unreferenced arguments if possible.
           (DO ((V (LAMBDA-VARS FN) (CDR V))
                (X '())
                (A (CALL-ARGS FM) (CDR A)))
               ((NULL? V))
             ----
             ;; Can flush if unreferenced and either has no side
             ;; effects or does have side effects but was
             ;; substituted for the (unique) reference.
             (COND ((AND (NULL? (CAR V))
                         (OR (AND (EFFECTLESS-EXCEPT-CONS
                                   (NODE-EFFECTS (CAR A)))
                                  (NULL? (NODE-SETQS (CAR A))))
                             (NODE-SUBSTP (CAR A))))
                    (ERASE-ALL-NODES (CAR A))
                    (SETF (CALL-ARGS FM) (DELQ (CAR A) (CALL-ARGS FM)))
                    (COND ((NULL? X)
                           (SETF (LAMBDA-VARS FN) (CDR V)))
                          (ELSE (RPLACD X (CDR V))))
                    (INCR *META-CALL-LAMBDA-PARAMETER-FLUSH-COUNT*))
                   (ELSE (SETQ X V)))
             ---)
           ;; If no arguments left, flush the CALL-LAMBDA and return
           ;;  just the body.
           (COND ((NULL? (LAMBDA-VARS FN))
                  (LET ((BODY (LAMBDA-BODY FN)))
                    (META-BEFORE-TRACE NODE)
                    (SETF (NODE-PARENT BODY) (NODE-PARENT NODE))
                    (ERASE-NODE (CALL-FUNCTION FM))
                    (ERASE-NODE NODE)
                    (INCR *META-CALL-LAMBDA-BETA-CONVERSION-COUNT*)
                    (META-AFTER-TRACE BODY 'META-CALL-LAMBDA)
                    BODY))
                 (ELSE (REANALYZE (CALL-FUNCTION FM))
                       (REANALYZE NODE))))))))

;;; Pass over all bindings and arguments.  Check for wrong number of arguments.
;;; Eliminate "rest" parameter by turning it into a normal
;;; parameter and calculating the appropriate argument to match.

(DEFINE (META-CALL-LAMBDA-FLUSH-REST NODE)
  (LET* ((FM (NODE-FORM NODE))
         (FN (NODE-FORM (CALL-FUNCTION FM))))
    (DO ((V (LAMBDA-VARS FN) (CDR V))
         (A (CALL-ARGS FM) (CDR A))
         (Z '()))
        ((NULL? V)
         ;; Do processing for "rest" var if we have one.  Note that introduc-
         ;;  ing the call to LIST is not necessarily the right thing, given
         ;;  that we have stack-allocated closures ("downward funargs").
         (COND ((NOT (EMPTY (LAMBDA-RESTVAR FN)))
                (LET ((NEWNODE
                       (NODIFY (CONS-A-CALL FUNCTION (ALPHA-TOPLEVEL 'LIST)
                                            ;; This A used to be (COPY-LIST A)
                                            ARGS A))))
                  (push Z NEWNODE)
                  (SETF (NODE-PARENT NEWNODE) NODE)
                  (WALK (LAMBDA (Q) (SETF (NODE-PARENT Q) NEWNODE)) A)
                  (SETF (NODE-PARENT (CALL-FUNCTION (NODE-FORM NEWNODE)))
                        NEWNODE)
                  (REANALYZE NEWNODE)
                  (SETQ A '()))
                ;; Put rest var into list of lambda vars (make it "required")
                (SETF (LAMBDA-VARS FN)
                      (APPEND! (LAMBDA-VARS FN) (LIST (LAMBDA-RESTVAR FN))))
                (SETF (LAMBDA-RESTVAR FN) *EMPTY*)
                (INCR *META-CALL-LAMBDA-REST-FLUSH-COUNT*)))
         ;; and check for arg count error before leaving
         (IF (NOT (NULL? A))
             (WARN "~S too many arguments for the LAMBDA-expression ~S"
                   "will ignore extra arguments ~S"
                   (SEXPRFY-LIST (CALL-ARGS FM))
                   (SEXPRFY (CALL-FUNCTION FM))
                   (SEXPRFY-LIST A)))
         (SETF (CALL-ARGS FM) (REVERSE! Z)))
      ----
      (COND ((NULL? A)
             (WARN "~S too few arguments for LAMBDA-expression ~S"
                   "will supply extra null arguments"
                   (SEXPRFY-LIST (CALL-ARGS FM))
                   (SEXPRFY (CALL-FUNCTION FM)))
             (LET ((NEWNODE (CREATE-CONSTANT '())))
               (PUSH Z NEWNODE)
               (SETF (NODE-PARENT NEWNODE) NODE)
               (REANALYZE NEWNODE)))
            (ELSE (PUSH Z (CAR A))))
      ---)))
;;; ... SUBST-CANDIDATE

;;; This is a predicate which is true if it is a good idea (in some funny
;;; sense) to try to substitute ARG for occurrences of VAR in BOD.
;;; Right now the heuristics are extremely conservative.  They can be made
;;; hairier.

(DEFINE (SUBST-CANDIDATE ARG VAR)
  (AND (NULL? (VARIABLE-WRITE-REFS VAR))
       (NOT (NULL? (VARIABLE-READ-REFS VAR)))
       (OR (NULL? (CDR (VARIABLE-READ-REFS VAR)))
           (MEMQ (STYPE (NODE-FORM ARG)) '(CONSTANT STATIC VARIABLE))
           (AND (LAMBDA-NODE? ARG)
                (SUBST-CANDIDATE-LAMBDA ARG VAR))
           (AND (CALL-NODE? ARG)
                (STATIC-NODE? (CALL-FUNCTION (NODE-FORM ARG)))
                (LET ((F (CGET (NODE-FORM (CALL-FUNCTION (NODE-FORM ARG)))
                               'SUBST-CANDIDATE)))
                  (AND F (FUNCALL F ARG VAR))))
           (AND (IF-NODE? ARG)
                (FX< (NODE-COMPLEXITY ARG) 32.)   ;???
                (OR (CONSTANT-NODE? (IF-CON (NODE-FORM ARG)))
                    (CONSTANT-NODE? (IF-ALT (NODE-FORM ARG))))
                (D0 ((R (VARIABLE-READ-REFS VAR) (CDR R)))
                    ((NULL? R) NIL)
                  ----
                  (LET ((PARENT (NODE-PARENT (CAR R))))
                    (IF (AND (IF-NODE? PARENT)
                             (VARIABLE-NODE? (IF-PRED (NODE-FORM PARENT)))
                             (EQ? VAR (NODE-FORM (IF-PRED (NODE-FORM PARENT)))))
                        (RETURN T)))
                  ---))
           )))

;;; Also a good idea if arg's body is simply a call to another
;;; jump lambda, but that's an awful hard thing to predicate.
;;; LEVEL-PROPAGATE should maintain VARIABLE-KNOWN-FUNCTION...

(DEFINE (SUBST-CANDIDATE-LAMBDA ARG VAR)
  ;; Known functions which can't be jumped to might want to get
  ;; substituted in, if they're not too complicated.
  (COND ((NOT (ALL-REFERENCES-ARE-CALLS? VAR))
         (NOT-SUBST-CANDIDATE ARG VAR "not all references are calls"))
        ((LET ((BOD (LAMBDA-BODY (NODE-FORM ARG))))
           (AND (CALL-NODE? BOD)
                (NULL? (CALL-ARGS (NODE-FORM BOD)))
                (VARIABLE-NODE? (CALL-FUNCTION (NODE-FORM BOD)))))
         ;; Random heuristic for hacking (LET ((F (LAMBDA () (G)))) ...)
         T)
        ((EQ? (NODE-LEVEL (LAMBDA-BODY (NODE-FORM ARG)))
              (NODE-LEVEL (NODE-PARENT (VARIABLE-BINDER VAR))))
         (NOT-SUBST-CANDIDATE ARG VAR "arg's level same as var's binder's"))
        (ELSE
         (DO ((R (VARIABLE-READ-REFS VAR) (CDR R))
              (Q1 (NODE-COMPLEXITY ARG)
                  (FX+ Q1 (NODE-COMPLEXITY (NODE-PARENT (CAR R)))))
              (Q2 (FX* (NODE-COMPLEXITY (LAMBDA-BODY (NODE-FORM ARG)))
                       (LENGTH (VARIABLE-READ-REFS VAR)))
                  (FX+ Q2
                       (DO ((A (CALL-ARGS (NODE-FORM (NODE-PARENT (CAR R))))
                               (CDR A))
                            (QQ 0 (FX+ QQ (NODE-COMPLEXITY (CAR A)))))
                           ((NULL? A) QQ)))))
             ((NULL? R)
              (COND ((FX<= Q1 Q2)
                     (NOT-SUBST-CANDIDATE ARG VAR "lambda too complicated"))
                    (ELSE T)))))))

(LSET *SUBST-CANDIDATE-FAILURE-TRACE?* NIL)

(DEFINE (NOT-SUBST-CANDIDATE ARG VAR WHY)
  (IGNORE ARG)
  (COND (*SUBST-CANDIDATE-FAILURE-TRACE?*
         (FORMAT *NOISE-OUTPUT*
                 "~&;Failed to substitute for ~S because: ~A~%"
                 (VARIABLE-IDENTIFIER VAR)
                 WHY)))
  NIL)

;;; ... PASSABLE and friends

(DEFINE (NULL-INTERSECTION? A B)
  (OR (NULL? A)                         ;Speed bum
      (NULL? B)
      (D0 ((X A (CDR X)))
          ((NULL? X) T)
        ----
        (IF (MEMQ (CAR X) B) (RETURN NIL))
        ---)))

(DEFINE (NULL-EFFS-INTERSECTION? A B)
  (OR (NULL? A)
      (NULL? B)
      (EQ? A 'NONE)
      (EQ? B 'NONE)
      (COND ((EQ? A 'ANY) NIL)
            ((EQ? B 'ANY) NIL)
            (ELSE (NULL-INTERSECTION? A B)))))
       
(DEFINE (NULL-EFFS-INTERSECTION-EXCEPT-CONS A B)
  (COND ((OR (NULL? A) (NULL? B) (EQ? A 'NONE) (EQ? B 'NONE)) T)
        ((EQ? A 'ANY) (ALIKEV? B '(CONS)))
        ((EQ? B 'ANY) (ALIKEV? A '(CONS)))
        (ELSE
         (D0 ((X A (CDR X)))
             ((NULL? X) T)
           ----
           (IF (AND (NOT (EQ? (CAR X) 'CONS))
                    (MEMQ (CAR X) B))
               (RETURN NIL))
           ---))))

(DEFINE (EFFECTLESS X) (OR (NULL? X) (EQ? X 'NONE)))

(DEFINE (EFFECTLESS-EXCEPT-CONS X) (OR (EFFECTLESS X) (ALIKEV? X '(CONS))))

;;; This is a predicate true iff the two nodes can be executed in either order.

(DEFINE (PASSABLE NODE1 NODE2)
  (OR (NULL? NODE1)
      (NULL? NODE2)
      (LAMBDA-NODE? NODE1)              ; Horrible hack
      (LAMBDA-NODE? NODE2)
      (COND ((OR (EMPTY (NODE-EFFECTS NODE1))
                 (EMPTY (NODE-EFFECTS NODE2)))
             (BUG "side effects analysis is missing"
                  "will assume two nodes are not permutable")
             NIL)
            (ELSE
             (AND (NULL-INTERSECTION? (NODE-SETQS NODE1) (NODE-SETQS NODE2))
                  (NULL-INTERSECTION? (NODE-REFS NODE1) (NODE-SETQS NODE2))
                  (NULL-INTERSECTION? (NODE-SETQS NODE1) (NODE-REFS NODE2))
                  (NULL-EFFS-INTERSECTION? (NODE-EFFECTS NODE1)
                                           (NODE-AFFECTED NODE2))
                  (NULL-EFFS-INTERSECTION? (NODE-AFFECTED NODE1)
                                           (NODE-EFFECTS NODE2))
                  (NULL-EFFS-INTERSECTION-EXCEPT-CONS (NODE-EFFECTS NODE1)
                                                      (NODE-EFFECTS NODE2)))))))

(STAT-COUNTER *SUBSTITUTE-COUNT*
              "substitutions of an argument for a parameter")
(STAT-COUNTER *SUBSTITUTE-CALL-LAMBDA-TRY-COUNT*
              "attempts to propagate substitution into a LET body")
(STAT-COUNTER *SUBSTITUTE-CALL-LAMBDA-SUCCESS-COUNT*
              "successes at propagating substitution into a LET body")
(STAT-COUNTER *REMOVE-SETQ-COUNT*
          "replacement of a SETQ of an unreferenced variable by the SETQ body")

(DEFINE (META-SUBSTITUTE ARG VAR BOD)
  (IF (AND ARG (EMPTY (NODE-EFFECTS ARG)))
      (BUG "side effects analysis is missing for the argument ~S"
           "will plow ahead into deep disaster"
           (SEXPRFY ARG)))
  (BIND ((*ARG* ARG)
         (*VAR* VAR)
         (*REMOVALS* '()))
    (LET ((N *SUBSTITUTE-COUNT*))
      (BLOCK0 (MOBY-SUBSTITUTE BOD)
              (COND ((AND (FX> *SUBSTITUTE-COUNT* N) *TRACE-OPTIMIZERS?*)
                     (FORMAT *NOISE-OUTPUT*
                             "~%;***** ~D substitution~P for the ~
                                        variable ~S by~%"
                             (FX- *SUBSTITUTE-COUNT* N)
                             (FX- *SUBSTITUTE-COUNT* N)
                             (VARIABLE-IDENTIFIER *VAR*))
                     (SXNOISE *ARG*)
                     (FORMAT *NOISE-OUTPUT*
                             ";***** courtesy of META-SUBSTITUTE~%")))))))

(DEFINE (MOBY-SUBSTITUTE BOD)
  (IF (EMPTY (NODE-EFFECTS BOD))
      (BUG "side effects analysis is missing for the LAMBDA body ~S"
           "will plow ahead into deep disaster"
           (SEXPRFY BOD)))
  (SUBSTITUTE BOD))

(DEFINE-MACRO (MSUBST FROB)
  `(LET ((%%%%FROB%%%% ,FROB))
     (LET ((%%%%SFROB%%%% (SUBSTITUTE ,FROB)))
       (COND ((NOT (EQ? %%%%SFROB%%%% %%%%FROB%%%%))
              (SETF ,FROB %%%%SFROB%%%%)
              (REPROPAGATE %%%%SFROB%%%%))))))
;;; ... SUBSTITUTE

(DEFINE (SUBSTITUTE NODE)
  (DECLARE (SPECIAL *ARG* *VAR* *REMOVALS*))
  (COND ((AND (OR (NULL? *VAR*)
                  (EMPTY (NODE-REFS NODE))
                  (NOT (MEMQ *VAR* (NODE-REFS NODE))))
              (OR (NULL? *REMOVALS*)
                  (EMPTY (NODE-SETQS NODE))
                  (NULL-INTERSECTION? *REMOVALS* (NODE-SETQS NODE))))
         ;; Speed (?!) hack - exit quickly without useless recursion.
         NODE)
        (ELSE (NODE-DISPATCH SUBSTITUTE NODE))))

(DEFDISPATCH SUBSTITUTE CONSTANT (NODE FM) (IGNORE FM) NODE)

(DEFDISPATCH SUBSTITUTE STATIC (NODE FM) (IGNORE FM) NODE)

(DEFDISPATCH SUBSTITUTE VARIABLE (NODE FM)
  (DECLARE (SPECIAL *ARG* *VAR* *REMOVALS*))
  (COND ((AND (EQ? FM *VAR*) (NEQ? *ARG* *NON-NULL-OBJECT*))
         (LET ((NEWCODE (COPY-CODE *ARG*)))
           (SETF (NODE-PARENT NEWCODE) (NODE-PARENT NODE))
           (ERASE-ALL-NODES NODE)
           (INCR *SUBSTITUTE-COUNT*)
           (SETF (NODE-SUBSTP *ARG*) T)
           (SETF (NODE-METAP NEWCODE) NIL)
           NEWCODE))
        (ELSE NODE)))

(DEFDISPATCH SUBSTITUTE SETQ (NODE FM)
  (DECLARE (SPECIAL *REMOVALS*))
  (MSUBST (SETQ-BODY FM))
  (COND ((MEMQ (SETQ-VAR FM) *REMOVALS*)
         (LET ((BODY (SETQ-BODY FM)))
           (META-BEFORE-TRACE NODE)
           (SETF (NODE-PARENT BODY) (NODE-PARENT NODE))
           (ERASE-NODE NODE)
           (SETF (NODE-METAP BODY) NIL)
           (INCR *REMOVE-SETQ-COUNT*)
           (META-AFTER-TRACE BODY 'SUBSTITUTE)
           BODY))
        (ELSE (IF (NODE-METAP NODE)
               (SETF (NODE-METAP NODE)
                     (NODE-METAP (SETQ-BODY FM))))
           NODE)))

(DEFDISPATCH SUBSTITUTE LAMBDA (NODE FM)
  (DECLARE (SPECIAL *VAR* *ARG*))
  (IF (OR (NULL? *VAR*)
          (AND (EFFECTLESS-EXCEPT-CONS (NODE-EFFECTS *ARG*))
               (EFFECTLESS (NODE-AFFECTED *ARG*))
               (FX< (NODE-COMPLEXITY *ARG*) 6.))) ; ?!?
      (MSUBST (LAMBDA-BODY FM)))
  (IF (NODE-METAP NODE)
      (SETF (NODE-METAP NODE)
            (NODE-METAP (LAMBDA-BODY FM))))
  NODE)

(DEFDISPATCH SUBSTITUTE CATCH (NODE FM)
  ;; Careful!  Is this really okay?  See NO:SUBSTITUTE-CATCH.TXT.
  (MSUBST (CATCH-BODY FM))
  (IF (NODE-METAP NODE)
      (SETF (NODE-METAP NODE)
            (NODE-METAP (CATCH-BODY FM))))
  NODE)

(DEFDISPATCH SUBSTITUTE LABELS (NODE FM)
  (DECLARE (SPECIAL *ARG*))
  (D0 ((A (LABELS-VALS FM) (CDR A)))
      ((NULL? A)
       (MSUBST (LABELS-BODY FM)))
    ----
    (MSUBST (CAR A))
    (IF (NOT (PASSABLE *ARG* (CAR A))) (RETURN NIL))
    ---)
  (IF (AND (NODE-METAP NODE)
           (D0 ((A (LABELS-VALS FM) (CDR A)))
               ((NULL? A)
                (NOT (NODE-METAP (LABELS-BODY FM))))
             ----
             (IF (NOT (NODE-METAP (CAR A))) (RETURN T))
             ---))
      (SETF (NODE-METAP NODE) NIL))
  NODE)

;;; The theory here is that statements of the BLOCK may be permuted if
;;; the side effects analysis permits.  Thus as the substitution sweeps
;;; through the statements, those which are impassible are put onto
;;; the BARRIERS list.  Those which can move forward through all barriers
;;; are substituted into, and those which can move forward and be moved
;;; past are put onto the WINNERS list.  The last statement of the BLOCK
;;; is immovable.  At the end, the WINNERS are reassembled before the BARRIERS.

(STAT-COUNTER *SUBSTITUTE-BLOCK-PERMUTATION-COUNT*
              "the statements of a BLOCK are permuted")

(DEFDISPATCH SUBSTITUTE BLOCK (NODE FM)
  (DECLARE (SPECIAL *ARG*))  ; if removing setqs then *ARG* is () - lose!
  (DO ((A (BLOCK-ARGS FM) (CDR A))
       (BARRIERS '())
       (WINNERS '()))
      ((NULL? (CDR A))
       (IF (NULL? BARRIERS) (MSUBST (CAR A)))
       (LET ((NEWARGS (APPEND! (REVERSE WINNERS) (REVERSE BARRIERS) A)))
         (OR (FX= (LENGTH NEWARGS) (LENGTH (BLOCK-ARGS FM)))
             (BUGLET ((*NODE* NODE))
                     "lost track of arguments in SUBSTITUTE-BLOCK"
                     "unclear what to do from here"))
         (D0 ((A (BLOCK-ARGS FM) (CDR A))
              (N NEWARGS (CDR N)))
             ((NULL? A)
              (SETF (BLOCK-ARGS FM) NEWARGS))
           ----
           (COND ((NEQ? (CAR N) (CAR A))
                  ;; Aha! They have been permuted.
                  (META-BEFORE-TRACE NODE)
                  (INCR *SUBSTITUTE-BLOCK-PERMUTATION-COUNT*)
                  (SETF (BLOCK-ARGS FM) NEWARGS)
                  (SETF (NODE-METAP NODE) NIL)
                  (META-AFTER-TRACE NODE 'SUBSTITUTE-BLOCK)
                  (DUMP-CRUFT
                   *TRACE-OPTIMIZERS?*
                   (FORMAT *NOISE-OUTPUT*
                           '(";Winners: ~S~%;Barriers: ~S~%"
                             ";N.B. substitutions may have occurred, so this may look funny.~%")
                           (SEXPRFY-LIST WINNERS)
                           (SEXPRFY-LIST BARRIERS)))
                  (RETURN NIL)))
           ---)
         NODE))
    ----
    (D0 ((B BARRIERS (CDR B)))
        ((NULL? B)
         (MSUBST (CAR A))
         (IF (NODE-METAP NODE)
             (SETF (NODE-METAP NODE) (NODE-METAP (CAR A))))
         (MEVAL NODE (CAR A))
         (COND ((PASSABLE *ARG* (CAR A))
                (PUSH WINNERS (CAR A)))
               (ELSE (SETQ BARRIERS (APPEND! BARRIERS (LIST (CAR A)))))))
      ----
      (COND ((NOT (PASSABLE (CAR A) (CAR B)))
             (PUSH BARRIERS (CAR A))
             (RETURN NIL)))
      ---)
    ---)
   (IF (AND (NODE-METAP NODE)
           (D0 ((A (BLOCK-ARGS FM) (CDR A)))
               ((NULL? A) NIL)
             ----
             (IF (NULL? (NODE-METAP (CAR A))) (RETURN T))
             ---))
      (SETF (NODE-METAP NODE) NIL))
  NODE)

(DEFDISPATCH SUBSTITUTE IF (NODE FM)
  (DECLARE (SPECIAL *ARG* *VAR*))
  (MSUBST (IF-PRED FM))
  (COND ((AND (EQ? *ARG* *NON-NULL-OBJECT*)
              (VARIABLE-NODE? (IF-PRED FM))
              (EQ? (NODE-FORM (IF-PRED FM)) *VAR*))
         (LET ((RESULT (IF-CON FM)))
           (META-BEFORE-TRACE NODE)
           (SETF (NODE-PARENT RESULT) (NODE-PARENT NODE))
           (ERASE-ALL-NODES (IF-PRED FM))
           (ERASE-ALL-NODES (IF-ALT FM))
           (ERASE-NODE NODE)
           (INCR *SUBSTITUTE-COUNT*)
           (SETF (NODE-METAP RESULT) NIL)
           (META-AFTER-TRACE RESULT 'SUBSTITUTE-IF)
           (SUBSTITUTE RESULT)))
        (ELSE
         (COND ((OR (NULL? *VAR*)     ; removing setqs?
                    (AND (PASSABLE *ARG* (IF-PRED FM))
                         (EFFECTLESS-EXCEPT-CONS (NODE-EFFECTS *ARG*))))
                (MSUBST (IF-CON FM))
                (MSUBST (IF-ALT FM))))
         (IF (NODE-METAP NODE)
             (SETF (NODE-METAP NODE)
                   (AND (NODE-METAP (IF-PRED FM))
                        (NODE-METAP (IF-CON FM))
                        (NODE-METAP (IF-ALT FM)))))
         NODE)))

(DEFDISPATCH SUBSTITUTE CASE (NODE FM)
  (MSUBST (CASE-KEY FM))
  (COND ((OR (NULL? *VAR*)              ; removing setqs?
             (AND (PASSABLE *ARG* (CASE-KEY FM))
                  (EFFECTLESS-EXCEPT-CONS (NODE-EFFECTS *ARG*))))
         (MSUBST (CASE-ELSE FM))
         (WALK (LAMBDA (X) (MSUBST (CDR X)))
               (CASE-CLAUSES FM))))
  (IF (AND (NODE-METAP NODE)
           (OR (NULL? (NODE-METAP (CASE-KEY FM)))
               (NULL? (NODE-METAP (CASE-ELSE FM)))
               (D0 ((C (CASE-CLAUSES FM) (CDR C)))
                   ((NULL? C) NIL)
                 ----
                 (IF (NULL? (NODE-METAP (CDAR C))) (RETURN T))
                 ---)))
      (SETF (NODE-METAP NODE) NIL))
  NODE)

(DEFDISPATCH SUBSTITUTE CALL (NODE FM)
  (MSUBST (CALL-FUNCTION FM))
  (IF (LAMBDA-NODE? (CALL-FUNCTION FM))
      (INCR *SUBSTITUTE-CALL-LAMBDA-TRY-COUNT*))
  (D0 ((A (CALL-ARGS FM) (CDR A)))
      ((NULL? A)
       (COND ((LAMBDA-NODE? (CALL-FUNCTION FM))
              (MSUBST (LAMBDA-BODY (NODE-FORM (CALL-FUNCTION FM))))
              (IF (NODE-METAP (CALL-FUNCTION FM))
                  (SETF (NODE-METAP (CALL-FUNCTION FM))
                        (NODE-METAP (LAMBDA-BODY (NODE-FORM (CALL-FUNCTION FM))))))
              (INCR *SUBSTITUTE-CALL-LAMBDA-SUCCESS-COUNT*))))
    ----
    (MSUBST (CAR A))
    (OR (PASSABLE *ARG* (CAR A)) (RETURN NIL))
    ---)
  (IF (AND (NODE-METAP NODE)
           (D0 ((A (CALL-ARGS FM) (CDR A)))
               ((NULL? A)
                (NOT (NODE-METAP (CALL-FUNCTION FM))))
             ----
             (IF (NOT (NODE-METAP (CAR A))) (RETURN T))
             ---))
      (SETF (NODE-METAP NODE) NIL))
  NODE)
