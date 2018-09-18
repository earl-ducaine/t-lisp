(herald (tcomp optimize t 217)
        (env tcomp))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University


;;;; Optimization

;;; Undo pass-1 analysis: ERASE-NODES and friends

(DEFINE-INTEGRABLE (ERASE-NODE NODE)
  (ERASE-NODES NODE 'ONCE))
(DEFINE-INTEGRABLE (ERASE-ALL-NODES NODE)
  (ERASE-NODES NODE 'ALL))
(DEFINE-INTEGRABLE (ERASE-NODES-IN-LIST NODELIST)
  (ERASE-NODE-LIST NODELIST 'ONCE))
(DEFINE-INTEGRABLE (ERASE-ALL-NODES-IN-LIST NODELIST)
  (ERASE-NODE-LIST NODELIST 'ALL))

(DEFINE (ERASE-NODES NODE ERASEP)
  (COND (ERASEP
         (NODE-DISPATCH ERASE-NODES NODE (IF (EQ? ERASEP 'ALL) 'ALL NIL))
         (COND (*POOL?*
                (RESET-STRUCTURE NODE)
                (RETURN-TO-POOL *NODE-POOL* NODE))))))

(DEFDISPATCH ERASE-NODES CONSTANT (NODE FM ALLP))

(DEFDISPATCH ERASE-NODES STATIC (NODE FM ALLP))

(DEFDISPATCH ERASE-NODES VARIABLE (NODE FM ALLP)
  (SETF (VARIABLE-READ-REFS FM)
        (DELQ NODE (VARIABLE-READ-REFS FM))))

(DEFDISPATCH ERASE-NODES SETQ (NODE FM ALLP)
  (ERASE-NODES (SETQ-BODY FM) ALLP)
  (IF (NOT (STATIC? (SETQ-VAR FM)))
      (SETF (VARIABLE-WRITE-REFS (SETQ-VAR FM))
            (DELQ NODE (VARIABLE-WRITE-REFS (SETQ-VAR FM))))))

(DEFDISPATCH ERASE-NODES LAMBDA (NODE FM ALLP)
  (ERASE-NODES (LAMBDA-BODY FM) ALLP))

(DEFDISPATCH ERASE-NODES CATCH (NODE FM ALLP)
  (ERASE-NODES (CATCH-BODY FM) ALLP))

(DEFDISPATCH ERASE-NODES LABELS (NODE FM ALLP)
  (ERASE-NODES (LABELS-BODY FM) ALLP)
  (ERASE-NODE-LIST (LABELS-VALS FM) ALLP))

(DEFDISPATCH ERASE-NODES BLOCK (NODE FM ALLP)
  (ERASE-NODE-LIST (BLOCK-ARGS FM) ALLP))

(DEFDISPATCH ERASE-NODES IF (NODE FM ALLP)
  (ERASE-NODES (IF-PRED FM) ALLP)
  (ERASE-NODES (IF-CON FM) ALLP)
  (ERASE-NODES (IF-ALT FM) ALLP))

(DEFDISPATCH ERASE-NODES CASE (NODE FM ALLP)
  (ERASE-NODES (CASE-KEY FM) ALLP)
  (ERASE-NODES (CASE-ELSE FM) ALLP)
  (WALK (LAMBDA (C) (ERASE-NODES (CDR C) ALLP))
        (CASE-CLAUSES FM)))

(DEFDISPATCH ERASE-NODES CALL (NODE FM ALLP)
  (ERASE-NODES (CALL-FUNCTION FM) ALLP)
  (ERASE-NODE-LIST (CALL-ARGS FM) ALLP))

(DEFINE (ERASE-NODE-LIST NODE-LIST ALLP)
  (WALK (LAMBDA (NODE) (ERASE-NODES NODE ALLP)) NODE-LIST))

;;; Meta-evaluation

;;; Fix this stuff.
(DEFINE (META-BEFORE-TRACE X)
  (COND (*TRACE-OPTIMIZERS?*
         (FORMAT *NOISE-OUTPUT* "~%;***** Optimizing this form:~%")
         (SXNOISE X))))

(DEFINE (META-AFTER-TRACE X WHO)
  (COND (*TRACE-OPTIMIZERS?*
         (FORMAT *NOISE-OUTPUT* ";*** to be this form:~%")
         (SXNOISE X)
         (FORMAT *NOISE-OUTPUT* ";***** courtesy of ~S~%" WHO)))
  X)

(DEFINE (META-EVALUATE NODE)
  (COND ((NODE-METAP NODE) NODE)
        (ELSE (NODE-DISPATCH META-EVALUATE NODE))))

(DEFDISPATCH META-EVALUATE CONSTANT (NODE FM)
  (REANALYZE NODE))

(DEFDISPATCH META-EVALUATE STATIC (NODE FM)
  (REANALYZE NODE))

(DEFDISPATCH META-EVALUATE VARIABLE (NODE FM)
  (REANALYZE NODE))

;;; META-SETQ does an optimization suggested by KMP, which probably arises
;;; fairly infrequently, but is an easy hack.  The trick is that a SETQ
;;; at the same tail-recursion level as the node which binds the variable
;;; being SETQ'd can't be at all useful, so it can be replaced by the
;;; body of the SETQ.

(STAT-COUNTER *USELESS-SETQ-FLUSH-COUNT*
              "(LAMBDA (A) ... (SETQ A X)) becomes (LAMBDA (A) ... X)")

(DEFDISPATCH META-EVALUATE SETQ (NODE FM)
  (MEVAL NODE (SETQ-BODY FM))
  (LET ((VAR (SETQ-VAR FM))
        (BODY (SETQ-BODY FM)))
    (COND ((AND (NOT (STATIC? VAR))
                (EQ? (NODE-LEVEL NODE) (NODE-LEVEL (VARIABLE-BINDER VAR))))
           (META-BEFORE-TRACE NODE)
           (SETF (NODE-PARENT BODY) (NODE-PARENT NODE))
           (ERASE-NODE NODE)
           (META-AFTER-TRACE BODY 'META-SETQ)
           (INCR *USELESS-SETQ-FLUSH-COUNT*)
           BODY)
          (ELSE (REANALYZE NODE)))))

;;; LAMBDA

(DEFDISPATCH META-EVALUATE LAMBDA (NODE FM)
  (MEVAL NODE (LAMBDA-BODY FM))
  ;; Turn unreferenced vars into ()'s
  (FLUSH-UNREFERENCED-PARAMETERS NODE)
  (REANALYZE NODE))

(STAT-COUNTER *META-LAMBDA-VARIABLE-FLUSH-COUNT*
              "unreferenced parameter is flushed")

;;; Currently there's no code here to flush an unreferenced rest-parameter.
(DEFINE (FLUSH-UNREFERENCED-PARAMETERS NODE)
  (LET ((FN (NODE-FORM NODE)))
    (DO ((V (IF (NOT (EMPTY (LAMBDA-RESTVAR FN)))
                (CONS (LAMBDA-RESTVAR FN) (LAMBDA-VARS FN))     ; Hack
              (LAMBDA-VARS FN))
            (CDR V))
         (REMOVALS '()))
        ((NULL? V)
         (COND (REMOVALS
                (SETF (LAMBDA-BODY FN)  ;!!! what about PARENT pointer?
                      (META-EVALUATE
                       (META-REMOVE-SETQS (LAMBDA-BODY FN) REMOVALS)))
                (WALK (LAMBDA (VV)
                        (IF (VARIABLE-WRITE-REFS VV)
                            (BUG "failed to remove SETQ's on ~S"
                                 "will plunge ahead into dangerous waters"
                                 (VARIABLE-IDENTIFIER VV))))
                      REMOVALS))))
      ----
      (COND ((AND (CAR V) (NULL? (VARIABLE-READ-REFS (CAR V))))
             (LET ((BODY (LAMBDA-BODY FN)))
               (IF (MEMQ (CAR V) (NODE-REFS BODY))
                   (BUG "\"unreferenced\" parameter ~S is referenced in body of ~S"
                        "will forge ahead assuming everything is okay"
                        (VARIABLE-IDENTIFIER (CAR V))
                        (SEXPRFY NODE)))
               (COND ((NULL? (VARIABLE-WRITE-REFS (CAR V)))
                      (IF (MEMQ (CAR V) (NODE-SETQS BODY))
                          (BUG "\"unmodified\" parameter ~S is SETQ'd in body of ~S"
                               "will forge ahead assuming everything is okay"
                               (VARIABLE-IDENTIFIER (CAR V))
                               (SEXPRFY NODE))))
                     (ELSE (PUSH REMOVALS (CAR V)))))
             (COND ((EQ? (CAR V) (LAMBDA-RESTVAR FN))
                    (SETF (LAMBDA-RESTVAR FN) '()))
                   (ELSE (RPLACA V '())))
             (INCR *META-LAMBDA-VARIABLE-FLUSH-COUNT*)
             ))
      ---)))

(DEFINE (META-REMOVE-SETQS BOD REMOVALS)
  (BIND ((*ARG* '())
         (*VAR* '())
         (*REMOVALS* REMOVALS))
    (MOBY-SUBSTITUTE BOD)))

;;; CATCH

(STAT-COUNTER *META-CATCH-VARIABLE-FLUSH-COUNT*
              "unreferenced CATCH variable is eliminated")
(STAT-COUNTER *META-CATCH-BETA-CONVERSION-COUNT*
              "(CATCH () BODY) becomes BODY")

(DEFDISPATCH META-EVALUATE CATCH (NODE FM)
  (MEVAL NODE (CATCH-BODY FM))
  (LET ((VAR (CATCH-VAR FM)))
    (COND ((AND VAR (NULL? (VARIABLE-READ-REFS VAR)))
           (IF (VARIABLE-WRITE-REFS VAR)
               (SETF (CATCH-BODY FM)
                     (META-EVALUATE
                      (META-REMOVE-SETQS (CATCH-BODY FM) (LIST VAR)))))
           (SETF (CATCH-VAR FM) '())
           (INCR *META-CATCH-VARIABLE-FLUSH-COUNT*))))
  (COND ((NULL? (CATCH-VAR FM))
         (LET ((BODY (CATCH-BODY FM)))
           (META-BEFORE-TRACE NODE)
           (SETF (NODE-PARENT BODY) (NODE-PARENT NODE))
           (ERASE-NODE NODE)
           (META-AFTER-TRACE BODY 'META-CATCH)
           (INCR *META-CATCH-BETA-CONVERSION-COUNT*)
           BODY))
        (ELSE (REANALYZE NODE))))

;;; LABELS

;;; The optimization for a LABELS is very similar to that for a LAMBDA.
;;; The main thing is to eliminate unreferenced variables.

(STAT-COUNTER *META-LABELS-VARIABLE-FLUSH-COUNT*
              "unreferenced variables eliminated from a LABELS")
(STAT-COUNTER *META-LABELS-VALUE-FLUSH-COUNT*
              "unused values eliminated from a LABELS")
(STAT-COUNTER *META-LABELS-BETA-CONVERSION-COUNT*
              "(LABELS () FOO) becomes FOO")

(DEFDISPATCH META-EVALUATE LABELS (NODE FM)
  (WALKCDR (LAMBDA (X) (MEVAL NODE (CAR X)))
           (LABELS-VALS FM))
  (MEVAL NODE (LABELS-BODY FM))
  ;; [Someday, write code here for doing substitutions.  This is not easy.]
  ;; Attempt to flush unreferenced parameters.
  (DO ((V (LABELS-VARS FM) (CDR V))
       (REMOVALS '()))
      ((NULL? V)
       (COND (REMOVALS
              (SETF (LABELS-BODY FM)
                    (META-EVALUATE
                     (META-REMOVE-SETQS (LABELS-BODY FM) REMOVALS)))
              (WALK (LAMBDA (VV)
                      (IF (VARIABLE-WRITE-REFS VV)
                          (BUG "failed to remove SETQ's on ~S"
                               "will plunge ahead into dangerous waters"
                               (VARIABLE-IDENTIFIER VV))))
                    REMOVALS))))
    ----
    (COND ((AND (CAR V) (NULL? (VARIABLE-READ-REFS (CAR V))))
           (IF (VARIABLE-WRITE-REFS (CAR V)) (push REMOVALS (CAR V)))
           (RPLACA V '())
           (INCR *META-LABELS-VARIABLE-FLUSH-COUNT*)
           ))
    ---)
  ;; Flush unused values if possible.
  (DO ((V (LABELS-VARS FM) (CDR V))
       (X '() (IF (NEQ? (LABELS-VARS FM) (CDR V)) V))
       (A (LABELS-VALS FM) (CDR A)))
      ((NULL? V))
    ----
    ;; Can flush if unreferenced and either has no side
    ;; effects or does have side effects but was
    ;; substituted for the (unique) reference.
    (COND ((AND (NULL? (CAR V))
                (OR (AND (EFFECTLESS-EXCEPT-CONS (NODE-EFFECTS (CAR A)))
                         (NULL? (NODE-SETQS (CAR A))))
                    (NODE-SUBSTP (CAR A))))
           (ERASE-ALL-NODES (CAR A))
           (SETF (LABELS-VALS FM) (DELQ (CAR A) (LABELS-VALS FM)))
           (COND ((NULL? X)
                  (SETF (LABELS-VARS FM) (CDR V)))
                 (ELSE (RPLACD X (CDR V))))
           (INCR *META-LABELS-VALUE-FLUSH-COUNT*)))
    ---)
  ;; If no arguments left, flush the LABELS and return
  ;;  just the body.
  (COND ((NULL? (LABELS-VARS FM))
         (LET ((BODY (LABELS-BODY FM)))
           (META-BEFORE-TRACE NODE)
           (SETF (NODE-PARENT BODY) (NODE-PARENT NODE))
           (ERASE-NODE NODE)
           (INCR *META-LABELS-BETA-CONVERSION-COUNT*)
           (META-AFTER-TRACE BODY 'META-LABELS)
           BODY))
        (ELSE (REANALYZE (LABELS-BODY FM))
              (REANALYZE NODE))))

;;; BLOCK

;;; Two interesting transformations here.
;;; (1) Eliminate BLOCK statements which have no side effects.
;;; (2) Convert BLOCK of one statement to that statement.

(STAT-COUNTER *META-BLOCK-ELIMINATE-COUNT*
              "statements eliminated from a BLOCK")
(STAT-COUNTER *META-BLOCK-SOLO-COUNT*
              "BLOCK of one statement simplified to the statement")
(STAT-COUNTER *META-BLOCK-DENEST-COUNT*
              "one BLOCK within another is flattened out")

(DEFDISPATCH META-EVALUATE BLOCK (NODE FM)
  (WALKCDR (LAMBDA (X) (MEVAL NODE (CAR X))) (BLOCK-ARGS FM))
  (DO ((A (BLOCK-ARGS FM) (CDR A))
       (FLAG NIL))
      ((NULL? A)
       (DO ((A (BLOCK-ARGS FM) (CDR A)))
           ((NULL? (CDR A))
            (LET ((RESULT (COND ((NULL? (CDR (BLOCK-ARGS FM)))
                                 (SETF (NODE-PARENT (CAR (BLOCK-ARGS FM)))
                                       (NODE-PARENT NODE))
                                 (INCR *META-BLOCK-SOLO-COUNT*)
                                 (COND ((NULL? FLAG)
                                        (META-BEFORE-TRACE NODE)
                                        (SETQ FLAG T)))
                                 (ERASE-NODE NODE)
                                 (CAR (BLOCK-ARGS FM)))
                                (ELSE (REANALYZE NODE)))))
              (IF FLAG (META-AFTER-TRACE RESULT 'META-BLOCK))
              RESULT))
         ----
         (COND ((AND (NULL? (NODE-SETQS (CAR A)))
                     (EFFECTLESS-EXCEPT-CONS (NODE-EFFECTS (CAR A))))
                (INCR *META-BLOCK-ELIMINATE-COUNT*)
                (COND ((NULL? FLAG)
                       (META-BEFORE-TRACE NODE)
                       (SETQ FLAG T)))
                (SETF (BLOCK-ARGS FM) (DELQ (CAR A) (BLOCK-ARGS FM)))
                (ERASE-ALL-NODES (CAR A))))
         ---))
    ----
    (COND ((BLOCK-NODE? (CAR A))
           (COND ((NULL? FLAG)
                  (META-BEFORE-TRACE NODE)
                  (SETQ FLAG T)))
           (INCR *META-BLOCK-DENEST-COUNT*)
           (LET ((PN (CAR A)))
             (WALK (LAMBDA (A) (SETF (NODE-PARENT A) NODE))
                   (BLOCK-ARGS (NODE-FORM PN)))
             (LET ((LA (LASTCDR (BLOCK-ARGS (NODE-FORM PN)))))
               (RPLACD LA (CDR A))
               (RPLACA A (CAR (BLOCK-ARGS (NODE-FORM PN))))
               (RPLACD A (CDR (BLOCK-ARGS (NODE-FORM PN))))
               (ERASE-NODE PN)
               (COND ((EQ? (NODE-LEVEL (CAR LA)) PN)
                      (SETF (NODE-LEVEL (CAR LA)) (CAR LA))
                      (PROPAGATE (CAR LA))))
               (IF (NEQ? (CAR A) (CAR LA))      ;???
                   (SETQ A LA))))))
    ---))

;;; IF

(DEFDISPATCH META-EVALUATE IF (NODE FM)
  (MEVAL NODE (IF-PRED FM))
  (MEVAL NODE (IF-CON FM))
  (MEVAL NODE (IF-ALT FM))
  (LET ((PR (NODE-FORM (IF-PRED FM))))
    (CASE (STYPE PR)
      ((IF) (META-IF-IF NODE))
      ((CONSTANT) (META-IF-CONSTANT NODE))
      ((VARIABLE) (META-IF-VARIABLE NODE))      ; statics too??
      ((BLOCK) (META-IF-BLOCK NODE))
      ((CALL)
       (COND ((LAMBDA-NODE? (CALL-FUNCTION PR))
              (META-IF-CALL-LAMBDA NODE))
             (ELSE (REANALYZE NODE))))
      (ELSE (REANALYZE NODE)))))

;;; Transform (IF (IF A B C) D E) into
;;;     ((LAMBDA (X Y)
;;;              (IF A (IF B (X) (Y)) (IF C (X) (Y))))
;;;      (LAMBDA () D)
;;;      (LAMBDA () E))

(STAT-COUNTER *META-IF-NOT-COUNT* "(IF (NOT A) B C) becomes (IF A C B)")
(STAT-COUNTER *META-IF-IF-COUNT* "IF of an IF is involuted (anchor pointing)")

(DEFINE (META-IF-IF NODE)
  (LET* ((FM (NODE-FORM NODE))
         (PFM (NODE-FORM (IF-PRED FM))))
    (COND ((AND (CONSTANT-NODE? (IF-CON PFM))
                (CONSTANT-NODE? (IF-ALT PFM)))
           (LET ((NEWCON (IF (CONSTANT-VALUE (NODE-FORM (IF-CON PFM)))
                             (IF-CON FM)
                             (IF-ALT FM)))
                 (NEWALT (IF (CONSTANT-VALUE (NODE-FORM (IF-ALT PFM)))
                             (IF-CON FM)
                             (IF-ALT FM))))
           ;; Speed hack.  This would effectively be taken care of by
           ;;  the more general case (below), but is so common that it
           ;;  deserves special attention.
           (COND ((NEQ? NEWCON NEWALT)
                  (META-BEFORE-TRACE NODE)
                  (ERASE-NODE (IF-CON PFM))
                  (ERASE-NODE (IF-ALT PFM))
                  (LET ((NEWNODE (IF-PRED FM))) ;Could use NODE instead?
                    (SETF (IF-CON PFM) NEWCON)
                    (SETF (IF-ALT PFM) NEWALT)
                    (SETF (NODE-PARENT NEWCON) NEWNODE)
                    (SETF (NODE-PARENT NEWALT) NEWNODE)
                    (SETF (NODE-PARENT NEWNODE) (NODE-PARENT NODE))
                    (ERASE-NODE NODE)
                    (INCR *META-IF-NOT-COUNT*)
                    (META-AFTER-TRACE NEWNODE 'META-IF-IF)
                    (REANALYZE NEWNODE)))
                 ((EFFECTLESS-EXCEPT-CONS (NODE-EFFECTS (IF-PRED FM)))
                  (META-BEFORE-TRACE NODE)
                  (ERASE-NODE (IF-CON PFM))
                  (ERASE-NODE (IF-ALT PFM))
                  (SETF (NODE-PARENT NEWCON) (NODE-PARENT NODE))
                  (ERASE-NODE (IF-PRED FM))
                  (ERASE-NODE (IF (EQ? NEWCON (IF-CON FM))
                                  (IF-ALT FM)
                                (IF-CON FM)))
                  (ERASE-NODE NODE)
                  (META-AFTER-TRACE NEWCON 'META-IF-IF)
                  NEWCON)
                 (ELSE (REANALYZE NODE)))))  ; YUCK!  This shouldn't happen.
          (ELSE
           (META-BEFORE-TRACE NODE)
           (LET* ((CONVAR (GENERATE-SYMBOL 'IFCON))
                  (ALTVAR (GENERATE-SYMBOL 'IFALT))
                  (NEWNODE (ALPHA-TOPLEVEL
                            `((LAMBDA (,CONVAR ,ALTVAR)
                                (IF ,(INT (IF-PRED PFM))
                                    (IF ,(INT (IF-CON PFM))
                                        (,CONVAR)
                                      (,ALTVAR))
                                  (IF ,(INT (IF-ALT PFM))
                                      (,CONVAR)
                                    (,ALTVAR))))
                              (LAMBDA () ,(INT (IF-CON FM)))
                              (LAMBDA () ,(INT (IF-ALT FM)))))))
             (SETF (NODE-PARENT NEWNODE) (NODE-PARENT NODE))
             (SETF (NODE-LEVEL NEWNODE) (NODE-LEVEL NODE))
             (PROPAGATE NEWNODE)
             (ERASE-NODE NODE)
             (ERASE-NODE (IF-PRED FM))
             (INCR *META-IF-IF-COUNT*)
             (META-AFTER-TRACE NEWNODE 'META-IF-IF)
             NEWNODE)))))

;;; Tranform (IF '() A B) into B, or (IF 'XXX A B) into A.

(STAT-COUNTER *META-IF-CONSTANT-COUNT*
              "IF of a constant has dead arm eliminated")

(DEFINE (META-IF-CONSTANT NODE)
  (META-BEFORE-TRACE NODE)
  (LET ((FM (NODE-FORM NODE)))
       (LET ((CON (IF-CON FM))
             (ALT (IF-ALT FM))
             (V (CONSTANT-VALUE (NODE-FORM (IF-PRED FM)))))
            (LET ((RESULT (COND (V (ERASE-ALL-NODES ALT) CON)
                                (ELSE (ERASE-ALL-NODES CON) ALT))))
                 (SETF (NODE-PARENT RESULT) (NODE-PARENT NODE))
                 (ERASE-NODE NODE)
                 (ERASE-ALL-NODES (IF-PRED FM))
                 (INCR *META-IF-CONSTANT-COUNT*)
                 (META-AFTER-TRACE RESULT 'META-IF-CONSTANT)
                 RESULT))))

;;; In (IF VAR XXX YYY), substitute NIL for VAR in YYY, and "non-NIL" for
;;;  VAR in XXX.

(DEFINE (META-IF-VARIABLE NODE)
  (LET* ((FM (NODE-FORM NODE))
         (VAR (NODE-FORM (IF-PRED FM))))
    (COND ((NULL? (VARIABLE-WRITE-REFS VAR))
           (LET ((NEWCON (META-EVALUATE (META-SUBSTITUTE *NON-NULL-OBJECT*
                                                         VAR
                                                         (IF-CON FM)))))
             (COND ((NEQ? NEWCON (IF-CON FM))
                    (SETF (IF-CON FM) NEWCON)
                    (REPROPAGATE NEWCON))))
           (LET ((NEWALT (META-EVALUATE (META-SUBSTITUTE *NULL-OBJECT*
                                                         VAR
                                                         (IF-ALT FM)))))
             (COND ((NEQ? NEWALT (IF-ALT FM))
                    (SETF (IF-ALT FM) NEWALT)
                    (REPROPAGATE NEWALT))))))
    (REANALYZE NODE)))
                       
;;; Tranform (IF (BLOCK A B C ... Z) P Q) into (BLOCK A B C ... (IF Z P Q)).
 
(STAT-COUNTER *META-IF-BLOCK-COUNT* "IF of a BLOCK becomes BLOCK of an IF")
 
(DEFINE (META-IF-BLOCK NODE)
  (META-BEFORE-TRACE NODE)
  (LET* ((FM (NODE-FORM NODE))
         (PN (IF-PRED FM))
         (PFM (NODE-FORM PN))
         (LASTARG (LASTCDR (BLOCK-ARGS PFM))))
    (SETF (NODE-METAP NODE) NIL)
    (SETF (NODE-METAP PN) NIL)
    (SETF (IF-PRED FM) (CAR LASTARG))
    (RPLACA LASTARG NODE)
    (SETF (NODE-PARENT PN) (NODE-PARENT NODE))
    (SETF (NODE-PARENT NODE) PN)
    (SETF (NODE-PARENT (IF-PRED FM)) NODE)
    (REPROPAGATE (IF-PRED FM))          ; !!!
    (INCR *META-IF-BLOCK-COUNT*)
    (META-AFTER-TRACE PN 'META-IF-BLOCK)
    PN))

;;; Tranform (IF ((LAMBDA (A ...) B) X ...) P Q)
;;;  into ((LAMBDA (A ...) (IF B P Q)) X ...).
;;; Note that we may be losing if e.g. A would be a JUMP LAMBDA.  After
;;;  the transformation it won't be.

(STAT-COUNTER *META-IF-CALL-LAMBDA-COUNT* "IF of a LET becomes LET of an IF")

(DEFINE (META-IF-CALL-LAMBDA NODE)
  (LET* ((FM (NODE-FORM NODE))
         (CA (IF-PRED FM))
         (LM (CALL-FUNCTION (NODE-FORM CA)))
         (FN (NODE-FORM LM)))
    (COND ;;((EVERY (LAMBDA (X) (LAMBDA-NODE? X)) (CALL-ARGS (NODE-FORM CA)))
          ;; (BREAK NOT-META-IF-CALL-LAMBDA)
          ;; (REANALYZE NODE))
          (ELSE
           (META-BEFORE-TRACE NODE)
           (SETF (NODE-METAP NODE) NIL)
           (SETF (NODE-METAP CA) NIL)
           (SETF (NODE-METAP LM) NIL)
           (SETF (IF-PRED FM) (LAMBDA-BODY FN))
           (SETF (LAMBDA-BODY FN) NODE)
           (SETF (NODE-PARENT CA) (NODE-PARENT NODE))
           (SETF (NODE-PARENT NODE) LM)
           (SETF (NODE-PARENT (IF-PRED FM)) NODE)
           (REPROPAGATE (IF-PRED FM)) ; ???? fix Nix's problem, sort of
           (INCR *META-IF-CALL-LAMBDA-COUNT*)
           (META-AFTER-TRACE CA 'META-IF-CALL-LAMBDA)
           CA))))

;;; CASE

(DEFDISPATCH META-EVALUATE CASE (NODE FM)
  (MEVAL NODE (CASE-KEY FM))
  (LET ((KEY (NODE-FORM (CASE-KEY FM))))
    (COND ((EQ? (STYPE KEY) 'CONSTANT)
           (META-CASE-CONSTANT NODE))
          (ELSE
           (MEVAL NODE (CASE-ELSE FM))
           (WALK (LAMBDA (C) (MEVAL NODE (CDR C)))
                 (CASE-CLAUSES FM))
           (CASE (STYPE KEY)
             ;;((IF) (META-CASE-IF NODE))
             ((BLOCK) (META-CASE-BLOCK NODE))
             ((CALL)
              (COND ((LAMBDA-NODE? (CALL-FUNCTION KEY))
                     (META-CASE-CALL-LAMBDA NODE))
                    (ELSE (REANALYZE NODE))))
             (ELSE (REANALYZE NODE)))))))

;;; Transform (CASE <constant> ...) into the appropriate clause.

(STAT-COUNTER *META-CASE-CONSTANT-COUNT*
              "CASE of a constant reduces to a clause")

(DEFINE (META-CASE-CONSTANT NODE)
  (META-BEFORE-TRACE NODE)
  (LET* ((FM (NODE-FORM NODE))
         (VAL (CONSTANT-VALUE (NODE-FORM (CASE-KEY FM)))))
        (ERASE-ALL-NODES (CASE-KEY FM))
        (INCR *META-CASE-CONSTANT-COUNT*)
        (DO ((C (CASE-CLAUSES FM) (CDR C))
             (RESULT (CASE-ELSE FM)))
            ((NULL? C)
             (SETF (NODE-PARENT RESULT) (NODE-PARENT NODE))
             (ERASE-NODE NODE)
             (META-AFTER-TRACE RESULT 'META-CASE-CONSTANT)
             RESULT)
            ----
            (COND ((MEMBER VAL (CAAR C))
                   (ERASE-ALL-NODES RESULT)
                   (SETQ RESULT (CDAR C)))
                  (ELSE (ERASE-ALL-NODES (CDAR C))))
            ---)))

;;; Tranform (CASE (BLOCK A B C ... Z) ...) into
;;; (BLOCK A B C ... (CASE Z ...)).

(STAT-COUNTER *META-CASE-BLOCK-COUNT*
              "CASE of a BLOCK becomes BLOCK of a CASE")

(DEFINE (META-CASE-BLOCK NODE)
  (META-BEFORE-TRACE NODE)
  (LET* ((FM (NODE-FORM NODE))
         (PN (CASE-KEY FM))
         (PFM (NODE-FORM PN))
         (LASTARG (LASTCDR (BLOCK-ARGS PFM))))
    (SETF (NODE-METAP NODE) NIL)
    (SETF (NODE-METAP PN) NIL)
    (SETF (CASE-KEY FM) (CAR LASTARG))
    (RPLACA LASTARG NODE)
    (SETF (NODE-PARENT PN) (NODE-PARENT NODE))
    (SETF (NODE-PARENT NODE) PN)
    (SETF (NODE-PARENT (CASE-KEY FM)) NODE)
    (INCR *META-CASE-BLOCK-COUNT*)
    (META-AFTER-TRACE PN 'META-CASE-BLOCK)
    PN))

;;; Tranform (CASE ((LAMBDA (A ...) B) X ...) ...)
;;; into ((LAMBDA (A ...) (CASE B ...)) X ...).

(STAT-COUNTER *META-CASE-CALL-LAMBDA-COUNT*
              "CASE of a LET becomes LET of a CASE")

(DEFINE (META-CASE-CALL-LAMBDA NODE)
  (META-BEFORE-TRACE NODE)
  (LET* ((FM (NODE-FORM NODE))
         (CA (CASE-KEY FM))
         (LM (CALL-FUNCTION (NODE-FORM CA)))
         (FN (NODE-FORM LM)))
    (SETF (NODE-METAP NODE) NIL)
    (SETF (NODE-METAP CA) NIL)
    (SETF (NODE-METAP LM) NIL)
    (SETF (CASE-KEY FM) (LAMBDA-BODY FN))
    (SETF (LAMBDA-BODY FN) NODE)
    (SETF (NODE-PARENT CA) (NODE-PARENT NODE))
    (SETF (NODE-PARENT NODE) LM)
    (SETF (NODE-PARENT (CASE-KEY FM)) NODE)
    (INCR *META-CASE-CALL-LAMBDA-COUNT*)
    (META-AFTER-TRACE CA 'META-CASE-CALL-LAMBDA)
    CA))


;;; CALL

(DEFDISPATCH META-EVALUATE CALL (NODE FM)
  (WALKCDR (LAMBDA (X) (MEVAL NODE (CAR X)))
           (CALL-ARGS FM))
  (CASE (STYPE (NODE-FORM (CALL-FUNCTION FM)))
    ((STATIC) (META-CALL-STATIC NODE))
    ((VARIABLE) (META-CALL-VARIABLE NODE))
    ((LAMBDA) (META-CALL-LAMBDA NODE))
    ;; On 9 May 82, J. Rees spent three hours tracking down a compiler bug
    ;;  which showed up while compiling MAKE-OPERATION.  It ultimately found
    ;;  its source in the following T clause, which originally just
    ;;  said (REANALYZE NODE).
    (ELSE (MEVAL NODE (CALL-FUNCTION FM))
          (CASE (STYPE (NODE-FORM (CALL-FUNCTION FM)))
            ((STATIC) (META-CALL-STATIC NODE))
            ((VARIABLE) (META-CALL-VARIABLE NODE))
            ((LAMBDA) (META-CALL-LAMBDA NODE))
            (ELSE (REANALYZE NODE))))))

;;; Fold constants and absorb INTEGRABLE-FUNCTION definitions.

(STAT-COUNTER *META-CALL-STATIC-FOLD-COUNT*
              "a CALL to a known function with constant arguments is folded")
(STAT-COUNTER *META-CALL-STATIC-INTEGRATE-COUNT*
              "a CALL to an open-codeable function is open-coded")

(DEFINE (META-CALL-STATIC NODE)
  (LET* ((FM (NODE-FORM NODE))
         (FOO (CALL-FUNCTION FM))
         (FN (NODE-FORM FOO)))
    (COND ((CGET FN 'PRIMOP)
           (META-CALL-PRIMOP NODE))
          ((AND (CGET FN 'OKAY-TO-FOLD)
                (STATIC-APPLICABLE? FN)
                (D0 ((A (CALL-ARGS FM) (CDR A)))
                    ((NULL? A) T)
                  ----
                  (OR (CONSTANT-NODE? (CAR A))
                      (RETURN NIL))
                  ---))
           (LET ((VAL (NODE-APPLY FN (CALL-ARGS FM))))
             (LET ((NEWNODE (CREATE-CONSTANT VAL)))
               (META-BEFORE-TRACE NODE)
               (SETF (NODE-PARENT NEWNODE) (NODE-PARENT NODE))
               (ERASE-ALL-NODES NODE)
               (INCR *META-CALL-STATIC-FOLD-COUNT*)
               (META-AFTER-TRACE NEWNODE 'META-CALL-STATIC)
               (REANALYZE NEWNODE))))
          (ELSE
           (LET ((Z (INTEGRABLE-FUNCTION-DEFINITION FN)))
             (COND ((NOT (EMPTY Z))
                    (META-BEFORE-TRACE NODE)
                    (LET ((NEWFOO (ALPHA-TOPLEVEL Z)))
                      (SETF (NODE-PARENT NEWFOO) NODE)
                      (SETF (CALL-FUNCTION FM) NEWFOO)
                      (ERASE-ALL-NODES FOO)
                      (PROPAGATE NEWFOO))
                    (INCR *META-CALL-STATIC-INTEGRATE-COUNT*)
                    (META-AFTER-TRACE NODE 'META-CALL-STATIC)
                    (META-EVALUATE NODE))     ;?! No!... why not?
                   (ELSE (META-CALL-STATIC-1 NODE))))))))

(DEFINE (META-CALL-STATIC-1 NODE)
  (LET ((FM (NODE-FORM NODE)))
    (MEVAL NODE (CALL-FUNCTION FM))
    (LET ((MEV (CGET (NODE-FORM (CALL-FUNCTION FM)) 'META-EVALUATOR)))
      (COND (MEV (MEV NODE))
            ((FX= (LENGTH (CALL-ARGS FM)) 2)
             (CONSIDER-REVERSING-ARGUMENTS NODE))
            (ELSE (REANALYZE NODE))))))

;;; ... CONSIDER-REVERSING-ARGUMENTS

;;; The node should be a CALL node with exactly two arguments.
;;; Prefer to have constants up front and variables trailing.
;;; For the S-1 in particular, if one argument is itself a CALL-code
;;; of two arguments which are both variables or constants, prefer that
;;; one to be second (optimizes RT usage); this is hacked simply by
;;; preferring the less complex one to be second.

(DEFINE (CONSIDER-REVERSING-ARGUMENTS NODE)
  (LET ((FM (NODE-FORM NODE)))
    (COND ((AND (CALL-NODE? NODE)
                (FX= (LENGTH (CALL-ARGS FM)) 2))
           (LET ((ARG1 (CAR (CALL-ARGS FM)))
                 (ARG2 (CADR (CALL-ARGS FM)))
                 (RP (CGET (NODE-FORM (CALL-FUNCTION FM)) 'REVPRIMOP)))
             (COND ((AND RP
;                        (OR (EQ? *TARGET-MACHINE* 'S-1)
;                            (NOT (EQ? RP T)))
                         (EQ? *TARGET-MACHINE* 'S-1)
                         ;; Shit.  What about STATICs?
                         (OR (AND (CONSTANT-NODE? ARG2)
                                  (NOT (CONSTANT-NODE? ARG1)))
                             (AND (VARIABLE-NODE? ARG1)
                                  (NOT (VARIABLE-NODE? ARG2)))
                             (AND (NOT (CONSTANT-NODE? ARG1))
                                  (NOT (VARIABLE-NODE? ARG2))
                                  (> (NODE-COMPLEXITY ARG2)
                                     (NODE-COMPLEXITY ARG1))))
                         (PASSABLE ARG1 ARG2))
                    (META-BEFORE-TRACE NODE)
                    (RPLACA (CDR (CALL-ARGS FM)) ARG1)
                    (RPLACA (CALL-ARGS FM) ARG2)
                    (CHANGE-CALL-FUNCTION NODE RP)
                    (META-AFTER-TRACE NODE 'CONSIDER-REVERSING-ARGUMENTS)))))
          (ELSE (BUGLET ((*NODE* NODE))
                     "CONSIDER-REVERSING-ARGUMENTS called on a node not a CALL of two arguments"
                     "will definitely not attempt to reverse the arguments")))
    (REANALYZE NODE)))

(BLOCK0 'REVPRIMOPS
       (WALK (LAMBDA (FOO)
               (COND ((ATOM? FOO) (CPUT FOO 'REVPRIMOP FOO))
                     ((NULL? (CDR FOO)) (ERROR))
                     (ELSE (CPUT (CAR FOO) 'REVPRIMOP (CADR FOO))
                           (CPUT (CADR FOO) 'REVPRIMOP (CAR FOO)))))
             ;; An atom means a commutative operator; a parenthesized atom
             ;;  means a non-commutative operator; and two parenthesized are
             ;;  each other's commutations.
             '(FIXNUM-ADD FIXNUM-MULTIPLY FIXNUM-LOGIOR FIXNUM-LOGAND
                          FIXNUM-LOGXOR FIXNUM-LOGEQV
                          FIXNUM-LOGNAND FIXNUM-LOGNOR
                          (FIXNUM-LOGANDC1 FIXNUM-LOGANDC2)
                          (FIXNUM-LOGORC1 FIXNUM-LOGORC2)
                          FLONUM-ADD FLONUM-MULTIPLY)))

(DEFINE (META-CALL-VARIABLE NODE)
  (LET* ((FM (NODE-FORM NODE))
         (FN (NODE-FORM (CALL-FUNCTION FM))))
    (COND ((AND (CATCH-NODE? (VARIABLE-BINDER FN))
                (NULL? (VARIABLE-WRITE-REFS FN)))
           (META-CALL-EXIT NODE))
          (ELSE
           (MEVAL NODE (CALL-FUNCTION FM))
           (REANALYZE NODE)))))

(STAT-COUNTER *META-CALL-EXIT-ELIMINATE-COUNT*
              "(CATCH X ... (X FOO) ...) becomes (CATCH X ... FOO ...)")

(DEFINE (META-CALL-EXIT NODE)
  (LET* ((FM (NODE-FORM NODE))
         (FN (NODE-FORM (CALL-FUNCTION FM))))
    (COND ((NULL? (CALL-ARGS FM))
           (WARN "no arguments in call to CATCH variable ~S"
                 "will supply () for the missing argument"
                 (VARIABLE-IDENTIFIER FN))
           (SETF (CALL-ARGS FM) (LIST (CREATE-CONSTANT '())))
           (SETF (NODE-PARENT (CAR (CALL-ARGS FM))) NODE)
           NODE)
          ((NOT (NULL? (CDR (CALL-ARGS FM))))
           (WARN "more than one argument in call to CATCH variable ~S"
                 "will remove the extra arguments"
                 (VARIABLE-IDENTIFIER FN))
           (ERASE-ALL-NODES-IN-LIST (CDR (CALL-ARGS FM)))
           (RPLACD (CALL-ARGS FM) '())
           NODE)
          ((EQ? (NODE-LEVEL NODE) (NODE-LEVEL (VARIABLE-BINDER FN)))
           (LET ((NEWNODE (CAR (CALL-ARGS FM))))
             (META-BEFORE-TRACE NODE)
             (SETF (NODE-PARENT NEWNODE) (NODE-PARENT NODE))
	     (ERASE-NODE (CALL-FUNCTION FM))
             (ERASE-NODE NODE)
             (META-AFTER-TRACE NEWNODE 'META-CALL-EXIT)
             (INCR *META-CALL-EXIT-ELIMINATE-COUNT*)
             NEWNODE))
          (ELSE (MEVAL NODE (CALL-FUNCTION FM))
                (REANALYZE NODE)))))

;;; Transform (SETTER FOO) to SET-FOO.

(STAT-COUNTER *META-SETTER-COUNT* "(SETTER FOO) becomes SET-FOO")

(DEFINE (META-CALL-SETTER NODE)
  (LET ((FM (NODE-FORM NODE)))
    (LET ((SETFN (META-GET-SETTER (CAR (CALL-ARGS FM)))))
      (COND (SETFN
             (META-BEFORE-TRACE NODE)
             (LET ((NEWNODE (IF (STATIC? SETFN)
                                (NODIFY SETFN)
                              (ALPHA-TOPLEVEL SETFN)))) ; Ugh?
               (SETF (NODE-PARENT NEWNODE) (NODE-PARENT NODE))
               (ERASE-ALL-NODES NODE)
               ;; need to propagate?  hope not.
               (INCR *META-SETTER-COUNT*)
               (META-AFTER-TRACE NEWNODE 'META-EVALUATE-CALL-SETTER)
               NEWNODE))
            (ELSE (REANALYZE NODE))))))

;;; Returns false if no setter, or a structure for a static var which
;;;  holds setter, or maybe even an s-expression; who knows. 

(DEFINE (META-GET-SETTER NODE)
  (COND ((STATIC-NODE? NODE)
         (META-GET-SETTER-1 (NODE-FORM NODE)))
        (ELSE NIL)))

(DEFINE (META-GET-SETTER-1 STATIC)
  (OR (CGET STATIC 'SETTER)
      (LET ((I (INTEGRABLE-FUNCTION-DEFINITION STATIC)))
        (COND ((EMPTY I) NIL)
              ((STATIC? I)
               (META-GET-SETTER-1 I))
              ((SYMBOL? I)
               (META-GET-SETTER-1 (FREE-LOOKUP *NAMESPACE* I NIL)))
              (ELSE NIL)))))

(CPUT 'SETTER 'META-EVALUATOR META-CALL-SETTER)

(STAT-COUNTER *META-ALWAYS-COUNT* "((ALWAYS X) ...) becomes (BLOCK ... X)")

(DEFINE (META-CALL-ALWAYSFN NODE CONST)
  (META-BEFORE-TRACE NODE)
  (LET* ((FM (NODE-FORM NODE))
         (ARGS (APPEND! (CALL-ARGS FM) (LIST (ALPHA-CONSTANT CONST))))
         (NEWNODE (NODIFY (CONS-A-BLOCK ARGS ARGS))))
    (WALK (LAMBDA (A) (SETF (NODE-PARENT A) NEWNODE)) ARGS)
    (SETF (NODE-PARENT NEWNODE) (NODE-PARENT NODE))
    (ERASE-NODE (CALL-FUNCTION FM))
    (ERASE-NODE NODE)
    (INCR *META-ALWAYS-COUNT*)
    (META-AFTER-TRACE NEWNODE 'META-CALL-ALWAYSFN)
    NEWNODE))

(LET ((FOO (LAMBDA (FN CONST)
             (CPUT FN 'META-EVALUATOR
                   (QLOZURE (CONST)
                     (LAMBDA (NODE) (META-CALL-ALWAYSFN NODE CONST)))))))
  (FOO 'TRUE T)
  (FOO 'FALSE NIL))

;;; Transform a call on a PRIMOP with constant arguments if possible.

(STAT-COUNTER *META-CALL-PRIMOP-FOLD-COUNT*
              "a CALL to a primop with constant arguments is folded")

(DEFINE (META-CALL-PRIMOP NODE)
  (LET* ((FM (NODE-FORM NODE))
         (FN (NODE-FORM (CALL-FUNCTION FM))))
    (DO ((A (CALL-ARGS FM) (CDR A))
         (CONSTP (AND (STATIC-APPLICABLE? FN)
                      (OR (AND (EQ? (CGET FN 'EFFECTS) 'NONE)
                               (EQ? (CGET FN 'AFFECTED) 'NONE))
                          (CGET FN 'OKAY-TO-FOLD)))
                 (AND CONSTP (CONSTANT-NODE? (CAR A)))))
        ((NULL? A)
         (COND (CONSTP
                (LET ((VAL (NODE-APPLY FN (CALL-ARGS FM))))
                  (LET ((NEWNODE (CREATE-CONSTANT VAL)))
                    (META-BEFORE-TRACE NODE)
                    (SETF (NODE-PARENT NEWNODE) (NODE-PARENT NODE))
                    (ERASE-ALL-NODES NODE)
                    (INCR *META-CALL-PRIMOP-FOLD-COUNT*)
                    (META-AFTER-TRACE NEWNODE 'META-CALL-PRIMOP)
                    (REANALYZE NEWNODE))))
               ((CGET FN 'META-EVALUATOR)
                => (LAMBDA (MEV) (MEV NODE)))
               ((FX= (LENGTH (CALL-ARGS FM)) 2)
                (CONSIDER-REVERSING-ARGUMENTS NODE))
               (ELSE (REANALYZE NODE)))))))

(DEFINE (NODE-APPLY STATIC NODELIST)
  (LET ((VALUES (MAPCAR (LAMBDA (A) (CONSTANT-VALUE (NODE-FORM A)))
                        NODELIST)))
    (APPLY (STATIC->PROCEDURE STATIC) VALUES)))
;;; Random utilities.

(DEFINE (CREATE-CONSTANT VALUE)
  (LET ((NODE (ALPHA-CONSTANT VALUE)))
    (SETF (NODE-LEVEL NODE) *EMPTY*)
    (REANALYZE NODE)))

(DEFINE (CHANGE-CALL-FUNCTION NODE F)
  (LET ((FM (NODE-FORM NODE))
        (FNNODE (ALPHATIZE F)))
    (SETF (CALL-FUNCTION FM) FNNODE)
    (SETF (NODE-PARENT FNNODE) NODE)
    (SETF (NODE-LEVEL FNNODE) NIL)
    (REANALYZE FNNODE)))

;;; Takes a LAMBDA node, and, if it is of the form
;;;     (LAMBDA (V1 V2 ... VN) (F V1 V2 ... VN)),
;;; returns the node for F.  Otherwise returns the original node.

(DEFINE (CONVERT-LAMBDA-TO-VARIABLE NODE)
  (LET ((FM (NODE-FORM NODE)))
    (OR (AND (EQ? (STYPE FM) 'LAMBDA)
             (EMPTY (LAMBDA-RESTVAR FM))
             (LET ((BFM (NODE-FORM (LAMBDA-BODY FM))))
               (AND (EQ? (STYPE BFM) 'CALL)
                    (LET ((FFM (NODE-FORM (CALL-FUNCTION BFM))))
                      (AND (EQ? (STYPE FFM) 'VARIABLE)
                           (NEQ? (VARIABLE-BINDER FFM) NODE)))
                    (FX= (LENGTH (CALL-ARGS BFM))
                       (LENGTH (LAMBDA-VARS FM)))
                    (D0 ((V (LAMBDA-VARS FM) (CDR V))
                         (A (CALL-ARGS BFM) (CDR A)))
                        ((NULL? V) T)
                      (IF (NOT (EQ? (NODE-FORM (CAR A)) (CAR V)))
                          (RETURN NIL)))
                    (CALL-FUNCTION BFM))))
        NODE)))

;;; This does more or less the opposite operation to
;;; CONVERT-LAMBDA-TO-VARIABLE: transforms F into
;;; (LAMBDA (V1 V2 ... VN) (F V1 V2 ... VN)).  This is useful for
;;; yielding things like (PRIMOP CAR) for value, and for coercing expressions
;;; to LAMBDA-expressions in contexts where a LAMBDA is required (e.g.
;;; the third argument to FIXNUM-DIV2).

(DEFINE (CONVERT-VARIABLE-TO-LAMBDA NODE ARGSPECTRUM)
  (COND ((OR (NULL? ARGSPECTRUM)
             (NOT (FX= (CAR ARGSPECTRUM) (CDR ARGSPECTRUM))))
         NODE)
        (ELSE
         (DO ((V '() (CONS (GENERATE-SYMBOL 'ARG) V))
              (I 0 (FX+ I 1)))
             ((FX>= I (CAR ARGSPECTRUM))
              (ALPHATIZE `(LAMBDA ,V (,(INT NODE) ,@V))))))))

(define (meta-source-rewrite node rewriter)
  (meta-before-trace node)
  (let* ((vals (map (lambda (val) (int val))
		    (call-args (node-form node))))
	 (newnode (alpha-toplevel (rewriter vals))))
    (setf (node-parent newnode) (node-parent node))
    (setf (node-level newnode) (node-level node))
    (erase-node node)
    (propagate newnode)
    (meta-after-trace newnode 'meta-source-rewrite)
    newnode))

(define-local-syntax (define-source-rewrite pat . body)
  `(cput ',(car pat)
	 'meta-evaluator
	 (lambda (node)
	   (meta-source-rewrite node
				(lambda (%%args%%)
				  (destructure ((,(cdr pat) %%args%%))
				    ,@body))))))

(define-source-rewrite (values . vals)
  (let ((nvals (length vals)))
    (cond ((fx= nvals 1) (car vals))
	  (else
	   (let ((temps (map (lambda (val)
			       (ignore val)
			       (generate-symbol 'values))
			     vals)))
	     (if (fx> nvals *values-vector-length*)
		 (warn "too many values"
		       "object code will lose"))
	     `((lambda ,temps
		 ,@(do ((i 0 (fx+ i 1))
			(l temps (cdr l))
			(s '() (cons `(set-extend-elt-fixed *values-vector*
							    ,i
							    ,(car l))
				     s)))
		       ((null? l) (reverse! s)))
		 '(,nvals . **multiple-values**))
	       ,@vals))))))
