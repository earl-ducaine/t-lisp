(HERALD ANALYZE
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;; Analysis pushes information UP the tree during optimization.
       
(DEFINE (ANALYZE NODE REDO OPT)
  (ENV-ANALYZE NODE REDO)
  (IF OPT (EFFS-ANALYZE NODE REDO))
  (COMPLEXITY-ANALYZE NODE REDO)
  NODE)
       
(DEFINE (REANALYZE NODE)
  (DECLARE (SPECIAL *REANALYZE*))
  (ANALYZE NODE *REANALYZE* T)
  (SETF (NODE-METAP NODE) T)
  NODE)
       
;;; Propagation pushes information DOWN the tree during optimization.
       
(DEFINE (PROPAGATE NODE)
  (LEVEL-PROPAGATE NODE)
  NODE)
       
(DEFINE (REPROPAGATE NODE)
  (COND ((NULL? (NODE-PARENT NODE))
         (BUG "attempt to REPROPAGATE the root node"
              "will do it from the root node down, but I dunno...")
         (PROPAGATE NODE))
        (ELSE (PROPAGATE (NODE-PARENT NODE)))))
;;;; Environment analysis

;;; Fill in the REFS and SETQS  slots of nodes.
;;; Add to the READ-REFS and WRITE-REFS slots of variables.
;;; Also verify the PARENT slot of each node for error-checking.

(DEFINE (ENV-ANALYZE NODE REDOTHIS)
  (IF (OR REDOTHIS (EMPTY (NODE-REFS NODE)))
      (NODE-DISPATCH ENV-ANALYZE NODE (IF (EQ? REDOTHIS 'ALL) 'ALL))))

(DEFDISPATCH ENV-ANALYZE CONSTANT (NODE FM REDO)
  (IGNORE FM REDO)
  (SETF (NODE-REFS NODE) '())
  (SETF (NODE-SETQS NODE) '()))

(DEFDISPATCH ENV-ANALYZE STATIC (NODE FM REDO)
  (IGNORE FM REDO)
  (SETF (NODE-REFS NODE) '())
  (SETF (NODE-SETQS NODE) '()))

(DEFDISPATCH ENV-ANALYZE VARIABLE (NODE FM REDO)
  (IGNORE REDO)
  (SETF (NODE-REFS NODE) '())
  (SETF (NODE-SETQS NODE) '())
  (SETF (VARIABLE-EVER-READ-REFD FM) T)
  (SETF (VARIABLE-READ-REFS FM)
        (ADJOIN NODE (VARIABLE-READ-REFS FM)))
  (SETF (NODE-REFS NODE) (LIST FM)))

(DEFDISPATCH ENV-ANALYZE SETQ (NODE FM REDO)
  (LET ((VAR (SETQ-VAR FM))
        (BODY (SETQ-BODY FM)))
    (ENV-ANALYZE BODY REDO)
    (NODE-PARENT-CHECK BODY NODE)
    (SETF (NODE-REFS NODE) (NODE-REFS BODY))
    (COND ((STATIC? VAR)
           (SETF (NODE-SETQS NODE) (NODE-SETQS BODY)))
          (ELSE
           (SETF (VARIABLE-EVER-WRITE-REFD VAR) T)
           (SETF (VARIABLE-WRITE-REFS VAR)
                 (ADJOIN NODE (VARIABLE-WRITE-REFS VAR)))
           (SETF (NODE-SETQS NODE) (ADJOIN VAR (NODE-SETQS BODY)))))))

(DEFDISPATCH ENV-ANALYZE LAMBDA (NODE FM REDO)
  (LET ((BODY (LAMBDA-BODY FM)))
    (ENV-ANALYZE BODY REDO)
    (NODE-PARENT-CHECK BODY NODE)
    (DO ((V (REVERSE (LAMBDA-VARS FM)) (CDR V))
         (REFS (NODE-REFS BODY))
         (SETQS (NODE-SETQS BODY)))
        ((NULL? V)
         (LET ((RVAR (LAMBDA-RESTVAR FM)))
           (COND ((AND (NOT (EMPTY RVAR)) RVAR)
                  (SETF (NODE-REFS NODE) (SETREMQ RVAR REFS))
                  (SETF (NODE-SETQS NODE) (SETREMQ RVAR SETQS))
                  (CHECK-BLISS RVAR))
                 (ELSE
                  (SETF (NODE-REFS NODE) REFS)
                  (SETF (NODE-SETQS NODE) SETQS)))
           (WALK CHECK-BLISS (LAMBDA-VARS FM))))
      ----
      (COND ((CAR V)
             (SETQ REFS (SETREMQ (CAR V) REFS))
             (SETQ SETQS (SETREMQ (CAR V) SETQS))))
      ---)))

(DEFDISPATCH ENV-ANALYZE CATCH (NODE FM REDO)
  (LET ((BODY (CATCH-BODY FM))
        (VAR  (CATCH-VAR FM)))
    (ENV-ANALYZE BODY REDO)
    (CHECK-BLISS VAR)
    (SETF (NODE-REFS NODE) (SETREMQ VAR (NODE-REFS BODY)))
    (SETF (NODE-SETQS NODE) (SETREMQ VAR (NODE-SETQS BODY)))))

;;; Should this do error checking for unreferenced vars
;;; set in the LABELS binding spec?? -nia
;;; Yeah, just like the above code for LAMBDA.  -JAR

(DEFDISPATCH ENV-ANALYZE LABELS (NODE FM REDO)
  (DO ((VALS (LABELS-VALS FM) (CDR VALS))
       (REFS '() (UNION REFS (NODE-REFS (CAR VALS))))
       (SETQS '() (UNION SETQS (NODE-SETQS (CAR VALS)))))
      ((NULL? VALS)
       (SETF (LABELS-REFS FM) REFS)     ; for looping - see TARGETIZE-LABELS
       (SETF (LABELS-SETQS FM) SETQS)
       (LET ((BODY (LABELS-BODY FM)))
         (ENV-ANALYZE BODY REDO)
         (NODE-PARENT-CHECK BODY NODE)
         (SETF (NODE-REFS NODE) (SETDIFF (UNION REFS (NODE-REFS BODY))
                                         (LABELS-VARS FM)))
         (SETF (NODE-SETQS NODE) (SETDIFF (UNION SETQS (NODE-SETQS BODY))
                                          (LABELS-VARS FM)))
         (WALK CHECK-BLISS (LABELS-VARS FM))
         ))
    ----
    (ENV-ANALYZE (CAR VALS) REDO)
    (NODE-PARENT-CHECK (CAR VALS) NODE)
    ---))

(DEFDISPATCH ENV-ANALYZE BLOCK (NODE FM REDO)
  (ENV-UNION NODE (BLOCK-ARGS FM) REDO))

(DEFDISPATCH ENV-ANALYZE IF (NODE FM REDO)
  (ENV-UNION NODE
             (LIST (IF-PRED FM) (IF-CON FM) (IF-ALT FM))
             REDO))

(DEFDISPATCH ENV-ANALYZE CASE (NODE FM REDO)
  (ENV-UNION NODE
             (CONS* (CASE-KEY FM)
                    (CASE-ELSE FM)
                    (MAP CDR (CASE-CLAUSES FM)))
             REDO))

(DEFDISPATCH ENV-ANALYZE CALL (NODE FM REDO)
  (ENV-UNION NODE
             (CONS (CALL-FUNCTION FM)
                   (CALL-ARGS FM))
             REDO))

(DEFINE (ENV-UNION NODE THINGS REDO)
  (DO ((A THINGS (CDR A))
       (REFS '() (UNION REFS (NODE-REFS (CAR A))))
       (SETQS '() (UNION SETQS (NODE-SETQS (CAR A)))))
      ((NULL? A)
       (SETF (NODE-REFS NODE) REFS)
       (SETF (NODE-SETQS NODE) SETQS))
    ----
    (ENV-ANALYZE (CAR A) REDO)
    (NODE-PARENT-CHECK (CAR A) NODE)
    ---))

(DEFINE (CHECK-BLISS VAR)               ;Where ignorance is...
  (COND ((AND VAR (NEQ? (VARIABLE-EVER-READ-REFD VAR) 'IGNORABLE))
         (COND ((NOT (VARIABLE-EVER-READ-REFD VAR))
                (COND ((NOT (VARIABLE-EVER-WRITE-REFD VAR))
                       (IF (AND (NEQ? (VARIABLE-EVER-READ-REFD VAR) 'IGNORED)
                                (NEQ? (VARIABLE-IDENTIFIER VAR) 'IGNORED))
                           (MENTION "unreferenced variable ~S"
                                    "none"
                                    (VARIABLE-IDENTIFIER VAR))))
                      (ELSE
                       (MENTION "variable ~S is SETQ'd but never referenced"
                                "none"
                                (VARIABLE-IDENTIFIER VAR))))
                (SETF (VARIABLE-EVER-READ-REFD VAR) 'WARNED))
               ((AND (EQ? (VARIABLE-EVER-READ-REFD VAR) 'IGNORED)
                     (VARIABLE-READ-REFS VAR))
                (MENTION "ignored variable ~S is referenced"
                         "none"
                         (VARIABLE-IDENTIFIER VAR))
                (SETF (VARIABLE-EVER-READ-REFD VAR) 'WARNED))
               ))))

;;;; Side effects analysis

;;; The side effects analysis does not include the effects of SETQ's on
;;; lexical variables, because this information can be computed from the
;;; results of the environment analysis.

;;; Fill in EFFECTS and AFFECTED for each node.

;;; Side effect names include SETQ, CONS, SET-CAR, SET-CDR.

(DEFINE (EUNION A B)
  (COND ((EQ? A 'NONE) B)
        ((EQ? B 'NONE) A)
        ((OR (EQ? A 'ANY) (EQ? B 'ANY)) 'ANY)
        (ELSE (UNION A B))))

(DEFINE (EFFS-ANALYZE NODE REDOTHIS)
  (IF (OR REDOTHIS (EMPTY (NODE-EFFECTS NODE)))
      (NODE-DISPATCH EFFS-ANALYZE NODE (AND (EQ? REDOTHIS 'ALL) 'ALL))))

(DEFDISPATCH EFFS-ANALYZE CONSTANT (NODE FM REDO)
  (IGNORE FM REDO)
  (SETF (NODE-EFFECTS NODE) 'NONE)
  (SETF (NODE-AFFECTED NODE) 'NONE))

(DEFDISPATCH EFFS-ANALYZE STATIC (NODE FM REDO)
  (IGNORE REDO)
  (SETF (NODE-EFFECTS NODE) 'NONE)
  (SETF (NODE-AFFECTED NODE)
        (COND ((OR (CGET FM 'PRIMOP) (CGET FM 'DEFINED))
               'NONE)
              (ELSE
               '(SETQ)))))

(DEFDISPATCH EFFS-ANALYZE VARIABLE (NODE FM REDO)
  (IGNORE FM REDO)
  (SETF (NODE-EFFECTS NODE) 'NONE)
  (SETF (NODE-AFFECTED NODE) 'NONE))

(DEFDISPATCH EFFS-ANALYZE SETQ (NODE FM REDO)
  (LET ((BODY (SETQ-BODY FM)))
    (EFFS-ANALYZE BODY REDO)
    (COND ((STATIC? (SETQ-VAR FM))
           (SETF (NODE-EFFECTS NODE)
                 (EUNION '(SETQ) (NODE-EFFECTS BODY))))
          (ELSE
           ;; Lexical SETQ side effects are dealt with separately.
           (SETF (NODE-EFFECTS NODE) (NODE-EFFECTS BODY))))
    (SETF (NODE-AFFECTED NODE) (NODE-AFFECTED BODY))))

(DEFDISPATCH EFFS-ANALYZE LAMBDA (NODE FM REDO)
  (EFFS-ANALYZE (LAMBDA-BODY FM) REDO)
  (SETF (LAMBDA-EFFECTS FM) (NODE-EFFECTS (LAMBDA-BODY FM)))
  (SETF (LAMBDA-AFFECTED FM) (NODE-AFFECTED (LAMBDA-BODY FM)))
  (SETF (NODE-EFFECTS NODE) '(CONS))
  (SETF (NODE-AFFECTED NODE) 'NONE))

(DEFDISPATCH EFFS-ANALYZE CATCH (NODE FM REDO)
  (EFFS-ANALYZE (CATCH-BODY FM) REDO)
  (SETF (NODE-EFFECTS NODE)
        (EUNION '(CONS) (NODE-EFFECTS (CATCH-BODY FM))))
  (SETF (NODE-AFFECTED NODE) (NODE-AFFECTED (CATCH-BODY FM))))

(DEFDISPATCH EFFS-ANALYZE LABELS (NODE FM REDO)
  (EFFS-ANALYZE (LABELS-BODY FM) REDO)
  (WALK (LAMBDA (VALNODE)
          (EFFS-ANALYZE VALNODE REDO)
          (SETF (NODE-EFFECTS NODE)
                (EUNION (NODE-EFFECTS VALNODE)
                        (NODE-EFFECTS (LABELS-BODY FM))))
          (SETF (NODE-AFFECTED NODE)
                (EUNION (NODE-AFFECTED VALNODE)
                        (NODE-AFFECTED (LABELS-BODY FM)))))
        (LABELS-VALS FM)))

(DEFDISPATCH EFFS-ANALYZE BLOCK (NODE FM REDO)
  (DO ((A (BLOCK-ARGS FM) (CDR A))
       (EFFS '() (EUNION EFFS (NODE-EFFECTS (CAR A))))
       (AFFD '() (EUNION AFFD (NODE-AFFECTED (CAR A)))))
      ((NULL? A)
       (SETF (NODE-EFFECTS NODE) EFFS)
       (SETF (NODE-AFFECTED NODE) AFFD))
    ----
    (EFFS-ANALYZE (CAR A) REDO)
    ---))

(DEFDISPATCH EFFS-ANALYZE IF (NODE FM REDO)
  (EFFS-ANALYZE (IF-PRED FM) REDO)
  (EFFS-ANALYZE (IF-CON FM) REDO)
  (EFFS-ANALYZE (IF-ALT FM) REDO)
  (SETF (NODE-EFFECTS NODE)
        (EUNION (NODE-EFFECTS (IF-PRED FM))
                (EUNION (NODE-EFFECTS (IF-CON FM))
                        (NODE-EFFECTS (IF-ALT FM)))))
  (SETF (NODE-AFFECTED NODE)
        (EUNION (NODE-AFFECTED (IF-PRED FM))
                (EUNION (NODE-AFFECTED (IF-CON FM))
                        (NODE-AFFECTED (IF-ALT FM))))))

(DEFDISPATCH EFFS-ANALYZE CASE (NODE FM REDO)
  (EFFS-ANALYZE (CASE-KEY FM) REDO)
  (EFFS-ANALYZE (CASE-ELSE FM) REDO)
  (DO ((C (CASE-CLAUSES FM) (CDR C))
       (EFFS (EUNION (NODE-EFFECTS (CASE-KEY FM))
                     (NODE-EFFECTS (CASE-ELSE FM)))
             (EUNION EFFS (NODE-EFFECTS (CDAR C))))
       (AFFD (EUNION (NODE-AFFECTED (CASE-KEY FM))
                     (NODE-AFFECTED (CASE-ELSE FM)))
             (EUNION AFFD (NODE-AFFECTED (CDAR C)))))
      ((NULL? C)
       (SETF (NODE-EFFECTS NODE) EFFS)
       (SETF (NODE-AFFECTED NODE) AFFD))
    ----
    (EFFS-ANALYZE (CDAR C) REDO)
    ---))

(DEFDISPATCH EFFS-ANALYZE CALL (NODE FM REDO)
  (DO ((A (CALL-ARGS FM) (CDR A))
       (EFFS '() (EUNION EFFS (NODE-EFFECTS (CAR A))))
       (AFFD '() (EUNION AFFD (NODE-AFFECTED (CAR A))))
       (FN (NODE-FORM (CALL-FUNCTION FM))))
      ((NULL? A)
       (CASE (STYPE FN)
         ((STATIC)
          (EFFS-ANALYZE (CALL-FUNCTION FM) REDO)
          (SETF (NODE-EFFECTS NODE)
                (EUNION EFFS (OR (CGET FN 'EFFECTS)
                                 'ANY)))
          (SETF (NODE-AFFECTED NODE)
                (EUNION AFFD (OR (CGET FN 'AFFECTED)
                                 'ANY))))
         ((LAMBDA)
          (EFFS-ANALYZE (LAMBDA-BODY FN) REDO)
          (EFFS-ANALYZE (CALL-FUNCTION FM) 'ONCE)
          (SETF (NODE-EFFECTS NODE)
                (EUNION EFFS (LAMBDA-EFFECTS FN)))
          (SETF (NODE-AFFECTED NODE)
                (EUNION AFFD (LAMBDA-AFFECTED FN))))
         (ELSE
          (EFFS-ANALYZE (CALL-FUNCTION FM) REDO)
          (SETF (NODE-EFFECTS NODE) 'ANY)
          (SETF (NODE-AFFECTED NODE) 'ANY))))
    ----
    (EFFS-ANALYZE (CAR A) REDO)
    ---))

;;;; Complexity analysis

;;; Fill in the COMPLEXITY slot in each node.
;;; Returns the complexity as its value.

(DEFINE (COMPLEXITY-ANALYZE NODE REDOTHIS)
  (IF (OR REDOTHIS (EMPTY (NODE-COMPLEXITY NODE)))
      (SETF (NODE-COMPLEXITY NODE)
            (NODE-DISPATCH COMPLEXITY-ANALYZE NODE
                           (AND (EQ? REDOTHIS 'ALL) 'ALL))))
  (NODE-COMPLEXITY NODE))

(DEFDISPATCH COMPLEXITY-ANALYZE CONSTANT (NODE FM REDO)
  (IGNORE NODE FM REDO)
  2)

(DEFDISPATCH COMPLEXITY-ANALYZE STATIC (NODE FM REDO)
  (IGNORE NODE FM REDO)
  6)

(DEFDISPATCH COMPLEXITY-ANALYZE VARIABLE (NODE FM REDO)
  (IGNORE NODE FM REDO)
  2)

(DEFDISPATCH COMPLEXITY-ANALYZE SETQ (NODE FM REDO)
  (IGNORE NODE)
  (FX+ 4 (COMPLEXITY-ANALYZE (SETQ-BODY FM) REDO)))

(DEFDISPATCH COMPLEXITY-ANALYZE CATCH (NODE FM REDO)
  (IGNORE NODE)
  (FX+ 6. (COMPLEXITY-ANALYZE (CATCH-BODY FM) REDO)))

(DEFDISPATCH COMPLEXITY-ANALYZE LAMBDA (NODE FM REDO)
  (IGNORE NODE)
  (FX+ (COMPLEXITY-ANALYZE (LAMBDA-BODY FM) REDO)
     64.))

(DEFDISPATCH COMPLEXITY-ANALYZE LABELS (NODE FM REDO)
  (IGNORE NODE)
  (DO ((A (LABELS-VALS FM) (CDR A))
       (Q 0 (FX+ Q (COMPLEXITY-ANALYZE (CAR A) REDO))))
      ((NULL? A)
       (FX+ Q (COMPLEXITY-ANALYZE (LABELS-BODY FM) REDO)))))

(DEFDISPATCH COMPLEXITY-ANALYZE BLOCK (NODE FM REDO)
  (IGNORE NODE)
  (DO ((A (BLOCK-ARGS FM) (CDR A))
       (Q 0 (FX+ Q (COMPLEXITY-ANALYZE (CAR A) REDO))))
      ((NULL? A) Q)))

(DEFDISPATCH COMPLEXITY-ANALYZE IF (NODE FM REDO)
  (IGNORE NODE)
  (FX+ (FX+ (COMPLEXITY-ANALYZE (IF-PRED FM) REDO)  ; bogus early T +
            (COMPLEXITY-ANALYZE (IF-CON FM) REDO))
       (FX+ (COMPLEXITY-ANALYZE (IF-ALT FM) REDO)
            8)))

(DEFDISPATCH COMPLEXITY-ANALYZE CASE (NODE FM REDO)
  (IGNORE NODE)
  (DO ((C (CASE-CLAUSES FM) (CDR C))
       (Q (FX+ (FX+ (COMPLEXITY-ANALYZE (CASE-KEY FM) REDO) ; bogus +
                    (COMPLEXITY-ANALYZE (CASE-ELSE FM) REDO))
             8)
          (FX+ (FX+ (FX* (LENGTH (CAAR C))
                         (COND ((FIXNUM? (CAAAR C)) 4)
                               (ELSE 8)))
                (COMPLEXITY-ANALYZE (CDAR C) REDO))
             Q)))
      ((NULL? C) Q)))

(DEFDISPATCH COMPLEXITY-ANALYZE CALL (NODE FM REDO)
  (IGNORE NODE)
  (DO ((A (CALL-ARGS FM) (CDR A))
       (Q 0 (FX+ Q (COMPLEXITY-ANALYZE (CAR A) REDO))))
      ((NULL? A)
       (LET ((FN (NODE-FORM (CALL-FUNCTION FM))))
         (CASE (STYPE FN)
           ((STATIC)
            (COND ((CGET FN 'PRIMOP)
                   (FX+ Q (OR (CGET FN 'COMPLEXITY) 4)))
                  (ELSE (FX+ Q 46.))))
           ((VARIABLE)
            (FX+ Q (FX+ 40. (COMPLEXITY-ANALYZE (CALL-FUNCTION FM) REDO))))
           ((LAMBDA)
            (LET ((N (COMPLEXITY-ANALYZE (LAMBDA-BODY FN) REDO)))
              (SETF (NODE-COMPLEXITY (CALL-FUNCTION FM)) N)
              (FX+ N Q)))
           (ELSE
            (FX+ Q (FX+ 40. (COMPLEXITY-ANALYZE (CALL-FUNCTION FM) REDO)))))))))


;;;; Evaluation level propagation

;;; A node for which (EQ? NODE (NODE-LEVEL NODE)) is necessarily one of:
;;; 
;;; The key in a CASE, or the predicate of an IF.
;;; The function or an argument in a CALL.
;;; A value in a LABELS.
;;; The source of a SETQ.
;;; Any but the last arg of a BLOCK.
;;; The body of a non-JUMP strategy LAMBDA.
;;; 
;;; A node for which (NOT (EQ? NODE (NODE-LEVEL NODE))) is necessarily one of:
;;; 
;;; An arm of a CASE or IF.
;;; The body of a LABELS, CALL-LAMBDA, or CATCH.
;;; The last arg of a BLOCK.
;;; The body of a JUMP-strategy LAMBDA.
;;; The argument of a lexical THROW.

(DEFINE (LEVEL-PROPAGATE NODE)
  (D0 ((N NODE (NODE-PARENT N))
        (ONODE NODE N))
       ((NOT (EMPTY (NODE-LEVEL N)))
        (LEVEL-PROPAGATE-1 N))
    ----
    (COND ((NULL? N)
           (BUG "~S has no initial LEVEL from which to propagate (~S started it)"
                "I'll just forget all about the propagation"
                ONODE NODE)
           (RETURN NIL)))
    ---))

(DEFINE (LEVEL-PROPAGATE-1 NODE)
  (NODE-DISPATCH LEVEL-PROPAGATE NODE))

(DEFDISPATCH LEVEL-PROPAGATE CONSTANT (NODE FM)
  (IGNORE NODE FM))

(DEFDISPATCH LEVEL-PROPAGATE STATIC (NODE FM)
  (IGNORE NODE FM))

(DEFDISPATCH LEVEL-PROPAGATE VARIABLE (NODE FM)
  (IGNORE NODE FM))

(DEFDISPATCH LEVEL-PROPAGATE SETQ (NODE FM)
  (IGNORE NODE)
  (LPROP (SETQ-BODY FM) NIL))

;;; Let B be the BODY of a lambda-node.  Then (NOT (EQ? B (NODE-LEVEL B))) iff
;;; the lambda's compilation strategy will be JUMP or LET.

(DEFDISPATCH LEVEL-PROPAGATE LAMBDA (NODE FM)
  (IGNORE NODE)
  (LPROP (LAMBDA-BODY FM) NIL))

;;; Let A be an argument in a CALL.  Then (NOT (EQ? A (NODE-LEVEL A))) iff
;;; the strategy of the call will be EXIT.
;;; (-- but we get screwed if the variable is closure-refped!)

(DEFDISPATCH LEVEL-PROPAGATE CATCH (NODE FM)
  (LET ((LEVEL (NODE-LEVEL NODE))
        (VAR (CATCH-VAR FM)))
    (LPROP (CATCH-BODY FM) LEVEL)
    (ENV-ANALYZE NODE NIL)              ; Make sure analysis is there
    (COND ((NULL? (VARIABLE-WRITE-REFS VAR))
           (WALK (LAMBDA (R)
                   (LET ((P (NODE-PARENT R)))
                     (COND ((AND (CALL-NODE? P)
                                 (EQ? R (CALL-FUNCTION (NODE-FORM P))))
                            (LPROP (CAR (CALL-ARGS (NODE-FORM P))) LEVEL)))))
                 (VARIABLE-READ-REFS VAR))))))

;;; Our current crude strategy is as follows:
;;; Labels-functions can be JUMP strategy only if all calls to all such
;;; functions are at "acceptable" level.  An acceptable level is defined to be
;;; a level which is either the level of the LABELS itself or of the body of
;;; one of the labels-functions.

;;; I'm not at all convinced that this really works.

(DEFINE *losing-labels-break* NIL)

(DEFDISPATCH LEVEL-PROPAGATE LABELS (NODE FM)
  (LET ((LEVEL (NODE-LEVEL NODE)))
    (LPROP (LABELS-BODY FM) LEVEL)
    (WALK (LAMBDA (A) (LPROP A NIL))
          (LABELS-VALS FM))
    (ENV-ANALYZE NODE NIL)              ; Make sure analysis is there
    (DO ((A (LABELS-VALS FM) (CDR A))
         (L '() (COND ((LAMBDA-NODE? (CAR A))
                       (CONS (LAMBDA-BODY (NODE-FORM (CAR A))) L))
                      (ELSE L))))
        ((NULL? A)
         (LET ((ACCEPTABLE-LEVELS (CONS LEVEL L)))
           (D0 ((A (LABELS-VALS FM) (CDR A))
                (V (LABELS-VARS FM) (CDR V)))
               ((NULL? V)               ; won completely
;;              (break labels-is-winning)
                (WALK (LAMBDA (A)
                        (COND ((LAMBDA-NODE? A)
                               ;; Set to EMPTY to avoid repropagation screwups.
                               (SETF (NODE-LEVEL A) *EMPTY*)
                               (LPROP (LAMBDA-BODY (NODE-FORM A)) LEVEL))))
                      (LABELS-VALS FM)))
             ;; Losing Maclisp has no lexical throw.
             (COND ((AND (LAMBDA-NODE? (CAR A))
                         (OR (VARIABLE-WRITE-REFS (CAR V))
                             (D0 ((R (VARIABLE-READ-REFS (CAR V)) (CDR R)))
                                  ((NULL? R) NIL)       ; winning so far
                               (LET ((P (NODE-PARENT (CAR R))))
                                 (IF (OR (NOT (CALL-NODE? P))
                                         (NEQ? (CAR R)
                                               (CALL-FUNCTION (NODE-FORM P)))
                                         (NOT (MEMQ (NODE-LEVEL P)
                                                    ACCEPTABLE-LEVELS)))
                                     (RETURN 'LOSE))))))
                    (if *losing-labels-break*
                        (break labels-is-losing))
                    (RETURN 'LOSE)))))))))

(DEFDISPATCH LEVEL-PROPAGATE BLOCK (NODE FM)
  (WALKCDR (LAMBDA (A)
             (COND ((CDR A) (LPROP (CAR A) NIL))
                   (ELSE (LPROP (CAR A) (NODE-LEVEL NODE)))))
           (BLOCK-ARGS FM)))

(DEFDISPATCH LEVEL-PROPAGATE IF (NODE FM)
  (LET ((LEVEL (NODE-LEVEL NODE)))
    (LPROP (IF-PRED FM) NIL)
    (LPROP (IF-CON FM) LEVEL)
    (LPROP (IF-ALT FM) LEVEL)))

(DEFDISPATCH LEVEL-PROPAGATE CASE (NODE FM)
  (LET ((LEVEL (NODE-LEVEL NODE)))
    (LPROP (CASE-KEY FM) NIL)
    (LPROP (CASE-ELSE FM) LEVEL)
    (WALK (LAMBDA (C) (LPROP (CDR C) LEVEL)) (CASE-CLAUSES FM))))

(DEFDISPATCH LEVEL-PROPAGATE CALL (NODE FM)
  (LET ((LEVEL (NODE-LEVEL NODE))
        (FN (NODE-FORM (CALL-FUNCTION FM))))
    (COND ((AND (EQ? (STYPE FN) 'VARIABLE)
                (CATCH-NODE? (VARIABLE-BINDER FN))
                (NULL? (VARIABLE-WRITE-REFS FN)))
           ;; Jump-strategy THROW.
           ;; This is sort of redundant with the above code for CATCH.
           (LPROP (CALL-FUNCTION FM) NIL)       ; Need in case we must back out
           (LPROP (CAR (CALL-ARGS FM)) (NODE-LEVEL (VARIABLE-BINDER FN))))
          ((AND (EQ? (STYPE FN) 'LAMBDA)
                (EMPTY (LAMBDA-RESTVAR FN)))
           ;; LET.  Find levels in body first.
           (LPROP (LAMBDA-BODY (NODE-FORM (CALL-FUNCTION FM))) LEVEL)
           ;; Now handle arguments which might be JUMP-strategy LAMBDA's.
           ;; (Worry about LABELS someday?)
           (ENV-ANALYZE (CALL-FUNCTION FM) NIL)
           (WALK (LAMBDA (V ARG)
                   (COND ((AND (LAMBDA-NODE? ARG)
                               (NOT (NULL? V))
                               (NULL? (VARIABLE-WRITE-REFS V))
                               (D0 ((R (VARIABLE-READ-REFS V) (CDR R)))
                                   ((NULL? R) T)
                                 ----
                                 (LET ((RP (NODE-PARENT (CAR R))))
                                   (IF (OR (NOT (CALL-NODE? RP))
                                           (NEQ? (CALL-FUNCTION (NODE-FORM RP))
                                                 (CAR R))
                                           (NEQ? (NODE-LEVEL RP) LEVEL))
                                       (RETURN NIL)))
                                 ---))
                          (LPROP (LAMBDA-BODY (NODE-FORM ARG)) LEVEL))
                         (ELSE (LPROP ARG NIL))))
                 (LAMBDA-VARS FN)
                 (CALL-ARGS FM)))
          (ELSE
           (LPROP (CALL-FUNCTION FM) NIL)
           (WALK (LAMBDA (A) (LPROP A NIL)) (CALL-ARGS FM))))))

;;; If LEVEL is NIL, then this node is "at its own level": its value is
;;; actually used by its parent rather than just passed upwards for use by its
;;; parent's parent.

(DEFINE (LPROP NODE LEVEL)
  (COND ((OR (EMPTY (NODE-LEVEL NODE))
             (NOT (EQ? (NODE-LEVEL NODE) LEVEL)))
         (SETF (NODE-LEVEL NODE) (OR LEVEL NODE))
         (LEVEL-PROPAGATE-1 NODE))))
