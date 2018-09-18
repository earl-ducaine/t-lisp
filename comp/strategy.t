(HERALD STRATEGY
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Strategy annotation

;;; Experimental utilities.  Useful?

(DEFINE (ALL-REFERENCES-ARE-CALLS? V)
  (EVERY (LAMBDA (R)
           (LET ((RP (NODE-FORM (NODE-PARENT R))))
             (AND (EQ? (STYPE RP) 'CALL)
                  (EQ? (CALL-FUNCTION RP) R))))
         (VARIABLE-READ-REFS V)))

;;; Maybe the following ought to be a slot in a NODE.

(DEFINE (NODE-VAR NODE)
  (LET ((P (NODE-PARENT NODE)))
    (COND ((NULL? P) NIL)
          ((AND (CALL-NODE? P) (LAMBDA-NODE? (CALL-FUNCTION (NODE-FORM P))))
           (D0 ((A (CALL-ARGS (NODE-FORM P)) (CDR A))
                (V (LAMBDA-VARS (NODE-FORM (CALL-FUNCTION (NODE-FORM P))))
                   (CDR V)))
               ((NULL? A) NIL)
             (COND ((EQ? (CAR A) NODE) (RETURN (CAR V))))))
          ((LABELS-NODE? P)
           (D0 ((A (LABELS-VALS (NODE-FORM P)) (CDR A))
                (V (LABELS-VARS (NODE-FORM P)) (CDR V)))
               ((NULL? A) NIL)
             (COND ((EQ? (CAR A) NODE) (RETURN (CAR V))))))
          (ELSE NIL))))

;;; The following utility is used by CANONICALIZE and by REP-ANNOTATE.

(DEFINE (PRIMOP-PREDICATE-CALL? NODE)
  (AND (CALL-NODE? NODE)
       (STATIC-NODE? (CALL-FUNCTION (NODE-FORM NODE)))
       (CGET (NODE-FORM (CALL-FUNCTION (NODE-FORM NODE))) 'PRIMOP-PREDICATE)))

;;; DEFINE-INTEGRABLE also uses this.

(DEFINE (ENTITY-NODE? NODE)
  (AND (CALL-NODE? NODE)
       (STATIC-NODE? (CALL-FUNCTION (NODE-FORM NODE)))
       (CGET (NODE-FORM (CALL-FUNCTION (NODE-FORM NODE))) 'ENTITY-CONSTRUCTOR)))

(CPUT '%OBJECT 'ENTITY-CONSTRUCTOR T)

(DEFINE (HANDLER-NODE? NODE)
  (LET ((ENODE (NODE-PARENT (NODE-LEVEL NODE))))
    (AND ENODE
         (ENTITY-NODE? ENODE)
         (EQ? (NODE-LEVEL NODE) (CADR (CALL-ARGS (NODE-FORM ENODE)))))))

(DEFINE (METHOD-NODE? NODE)
  (LET ((HNODE (NODE-PARENT (NODE-LEVEL NODE))))
    (AND HNODE
         (LAMBDA-NODE? HNODE)
         (HANDLER-NODE? HNODE))))

;;;; Binding annotation

;;; Fill in the STRATEGY slot for LAMBDA, LABELS, CATCH, and CALL structures.
;;; Fill in the KNOWN-FUNCTION and CLOSURE-REFS slots for VARIABLE structures.
;;; Consistency check: make sure that the LEVEL slot of every node is filled in.

;;; A better name might be STRATEGY-ANNOTATE.

(DEFINE *GENERATE-TAIL-RECURSIVE-CALLS?* T)

(DEFINE (BIND-ANNOTATE NODE)
  (COND ((OR (EMPTY (NODE-LEVEL NODE))
             (NULL? (NODE-LEVEL NODE)))
         (BUGLET ((*NODE* NODE))
                 "NODE-LEVEL of a node isn't filled in!"
                 "will set it to be the node itself")
         (SETF (NODE-LEVEL NODE) NODE)))
  (NODE-DISPATCH BIND-ANNOTATE NODE))

(DEFDISPATCH BIND-ANNOTATE CONSTANT (NODE FM))

(DEFDISPATCH BIND-ANNOTATE STATIC (NODE FM))

(DEFDISPATCH BIND-ANNOTATE VARIABLE (NODE FM))

(DEFDISPATCH BIND-ANNOTATE SETQ (NODE FM)
  (BIND-ANNOTATE (SETQ-BODY FM)))

;;; Special hack for entities... if we're the (LAMBDA (OBJ . ARGS) FOO) in
;;; (MAKE-ENTITY ... (LAMBDA (OBJ OP) (IF ... (LAMBDA (OBJ . ARGS) FOO)) ...)),
;;; then we're a METHOD, and can be compiled exactly like a TPROC.
;;; Similarly if we're the (LAMBDA (OBJ OP) ...): HANDLERs are like TPROCs,
;;; too.

(DEFDISPATCH BIND-ANNOTATE LAMBDA (NODE FM)
  (SETF (LAMBDA-STRATEGY FM)
        (GET-LAMBDA-STRATEGY NODE
                             (COND ((AND (HANDLER-NODE? NODE)
                                         (LAMBDA-NODE? (CAR (CALL-ARGS (NODE-FORM (NODE-PARENT (NODE-LEVEL NODE)))))))
                                    (IF *ENTITY-DEBUG?* (BREAK GOT-A-HANDLER))
                                    ;; Sux.  Gotta force the strategy of
                                    ;;  MAKE-ENTITY's first (lambda)
                                    ;;  arg to be PROC and not TPROC.  Do this
                                    ;; in BIND-ANNOTATE-CALL?
                                    'HANDLER)
                                   ((METHOD-NODE? NODE)
                                    (IF *ENTITY-DEBUG?* (BREAK GOT-A-METHOD))
                                    'METHOD)
                                   (ELSE
                                    'PROC))))
  (BIND-ANNOTATE (LAMBDA-BODY FM)))

(DEFINE (GET-LAMBDA-STRATEGY NODE DEFAULT-STRATEGY)
  (LET ((BNODE (LAMBDA-BODY (NODE-FORM NODE))))
    (COND ((NEQ? BNODE (NODE-LEVEL BNODE))
           'JUMP)
          ((OR (NODE-REFS NODE) (NODE-SETQS NODE))
           (BIND-ANNOTATE-NOTE-CLOSURE-REFS NODE (NODE-REFS NODE))
           (BIND-ANNOTATE-NOTE-CLOSURE-REFS NODE (NODE-SETQS NODE))
           DEFAULT-STRATEGY)            ; EZCLOSE or PROC
          ((EQ? DEFAULT-STRATEGY 'EZCLOSE)
           'EZCLOSE)                    ; KLUDGE KLUDGE
          (ELSE
           'TPROC))))

;;; Fill in the CLOSURE-REFS slot of variables referred to by closures.
;;; This is a list of LAMBDA-nodes with the property that no member is a
;;; descendant of any other member.
;;; Maclisp compiler can't compile this routine correctly.

(DEFINE (BIND-ANNOTATE-NOTE-CLOSURE-REFS NODE VARS)
  (WALK (LAMBDA (V)
          (COND ((MEMQ NODE (VARIABLE-CLOSURE-REFS V))) ; already got it
                ((AND (VARIABLE-KNOWN-FUNCTION V)
                      (EQ? (LAMBDA-STRATEGY (NODE-FORM (VARIABLE-KNOWN-FUNCTION V)))
                           'JUMP))
                 (BUGLET ((*VAR* V))
                         "a JUMP LAMBDA is referred to by a closure"
                         "I don't know!  This isn't supposed to happen!"))
                ((AND (CATCH-NODE? (VARIABLE-BINDER V))
                      (EQ? (CATCH-STRATEGY (NODE-FORM (VARIABLE-BINDER V)))
                           'EXIT))
                 (BUGLET ((*VAR* V))
                         "an EXIT CATCH variable is referred to by a closure"
                         "I don't know!  This is horrible!"))
                (ELSE (IF (NOT (ANY (LAMBDA (C) (ANCESTOR? C NODE))
                                 (VARIABLE-CLOSURE-REFS V)))
                       (push (VARIABLE-CLOSURE-REFS V) NODE)))))
        VARS))

;;; Returns true if NODE1 is an ancestor of NODE2.

(DEFINE (ANCESTOR? NODE1 NODE2)
  (D0 ((P NODE2 (NODE-PARENT P)))
      ((NULL? P) NIL)
    (COND ((EQ? P NODE1) (RETURN T)))))

(DEFDISPATCH BIND-ANNOTATE CATCH (NODE FM)
  (BIND-ANNOTATE (CATCH-BODY FM))
  (SETF (CATCH-STRATEGY FM)
        (LET ((VAR (CATCH-VAR FM)))
          (COND ((AND (NULL? (VARIABLE-WRITE-REFS VAR))
                      (NULL? (VARIABLE-CLOSURE-REFS VAR))
                      (ALL-REFERENCES-ARE-CALLS? VAR))
                 (WALK (LAMBDA (R)
                         ;; Backpatch.
                         (SETF (CALL-STRATEGY (NODE-FORM (NODE-PARENT R)))
                               'EXIT))
                       (VARIABLE-READ-REFS VAR))
                 'EXIT)
                (ELSE
                 (WALK (LAMBDA (R)
                         (LET ((CFM (NODE-FORM (NODE-PARENT R))))
                           ;; Horrible unmodular kludge!
                           (COND ((EQ? (STYPE CFM) 'CALL)
                                  (LPROP (CAR (CALL-ARGS CFM)) NIL)))))
                       (VARIABLE-READ-REFS VAR))
                 (COND ((AND *GENERATE-TAIL-RECURSIVE-CALLS?*
                             (NODE-LEVEL NODE)
                             (LET ((P (NODE-PARENT (NODE-LEVEL NODE))))
                               (AND (LAMBDA-NODE? P)
                                    (NOT (MEMQ (LAMBDA-STRATEGY (NODE-FORM P))
                                               '(NONE JUMP))))))
                        'TAIL)
                       (ELSE
                        ;; (SETF (NODE-LEVEL NODE) NODE)
                        ;; (LEVEL-PROPAGATE NODE)
                        'PROC)))))))

(DEFDISPATCH BIND-ANNOTATE LABELS (NODE FM)
  (SETUP-KNOWN-FUNCTIONS-ETC (LABELS-VARS FM) (LABELS-VALS FM))
  (BIND-ANNOTATE (LABELS-BODY FM)))

;;; Common utility for LET and LABELS.  Think of a better name than this.
(DEFINE (SETUP-KNOWN-FUNCTIONS-ETC VARS VALS)
  ;; Set strategies for all the lambdas.
  (WALK (LAMBDA (VAR VAL)
          (COND ((LAMBDA-NODE? VAL)
                 (IF (NULL? (VARIABLE-WRITE-REFS VAR))
                     (SETF (VARIABLE-KNOWN-FUNCTION VAR) VAL))
                 (SETF (LAMBDA-STRATEGY (NODE-FORM VAL))
                       (GET-LAMBDA-STRATEGY VAL
                                            (IF (ALL-REFERENCES-ARE-CALLS? VAR)
                                                'EZCLOSE
                                              'PROC))))))
        VARS
        VALS)
  ;; Now recursively annotate the values.
  (WALK (LAMBDA (VAL)
          (COND ((LAMBDA-NODE? VAL)
                 (BIND-ANNOTATE (LAMBDA-BODY (NODE-FORM VAL))))
                (ELSE
                 (BIND-ANNOTATE VAL))))
        VALS))

(DEFDISPATCH BIND-ANNOTATE BLOCK (NODE FM)
  (WALK BIND-ANNOTATE (BLOCK-ARGS FM)))

(DEFDISPATCH BIND-ANNOTATE IF (NODE FM)
  (BIND-ANNOTATE (IF-PRED FM))
  (BIND-ANNOTATE (IF-CON FM))
  (BIND-ANNOTATE (IF-ALT FM)))

;;; This should decide about whether the CASE should be table lookup or not.

(DEFDISPATCH BIND-ANNOTATE CASE (NODE FM)
  (BIND-ANNOTATE (CASE-KEY FM))
  (BIND-ANNOTATE (CASE-ELSE FM))
  (WALK (LAMBDA (C) (BIND-ANNOTATE (CDR C)))
        (CASE-CLAUSES FM)))

(DEFINE *ALLOW-ENTITY-STRATEGY-CALLS?* T)

(DEFDISPATCH BIND-ANNOTATE CALL (NODE FM)
  (LET ((FN (NODE-FORM (CALL-FUNCTION FM))))
    (CASE (STYPE FN)
      ((STATIC)
       (COND ((CGET FN 'PRIMOP) (BIND-ANNOTATE-CALL-PRIMOP NODE))
             ((CGET FN 'ENTITY-CONSTRUCTOR)
              (BIND-ANNOTATE-CALL-PROC NODE FM)
              (LET ((P (CAR (CALL-ARGS FM))))
                (COND ((LAMBDA-NODE? P)
                       ;; Incredible kludge - disallow TPROCs
                       (IF (OR (NODE-REFS NODE) (NODE-SETQS NODE))
                           (SETF (LAMBDA-STRATEGY (NODE-FORM P)) 'PROC))
                       (IF (AND (LAMBDA-NODE? (CADR (CALL-ARGS FM)))
                                *ALLOW-ENTITY-STRATEGY-CALLS?*)
                           (SETF (CALL-STRATEGY FM) 'ENTITY)))))
              (IF *ENTITY-DEBUG?* (BREAK GOT-A-ENTITY)))
             (ELSE (BIND-ANNOTATE-CALL-PROC NODE FM))))
      ((VARIABLE)
       (COND ((AND (VARIABLE-KNOWN-FUNCTION FN)
                   (EQ? (LAMBDA-STRATEGY
                         (NODE-FORM (VARIABLE-KNOWN-FUNCTION FN)))
                        'JUMP))
              (SETF (CALL-STRATEGY FM) 'JUMP)
              (WALK BIND-ANNOTATE (CALL-ARGS FM)))
             ((AND (CATCH-NODE? (VARIABLE-BINDER FN))
                   (EQ? (CATCH-STRATEGY (NODE-FORM (VARIABLE-BINDER FN)))
                        'EXIT))
              (SETF (CALL-STRATEGY FM) 'EXIT)
              (WALK BIND-ANNOTATE (CALL-ARGS FM)))
             ;; What about EZCLOSE?
             (ELSE (BIND-ANNOTATE-CALL-PROC NODE FM))))
      ((LAMBDA)
       (COND ((EMPTY (LAMBDA-RESTVAR (NODE-FORM (CALL-FUNCTION FM))))
              (BIND-ANNOTATE-CALL-LET NODE FM (NODE-FORM (CALL-FUNCTION FM))))
             (ELSE (BIND-ANNOTATE-CALL-PROC NODE FM))))
      (ELSE (BIND-ANNOTATE-CALL-PROC NODE FM)))))

(DEFINE (BIND-ANNOTATE-CALL-LET NODE FM FN)
  (IGNORE NODE)
  (SETF (CALL-STRATEGY FM) 'LET)
  (SETF (LAMBDA-STRATEGY FN) 'NONE)
  (SETUP-KNOWN-FUNCTIONS-ETC (LAMBDA-VARS FN) (CALL-ARGS FM))
  (BIND-ANNOTATE (LAMBDA-BODY FN)))

(DEFINE (BIND-ANNOTATE-CALL-PROC NODE FM)
  (LET ((PNODE (NODE-PARENT (NODE-LEVEL NODE))))
    (SETF (CALL-STRATEGY FM)
          (COND ((AND *GENERATE-TAIL-RECURSIVE-CALLS?*
                      PNODE
                      (LAMBDA-NODE? PNODE)
                      (NOT (MEMQ (LAMBDA-STRATEGY (NODE-FORM PNODE))
                                 '(JUMP NONE))))
                 'TAIL)
                (ELSE 'PROC))))
  (BIND-ANNOTATE (CALL-FUNCTION FM))
  (WALK BIND-ANNOTATE (CALL-ARGS FM)))

(DEFINE (BIND-ANNOTATE-CALL-PRIMOP NODE)
  (LET* ((FM (NODE-FORM NODE))
         (INFO (CGET (NODE-FORM (CALL-FUNCTION FM))
                     'BIND-ANNOTATE-PRIMOP-INFO)))
    (SETF (CALL-STRATEGY FM) 'PRIMOP)
    (COND (INFO (WALK (LAMBDA (A I)
                        (COND ((EQ? I 'JUMP)
                               (BIND-ANNOTATE-JUMP-LAMBDA A))
                              (ELSE
                               (BIND-ANNOTATE A))))
                      (CALL-ARGS FM)
                      INFO))
          (ELSE (WALK BIND-ANNOTATE (CALL-ARGS FM))))))

(DEFINE (BIND-ANNOTATE-JUMP-LAMBDA NODE)
  (OR (LAMBDA-NODE? NODE)       ;Catch (FIXNUM-DIV2 A B C) with C not a LAMBDA
      (BUGLET ((*NODE* NODE))
              "node in BIND-ANNOTATE-JUMP-LAMBDA isn't a LAMBDA"
              "burn and die horribly"))
  (BIND-ANNOTATE (LAMBDA-BODY (NODE-FORM NODE)))
  (SETF (LAMBDA-STRATEGY (NODE-FORM NODE)) 'JUMP))

;;; "Canonicalization"

;;; Think of a better term than CANONICALIZE.

;;; The CANONICALIZE phase is charged with assorted random tasks:
;;; - Calls to primop predicates (FOO? X) not in predicate position are
;;;   turned into (IF (FOO? X) T NIL).  This simplifies the code generator
;;;   and others, and someday might help arm-merging as hinted at below.
;;; - For each TPROC, object code is generated by calling
;;;   FINISH-COMPILATION or whatever.
;;; - Someday, begin to do common subexpression stuff: e.g.
;;;     (IF P (IF Q T NIL) T NIL)  ==>
;;;     (LET ((F (LAMBDA () T)))
;;;       (IF P (IF Q (F) NIL) (F) NIL))

;;; This phase should be invoked after strategy-annotation.

(DEFINE-MACRO (CANONICALIZE! FROB)
  `(LET ((%%%%NEWFROB%%%% (CANONICALIZE ,FROB)))
     (COND ((NEQ? %%%%NEWFROB%%%% ,FROB)
            (SETF ,FROB %%%%NEWFROB%%%%)
;;          (NODE-PARENT-CHECK %%%%NEWFROB%%%% ,NODE)
            (REPROPAGATE %%%%NEWFROB%%%%)))))

(DEFINE (CANONICALIZE NODE)
  (NODE-DISPATCH CANONICALIZE NODE))

(DEFDISPATCH CANONICALIZE CONSTANT (NODE FM)
  NODE)

(DEFDISPATCH CANONICALIZE STATIC (NODE FM)
  NODE)

(DEFDISPATCH CANONICALIZE VARIABLE (NODE FM)
  NODE)

(DEFDISPATCH CANONICALIZE SETQ (NODE FM)
  (CANONICALIZE! (SETQ-BODY FM))
  NODE)

(DEFDISPATCH CANONICALIZE LAMBDA (NODE FM)
  (DECLARE (SPECIAL *COMPILE-TPROCS-EARLY?*))
  (CANONICALIZE! (LAMBDA-BODY FM))
  (COND ((AND (EQ? (LAMBDA-STRATEGY FM) 'TPROC)
              *COMPILE-TPROCS-EARLY?*
              (NOT (AND (NODE-PARENT NODE)      ; horrible kludge
                        (CALL-NODE? (NODE-PARENT NODE))
                        (EQ? (CALL-STRATEGY (NODE-FORM (NODE-PARENT NODE)))
                             'ENTITY)))
              (LET ((VAR (NODE-VAR NODE)))
                (NOT (AND VAR (VARIABLE-KNOWN-FUNCTION VAR)))))
         (LET ((PNODE (NODE-PARENT NODE)))
           (SETF (NODE-PARENT NODE) NIL)
           (SETF (NODE-LEVEL NODE) NODE)
           (FINISH-COMPILATION NODE)
           (LET* ((PTAG (CDDR (LAMBDA-GENTAG FM)))
                  (NEWNODE
                   (CREATE-CONSTANT (QLOZURE (PTAG)
                                      (LAMBDA () (GEN-TPROC PTAG))
                                      ((FUNNY-CONSTANT? SELF) T)
                                      ((PRIN SELF STREAM)
                                       (FORMAT STREAM "#{Tproc-stub ~S}" PTAG))
                                      ))))
             (SETF (NODE-PARENT NEWNODE) PNODE)
             (ERASE-ALL-NODES NODE)     ; ??
             NEWNODE)))
        (ELSE
         NODE)))

(DEFDISPATCH CANONICALIZE CATCH (NODE FM)
  (CANONICALIZE! (CATCH-BODY FM))
  NODE)

(DEFDISPATCH CANONICALIZE LABELS (NODE FM)
  (DO ((V (LABELS-VALS FM) (CDR V)))
      ((NULL? V)
       (CANONICALIZE! (LABELS-BODY FM))
       NODE)
    (CANONICALIZE! (CAR V))))

(DEFDISPATCH CANONICALIZE BLOCK (NODE FM)
  (DO ((A (BLOCK-ARGS FM) (CDR A)))
      ((NULL? A) NODE)
    (CANONICALIZE! (CAR A))))

(DEFDISPATCH CANONICALIZE IF (NODE FM)
  (CANONICALIZE! (IF-PRED FM))
  (CANONICALIZE! (IF-CON FM))
  (CANONICALIZE! (IF-ALT FM))
  NODE)

(DEFDISPATCH CANONICALIZE CASE (NODE FM)
  (CANONICALIZE! (CASE-KEY FM))
  (DO ((C (CASE-CLAUSES FM) (CDR C)))
      ((NULL? C)
       (CANONICALIZE! (CASE-ELSE FM))
       NODE)
    (CANONICALIZE! (CDAR C))))

(DEFDISPATCH CANONICALIZE CALL (NODE FM)
  (COND ;((EQ? (CALL-STRATEGY FM) 'ENTITY) ...)
        ((STATIC-NODE? (CALL-FUNCTION FM))
         (LET ((PROBE (CGET (NODE-FORM (CALL-FUNCTION FM)) 'CANONICALIZE)))
           (IF PROBE
               (LET ((NEWNODE (FUNCALL PROBE NODE FM)))
                 (CANONICALIZE-CALL-1 NEWNODE (NODE-FORM NEWNODE)))
             (CANONICALIZE-CALL-1 NODE FM))))
        (ELSE (CANONICALIZE-CALL-1 NODE FM))))

(DEFINE (CANONICALIZE-CALL-1 NODE FM)
  (CANONICALIZE! (CALL-FUNCTION FM))
  (DO ((A (CALL-ARGS FM) (CDR A)))
      ((NULL? A)
       (MAYBE-IFIFY-PRIMOP-PREDICATE NODE))
    (CANONICALIZE! (CAR A))))

(COMMENT
(DEFINE-MACRO (DEFINE-CANONICALIZER FN ARGS . BODY)
  (LET ((FOO (SYMBOLCONC FN '$CANONICALIZE)))
    `(PROGN 'COMPILE
            (DEFINE (,FOO . ,ARGS) . ,BODY)
            (CPUT ',FN 'CANONICALIZE ,FOO))))

(DEFINE-CANONICALIZER MAKE-ENTITY (NODE FM)
  (LET ((A (CALL-ARGS FM)))
    (COND ((AND (LAMBDA-NODE? (CAR A))
                (LAMBDA-NODE? (CADR A)))
           ;(SETF ...)
           NODE))))
)

(STAT-COUNTER *CALL-PRIMOP-IF-WRAP-COUNT*
              "(FOO? X) becomes (IF (FOO? X) T NIL)")

(DEFINE (MAYBE-IFIFY-PRIMOP-PREDICATE NODE)
  (LET ((FM (NODE-FORM NODE))
        (PARENT (NODE-PARENT NODE)))
    (COND ((AND (EQ? (CALL-STRATEGY FM) 'PRIMOP)
                (CGET (NODE-FORM (CALL-FUNCTION FM))
                      'PRIMOP-PREDICATE)
                (OR (NULL? PARENT)
                    (NOT (IF-NODE? PARENT))
                    (NOT (EQ? NODE (IF-PRED (NODE-FORM PARENT))))))
           (LET ((NEWNODE (ALPHATIZE `(IF ,(INT NODE) ',T ',NIL))))
             (INCR *CALL-PRIMOP-IF-WRAP-COUNT*)
             (SETF (NODE-PARENT NEWNODE) PARENT)
             (REANALYZE NEWNODE)))
          (ELSE
           NODE))))
