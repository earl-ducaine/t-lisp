(HERALD CLOSURE
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;; Depth and closure annotation.

;;;; Depth annotation

;;; Set the DEPTH slot of each node.  This is simply a convenience for the 
;;; common ancestor routine used by CLOSE-ANNOTATE.

(DEFINE (DEPTH-ANNOTATE NODE DEPTH)
  (SETF (NODE-DEPTH NODE) DEPTH)
  (NODE-DISPATCH DEPTH-ANNOTATE NODE (FX+ DEPTH 1)))

(DEFDISPATCH DEPTH-ANNOTATE CONSTANT (NODE FM DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE STATIC (NODE FM DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE VARIABLE (NODE FM DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE SETQ (NODE FM DEPTH)
  (DEPTH-ANNOTATE (SETQ-BODY FM) DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE CATCH (NODE FM DEPTH)
  (DEPTH-ANNOTATE (CATCH-BODY FM) DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE LAMBDA (NODE FM DEPTH)
  (DEPTH-ANNOTATE (LAMBDA-BODY FM) DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE LABELS (NODE FM DEPTH)
  (WALK (LAMBDA (A) (DEPTH-ANNOTATE A DEPTH)) (LABELS-VALS FM))
  (DEPTH-ANNOTATE (LABELS-BODY FM) DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE BLOCK (NODE FM DEPTH)
  (WALK (LAMBDA (A) (DEPTH-ANNOTATE A DEPTH)) (BLOCK-ARGS FM)))

(DEFDISPATCH DEPTH-ANNOTATE IF (NODE FM DEPTH)
  (DEPTH-ANNOTATE (IF-PRED FM) DEPTH)
  (DEPTH-ANNOTATE (IF-CON FM) DEPTH)
  (DEPTH-ANNOTATE (IF-ALT FM) DEPTH))

(DEFDISPATCH DEPTH-ANNOTATE CASE (NODE FM DEPTH)
  (DEPTH-ANNOTATE (CASE-KEY FM) DEPTH)
  (DEPTH-ANNOTATE (CASE-ELSE FM) DEPTH)
  (WALK (LAMBDA (C) (DEPTH-ANNOTATE (CDR C) DEPTH)) (CASE-CLAUSES FM)))

(DEFDISPATCH DEPTH-ANNOTATE CALL (NODE FM DEPTH)
  (DEPTH-ANNOTATE (CALL-FUNCTION FM) DEPTH)
  (WALK (LAMBDA (A) (DEPTH-ANNOTATE A DEPTH)) (CALL-ARGS FM)))

;;;; Closure annotation

;;; A CONSENV is a list in 1-1 correspondence with the EXTEND structure
;;; which will represent the consed env at runtime.  In fact, the first
;;; element of the list represents the extend's template (either (), for
;;; useless template, or a LAMBDA-node for a closure which got consed when
;;; the environment did), and the second element is optionally a
;;; backpointer for outer environment contours (i.e. another CONSENV).
;;; The other elements (second or third through last) are VARIABLE structures.

(DEFINE (CONSENV-LOOKUP VAR CENV)
  (AND (OR (PAIR? VAR)                  ; see TARGETIZE-POINT-OF-CALL
           (VARIABLE-CLOSURE-REFS VAR)) ; speed hack
       (OR (MEMQ VAR CENV)
           (D0 ((E CENV (CDR E)))
               ((NULL? E) NIL)
             (IF (AND (PAIR? (CAR E)) (CONSENV-LOOKUP VAR (CAR E)))
                 (RETURN T))))))

(DEFINE (CHILD-CONSENV NODE CENV)
  (LET* ((M (NODE-MIGRATIONS NODE))
         (CENV
          (COND ((NOT (NULL? M))
                 (CONS '()              ; template - gets clobberred later
                         (COND ((ENV-BACKPOINTER-NEEDED? NODE CENV)
                                (CONS CENV M))
                               (ELSE M))))
                ((AND (NULL? (NODE-REFS NODE)) (NULL? (NODE-SETQS NODE)))
                 '())
                (ELSE CENV))))
    (SETF (NODE-CONSENV NODE) CENV)
    CENV))

;;; This predicate should return true if any of this node's refs or setqs is
;;;  in the consed env, or if it makes calls to any EZCLOSE procedures.
;;; Does this have a bad interaction with calls to EZCLOSEs?  - YES!

(DEFINE (ENV-BACKPOINTER-NEEDED? NODE CENV)
  (AND (NOT (NULL? CENV))
       (D0 ((R (NODE-REFS NODE) (CDR R)))
           ((NULL? R)
            (D0 ((R (NODE-SETQS NODE) (CDR R)))
                ((NULL? R) NIL)
              (COND ((CONSENV-LOOKUP (CAR R) CENV)
                     (RETURN T)))))
         (COND ((OR (CONSENV-LOOKUP (CAR R) CENV)
                    (VARIABLE-KNOWN-FUNCTION (CAR R)))
                (RETURN T))))))

;;; Common migration utility for LAMBDA, CATCH, and LABELS cases of
;;; CLOSE-ANNOTATE.  Hairy postprocessing for setq'ed variables tries to
;;; delay migration until latest possible point which is still
;;; reasonable with respect to trying to cluster migrations.

(DEFINE (CLOSE-ANNOTATE-VARS NODE VARS)
  (DO ((V VARS (CDR V))
       (Z '())                          ; list of setq vars
       (A '()))                         ; list of setq var ancestors
      ((NULL? V)
       (IF Z
           ;; Last-ditch postprocessing on setq'ed variables: for each variable,
           ;; try to find an ancestor of its preferred migration point which
           ;; would not mind migrating it.
           (DO ((Z Z (CDR Z))
                (A A (CDR A))
                (ZZ '())
                (AA '()))
               ((NULL? Z)
                ;; All done.  All setqed vars which haven't already been taken
                ;; care of will get migrated at some reasonable common node.
                (IF ZZ
                    (LET ((N (OK-MIGRATION-ANCESTOR
                              NODE
                              (COMMON-ANCESTOR-LIST NIL AA))))
                      (SETF (NODE-MIGRATIONS N)
                            (UNION (NODE-MIGRATIONS N) ZZ)))))
             ----
             (D0 ((N (CAR A) (NODE-PARENT N)))
                 ((EQ? N (VARIABLE-BINDER (CAR Z)))
                  (push ZZ (CAR Z))    ; lose, keep it for end
                  (push AA (CAR A)))
               ----
               (COND ((NULL? N)
                      (BUGLET ((*VAR* (CAR Z)))
                              "binder not in ancestor path in CLOSE-ANNOTATE-VAR"
                              "none - hopefully things will work out ok anyhow")
                      (SETQ N (VARIABLE-BINDER (CAR Z))))
                     ((GOOD-PLACE-FOR-MIGRATIONS? N)
                      (SETF (NODE-MIGRATIONS N) ; win, migrate var here.
                            (ADJOIN (CAR Z) (NODE-MIGRATIONS N)))
                      (RETURN NIL)))
               ---)
             ---)))
    ----
    (LET ((PROBE (CLOSE-ANNOTATE-VAR NODE (CAR V))))
      (COND (PROBE (push Z (CAR V))
                   (push A PROBE))))
    ---))

;;; Do migration processing for a single variable.
;;; If it's setq'ed and closure-refd, but we can't find a really good place
;;; to migrate this variable, we return the nearest common ancestor of all
;;; its read-refs and write-refs.

(DEFINE (CLOSE-ANNOTATE-VAR NODE VAR)
  (COND ((OR (NULL? VAR)
             (NULL? (VARIABLE-CLOSURE-REFS VAR))
             (AND (VARIABLE-KNOWN-FUNCTION VAR)
                  (EQ? (LAMBDA-STRATEGY
                        (NODE-FORM (VARIABLE-KNOWN-FUNCTION VAR)))
                       'EZCLOSE)))
         NIL)
        ((OR (VARIABLE-WRITE-REFS VAR)
             (LABELS-NODE? (VARIABLE-BINDER VAR)))      ; temp hack...
         ;; This assumes preorder processing of variables... otherwise we'll
         ;; get n^2 performance in DEPTH-ANNOTATE...
         (IF (EMPTY (NODE-DEPTH (VARIABLE-BINDER VAR)))
             (DEPTH-ANNOTATE (VARIABLE-BINDER VAR) 0))
         (OK-MIGRATION-ANCESTOR
          NODE
          (COMMON-ANCESTOR-LIST
           (COMMON-ANCESTOR-LIST (CAR (VARIABLE-CLOSURE-REFS VAR))
                                 (VARIABLE-WRITE-REFS VAR))
           (VARIABLE-READ-REFS VAR))))
        (ELSE                          ; Not setq'ed.  Do multiple migrations.
         (WALK (LAMBDA (R)
                 (LET ((RR (OK-MIGRATION-ANCESTOR NODE R)))
                   (SETF (NODE-MIGRATIONS RR)
                         (ADJOIN VAR (NODE-MIGRATIONS RR)))))
               (VARIABLE-CLOSURE-REFS VAR))
         NIL)))

;;; Return the node or one of its ancestors at which we wouldn't be unhappy
;;;  doing migrations.  Hack for MAKE-ENTITY sometime.
;;; TOP is the uppermost node at which we dare migrate anything.

(DEFINE (OK-MIGRATION-ANCESTOR TOP NODE)
  (DO ((N NODE (NODE-PARENT N))
       (NODE NODE (COND ((LABELS-NODE? N) N) (ELSE NODE))))
      ((EQ? N TOP)
       (LET ((NODE (COND ((LABELS-NODE? N) N) (ELSE NODE))))
         (COND ((NODE-MIGRATIONS NODE) NODE)
               ((NOT (LAMBDA-NODE? NODE)) NODE)
               ((EQ? (LAMBDA-STRATEGY (NODE-FORM NODE)) 'EZCLOSE)
                (NODE-PARENT NODE))
               ((ENTITY-NODE? (NODE-PARENT NODE))
                (NODE-PARENT NODE))
               (ELSE NODE))))))

(DEFINE (GOOD-PLACE-FOR-MIGRATIONS? NODE)
  (OR (NOT (NULL? (NODE-MIGRATIONS NODE)))
      (AND (LAMBDA-NODE? NODE)
           (EQ? (LAMBDA-STRATEGY (NODE-FORM NODE)) 'PROC))))

(DEFINE (COMMON-ANCESTOR-LIST NODE NODELIST)
  (DO ((A (NO-OP NODE) (COMMON-ANCESTOR A (CAR L)))   ; circumvent TC bug!
       (L (NO-OP NODELIST) (CDR L)))
      ((NULL? L) A)))

(DEFINE (COMMON-ANCESTOR NODE1 NODE2)
  (DECLARE (FIXNUM DEPTH1 D1 D2 D))
  (IF (NULL? NODE1) NODE2
    (DO ((DEPTH1 (NODE-DEPTH NODE1))
         (D2 (NODE-DEPTH NODE2) (FX- D2 1))
         (P2 NODE2 (NODE-PARENT P2)))
        ((FX<= D2 DEPTH1)
         (DO ((P1 NODE1 (NODE-PARENT P1))
              (D1 DEPTH1 (FX- D1 1)))
             ((FX= D1 D2)
              ;; At this point, P1 and P2 are at the same depth (D1 = D2)
              ;; in the tree.  Now ascend until we get to their common
              ;; ancestor.
              (D0 ((P1 P1 (NODE-PARENT P1))
                   (P2 P2 (NODE-PARENT P2)))
                  ((EQ? P1 P2) P1)
                ----
                (COND ((OR (NULL? P1) (NULL? P2))
                       (BUGLET ((*NODE1* NODE1)
                                (*NODE2* NODE2))
                               "ran through root of tree in COMMON-ANCESTOR"
                               "will assume ancestor is first node (!)")
                       (RETURN NODE1)))
                ---)))))))

;;; Before we CLOSE-ANNOTATE a node, its MIGRATIONS slot may already be
;;; non-null, but afterwards, it should not be augmented.

(DEFINE (CLOSE-ANNOTATE NODE CENV)
  (NODE-DISPATCH CLOSE-ANNOTATE NODE CENV))

(DEFDISPATCH CLOSE-ANNOTATE CONSTANT (NODE FM CENV)
  (CHILD-CONSENV NODE CENV))

(DEFDISPATCH CLOSE-ANNOTATE STATIC (NODE FM CENV)
  (CHILD-CONSENV NODE CENV))

(DEFDISPATCH CLOSE-ANNOTATE VARIABLE (NODE FM CENV)
  (CHILD-CONSENV NODE CENV))

(DEFDISPATCH CLOSE-ANNOTATE SETQ (NODE FM CENV)
  (CLOSE-ANNOTATE (SETQ-BODY FM) (CHILD-CONSENV NODE CENV)))

(DEFDISPATCH CLOSE-ANNOTATE CATCH (NODE FM CENV)
  (CLOSE-ANNOTATE-VARS (CATCH-BODY FM) (LIST (CATCH-VAR FM)))
  (CLOSE-ANNOTATE (CATCH-BODY FM) (CHILD-CONSENV NODE CENV)))

(DEFDISPATCH CLOSE-ANNOTATE LAMBDA (NODE FM CENV)
  (CLOSE-ANNOTATE-VARS (LAMBDA-BODY FM)
                       (COND ((EMPTY (LAMBDA-RESTVAR FM)) (LAMBDA-VARS FM))
                             (ELSE (CONS (LAMBDA-RESTVAR FM) (LAMBDA-VARS FM)))))
  (LET ((CENV (CHILD-CONSENV NODE CENV)))
    (COND ((AND (EQ? (LAMBDA-STRATEGY FM) 'PROC)
                (NOT (NULL? CENV))
                (NULL? (CAR CENV)))
           (SETF (CAR CENV) NODE)))             ; Hack
    (CLOSE-ANNOTATE (LAMBDA-BODY FM) CENV)))

(DEFDISPATCH CLOSE-ANNOTATE LABELS (NODE FM CENV)
  (CLOSE-ANNOTATE-VARS NODE (LABELS-VARS FM))
  (LET ((CENV (CHILD-CONSENV NODE CENV)))
    (WALK (LAMBDA (A) (CLOSE-ANNOTATE A CENV)) (LABELS-VALS FM))
    (CLOSE-ANNOTATE (LABELS-BODY FM) CENV)))

(DEFDISPATCH CLOSE-ANNOTATE BLOCK (NODE FM CENV)
  (LET ((CENV (CHILD-CONSENV NODE CENV)))
    (WALK (LAMBDA (A) (CLOSE-ANNOTATE A CENV)) (BLOCK-ARGS FM))))

(DEFDISPATCH CLOSE-ANNOTATE IF (NODE FM CENV)
  (LET ((CENV (CHILD-CONSENV NODE CENV)))
    (CLOSE-ANNOTATE (IF-PRED FM) CENV)
    (CLOSE-ANNOTATE (IF-CON FM) CENV)
    (CLOSE-ANNOTATE (IF-ALT FM) CENV)))

(DEFDISPATCH CLOSE-ANNOTATE CASE (NODE FM CENV)
  (LET ((CENV (CHILD-CONSENV NODE CENV)))
    (CLOSE-ANNOTATE (CASE-KEY FM) CENV)
    (CLOSE-ANNOTATE (CASE-ELSE FM) CENV)
    (WALK (LAMBDA (C) (CLOSE-ANNOTATE (CDR C) CENV)) (CASE-CLAUSES FM))))

;;; Do something funny with MAKE-ENTITY someday.

(DEFDISPATCH CLOSE-ANNOTATE CALL (NODE FM CENV)
  (LET ((CENV (CHILD-CONSENV NODE CENV)))
    (CLOSE-ANNOTATE (CALL-FUNCTION FM) CENV)
    (WALK (LAMBDA (A) (CLOSE-ANNOTATE A CENV)) (CALL-ARGS FM))))
