(HERALD COPY
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; COPY-CODE

(DEFINE (COPY-CODE NODE)
  (LET ((NEWCODE (COPY-NODES NODE '() '())))
    (REANALYZE NEWCODE)))

;;; RENAMINGS is an a-list for both variables and progbodies.

(DEFINE (COPY-NODES NODE RENAMINGS PARENT)
  (LET ((NEWNODE (NODIFY NIL)))
    (SETF (NODE-PARENT NEWNODE) PARENT)
    (SETF (NODE-FORM NEWNODE)
          (NODE-DISPATCH COPY-NODES NODE NEWNODE RENAMINGS))
    (SETF (NODE-LEVEL NEWNODE) *EMPTY*)
    NEWNODE))

(DEFDISPATCH COPY-NODES CONSTANT (NODE FM NEWNODE RENAMINGS) FM)

(DEFDISPATCH COPY-NODES STATIC (NODE FM NEWNODE RENAMINGS) FM)

(DEFDISPATCH COPY-NODES VARIABLE (NODE FM NEWNODE RENAMINGS)
  (COND ((CDR (ASSQ FM RENAMINGS)))     ;Hmm
        (ELSE FM)))

(DEFDISPATCH COPY-NODES SETQ (NODE FM NEWNODE RENAMINGS)
  (CONS-A-SETQ VAR (COND ((STATIC? (SETQ-VAR FM))
                          (SETQ-VAR FM))
                         (ELSE
                          (OR (CDR (ASSQ (SETQ-VAR FM) RENAMINGS))
                              (SETQ-VAR FM))))
               BODY (COPY-NODES (SETQ-BODY FM) RENAMINGS NEWNODE)
               LOCALP (SETQ-LOCALP FM)
               DEFINITIONP (SETQ-DEFINITIONP FM)))

(DEFDISPATCH COPY-NODES LAMBDA (NODE FM NEWNODE RENAMINGS)
  (DO ((V (LAMBDA-VARS FM) (CDR V))
       (NV '()))
      ((NULL? V)
       (LET* ((RV (LAMBDA-RESTVAR FM))
              (VAR (COND ((AND (NOT (EMPTY RV)) RV)
                          (LET ((VAR (COPY-VARIABLE RV NEWNODE)))
                            (push RENAMINGS (CONS RV VAR))
                            VAR))
                         (ELSE RV))))
         (CONS-A-LAMBDA VARS (REVERSE! NV)
                        BODY (COPY-NODES (LAMBDA-BODY FM) RENAMINGS NEWNODE)
                        RESTVAR VAR)))
    ----
    (PUSH NV
           (COND ((NULL? (CAR V)) '())
                 (ELSE (LET ((VAR (COPY-VARIABLE (CAR V) NEWNODE)))
                         (PUSH RENAMINGS (CONS (CAR V) VAR))
                         VAR))))
    ---))

(DEFDISPATCH COPY-NODES CATCH (NODE FM NEWNODE RENAMINGS)
  (LET* ((OLDVAR (CATCH-VAR FM))
         (NEWVAR (COPY-VARIABLE OLDVAR NEWNODE)))
    (CONS-A-CATCH VAR NEWVAR
                  BODY (COPY-NODES (CATCH-BODY FM)
                                   (CONS (CONS OLDVAR NEWVAR)
                                         RENAMINGS)
                                   NEWNODE))))

(DEFDISPATCH COPY-NODES LABELS (NODE FM NEWNODE RENAMINGS)
  (LET* ((NEWVARS (MAP (LAMBDA (VAR)
                         (COPY-VARIABLE VAR NEWNODE))
                       (LABELS-VARS FM)))
         (NR (APPEND! (MAP CONS (LABELS-VARS FM) NEWVARS)
                      RENAMINGS)))
    (CONS-A-LABELS
     VARS NEWVARS
     VALS (MAP (LAMBDA (VAL) (COPY-NODES VAL NR NEWNODE))
               (LABELS-VALS FM))
     BODY (COPY-NODES (LABELS-BODY FM) NR NEWNODE))))

(DEFDISPATCH COPY-NODES BLOCK (NODE FM NEWNODE RENAMINGS)
  (CONS-A-BLOCK ARGS (MAP (LAMBDA (X) (COPY-NODES X RENAMINGS NEWNODE))
                          (BLOCK-ARGS FM))))

(DEFDISPATCH COPY-NODES IF (NODE FM NEWNODE RENAMINGS)
  (CONS-A-IF PRED (COPY-NODES (IF-PRED FM) RENAMINGS NEWNODE)
             CON (COPY-NODES (IF-CON FM) RENAMINGS NEWNODE)
             ALT (COPY-NODES (IF-ALT FM) RENAMINGS NEWNODE)))

(DEFDISPATCH COPY-NODES CASE (NODE FM NEWNODE RENAMINGS)
  (CONS-A-CASE KEY (COPY-NODES (CASE-KEY FM) RENAMINGS NEWNODE)
               ELSE (COPY-NODES (CASE-ELSE FM) RENAMINGS NEWNODE)
               CLAUSES (MAP (LAMBDA (C)
                              (CONS (CAR C)
                                    (COPY-NODES (CDR C) RENAMINGS NEWNODE)))
                            (CASE-CLAUSES FM))))

(DEFDISPATCH COPY-NODES CALL (NODE FM NEWNODE RENAMINGS)
  (CONS-A-CALL FUNCTION (COPY-NODES (CALL-FUNCTION FM)
                                    RENAMINGS
                                    NEWNODE)
               ARGS (MAP (LAMBDA (A)
                           (COPY-NODES A RENAMINGS NEWNODE))
                         (CALL-ARGS FM))))

(DEFINE (COPY-VARIABLE VAR NEWNODE)
  (CONS-A-VARIABLE IDENTIFIER (GENERATE-SYMBOL (VARIABLE-IDENTIFIER VAR))
                   BINDER NEWNODE))