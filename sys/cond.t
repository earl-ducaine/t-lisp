(HERALD (TSYS COND T 14)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Conditional macros

(DEFINE **NO-MORE-COND-CLAUSES** '**NO-MORE-COND-CLAUSES**)     ; NIL ?

(DEFINE-SYNTAX (COND . CLAUSES)
  (LABELS (((EXPAND-COND CLAUSES)
            (COND ((ATOM? CLAUSES) '**NO-MORE-COND-CLAUSES**)
                  ((ATOM? (CAR CLAUSES))
                   (SYNTAX-ERROR "atomic ~S clause: ~S" 'COND (CAR CLAUSES)))
                  ((ATOM? (CDAR CLAUSES))
                   `(,(t-syntax 'OR) ,(CAAR CLAUSES)
				     ,(EXPAND-COND (CDR CLAUSES))))
                  ((EQ? (CADAR CLAUSES) '=>)
                   `(COND-=>-AUX ,(CAAR CLAUSES)
                                 (,(t-syntax 'LAMBDA) ()
                                   ,(CADDR (CAR CLAUSES)))
                                 (,(t-syntax 'LAMBDA) ()
                                   ,(EXPAND-COND (CDR CLAUSES)))))
                  (ELSE `(,(t-syntax 'IF) ,(CAAR CLAUSES)
			     ,(BLOCKIFY (CDAR CLAUSES))
			     ,(EXPAND-COND (CDR CLAUSES)))))))
    (EXPAND-COND CLAUSES)))

(DEFINE-SYNTAX (XCOND . CLAUSES)
  `(,(t-syntax 'COND) ,@CLAUSES (ELSE (NO-OP (LOSING-XCOND)))))

(DEFINE (LOSING-XCOND)
  (ERROR "no clause selected in ~S expression" 'XCOND))

(DEFINE-SYNTAX (OR . ARGS)
  (LABELS (((EXPAND-OR ARGS)
            (COND ((ATOM? ARGS) 'NIL)
                  ((ATOM? (CDR ARGS)) (CAR ARGS))
                  (ELSE `(OR-AUX ,(CAR ARGS)
				 (,(t-syntax 'LAMBDA) ()
				       ,(EXPAND-OR (CDR ARGS))))))))
    (EXPAND-OR ARGS)))

(DEFINE-SYNTAX (AND . ARGS)
  (LABELS (((EXPAND-AND ARGS)
            (COND ((ATOM? ARGS) 'T)
                  ((ATOM? (CDR ARGS)) (CAR ARGS))
                  (ELSE `(,(t-syntax 'IF) ,(CAR ARGS)
					  ,(EXPAND-AND (CDR ARGS))
					  NIL)))))
    (EXPAND-AND ARGS)))

(DEFINE **CASE-FELL-OFF-END** '**CASE-FELL-OFF-END**)

(DEFINE-SYNTAX (CASE KEY . CLAUSES)
  (LABELS (((EXPAND-CASE-1 KEYVAR CLAUSES)
            (IF (ATOM? CLAUSES) '**CASE-FELL-OFF-END**
              (LET ((CLAUSE (CAR CLAUSES))
                    (LOSE (LAMBDA () (SYNTAX-ERROR "bad ~S clause syntax: ~S"
                                                   'CASE
                                                   (CAR CLAUSES)))))
                (COND ((ATOM? CLAUSE) (LOSE))
                      ((LIST? (CAR CLAUSE))
                       `(,(t-syntax 'IF)
			 (,(t-syntax 'OR)
			  ,@(MAP (LAMBDA (K) `(EQ? ,KEYVAR ',K)) ; EQUIV?
				 (CAR CLAUSE)))
			 ,(BLOCKIFY (CDR CLAUSE))
			 ,(EXPAND-CASE-1 KEYVAR (CDR CLAUSES))))
                      ((EQ? (CAR CLAUSE) 'ELSE) (BLOCKIFY (CDR CLAUSE)))
                      (ELSE (LOSE)))))))
    (LET ((KEYVAR (GENERATE-SYMBOL 'CASE)))
      `((,(t-syntax 'LAMBDA) (,KEYVAR)
          ,(EXPAND-CASE-1 KEYVAR CLAUSES))
        ,KEY))))

(DEFINE-SYNTAX (XCASE KEY . CLAUSES)
  (COND ((ASSQ 'ELSE CLAUSES)
	 (SYNTAX-ERROR "~S expression has ~S clause~%  ~S"
		       'XCASE 'ELSE `(XCASE ,KEY ,@CLAUSES))))
  `(,(t-syntax 'CASE) ,KEY ,@CLAUSES (ELSE (NO-OP (LOSING-XCASE)))))

(DEFINE (LOSING-XCASE)
  (ERROR "no clause selected in ~S expression" 'XCASE))

(DEFINE **SELECT-FELL-OFF-END** '**SELECT-FELL-OFF-END**)

(DEFINE-SYNTAX (SELECT KEY . CLAUSES)
  (LABELS (((EXPAND-SELECT-1 KEYVAR CLAUSES)
            (IF (ATOM? CLAUSES) '**SELECT-FELL-OFF-END**
              (LET ((CLAUSE (CAR CLAUSES))
                    (LOSE (LAMBDA () (SYNTAX-ERROR "bad ~S clause syntax: ~S"
                                                   'SELECT
                                                   (CAR CLAUSES)))))
                (COND ((ATOM? CLAUSE) (LOSE))
                      ((LIST? (CAR CLAUSE))
                       `(,(t-syntax 'IF)
			 (,(t-syntax 'OR)
			  ,@(MAP (LAMBDA (K) `(EQ? ,KEYVAR ,K))  ; EQUIV?
				 (CAR CLAUSE)))
			 ,(BLOCKIFY (CDR CLAUSE))
			 ,(EXPAND-SELECT-1 KEYVAR (CDR CLAUSES))
			 ))
                      ((EQ? (CAR CLAUSE) 'ELSE) (BLOCKIFY (CDR CLAUSE)))
                      (ELSE (LOSE)))))))
    (LET ((KEYVAR (GENERATE-SYMBOL 'SELECT)))
      `((,(t-syntax 'LAMBDA) (,KEYVAR)
          ,(EXPAND-SELECT-1 KEYVAR CLAUSES))
        ,KEY))))

(DEFINE-SYNTAX (XSELECT KEY . CLAUSES)
  (COND ((ASSQ 'ELSE CLAUSES)
	 (SYNTAX-ERROR "~S expression has ~S clause~%  ~S"
		       'XSELECT 'ELSE `(XSELECT ,KEY ,@CLAUSES))))
  `(,(t-syntax 'SELECT) ,KEY ,@CLAUSES (ELSE (NO-OP (LOSING-XSELECT)))))

(DEFINE (LOSING-XSELECT)
  (ERROR "no clause selected in ~S expression" 'XSELECT))
