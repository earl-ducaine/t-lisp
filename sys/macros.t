(herald (tsys macros t 147)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Standard macros

(define-syntax (define . x)
  (cond ((pair? (car x))
         `(,(t-syntax 'define-variable-value) ,(caar x)
	    (,(t-syntax 'named-lambda) ,(caar x) ,(cdar x) . ,(cdr x))))
        ((null? (cddr x))
         `(,(t-syntax 'define-variable-value) ,(car x) ,(cadr x)))
        (else
         (syntax-error "illegal definition syntax~%  ~S" `(define . ,x)))))

(define-syntax (define-integrable      . x) `(,(t-syntax 'define) . ,x))
(define-syntax (define-integrable-only . x) `(,(t-syntax 'define) . ,x))
(define-syntax (define-constant        . x) `(,(t-syntax 'define) . ,x))

(DEFINE-SYNTAX (IF-INTEGRATED INT NOT-INT)
  (IGNORE INT)
  NOT-INT)

(DEFINE-SYNTAX (DECLARE-SETTER VAR CROCK)
  (IGNORE VAR CROCK)
  ''DECLARE-SETTER)

(DEFINE-SYNTAX (SET PLACE VALUE)
  (LET ((VAR (GENERATE-SYMBOL 'SET)))
    `(LET ((,VAR ,VALUE))
       ,(COND ((ATOM? PLACE)
               `(,(t-syntax 'SET-VARIABLE-VALUE) ,PLACE ,VAR))
              (ELSE
               `((SETTER ,(CAR PLACE)) ,@(CDR PLACE) ,VAR)))
       ,VAR)))

(DEFINE (BLOCKIFY X)
  (COND ((ATOM? X) 'NIL)
        ((ATOM? (CDR X)) (CAR X))
        (ELSE `(,(t-syntax 'BLOCK) ,@X))))

(DEFINE-SYNTAX (BLOCK0 VAL . BODY)
  (LET ((G (GENERATE-SYMBOL 'BLOCK0)))
    `((,(t-syntax 'LAMBDA) (,G) ,@BODY ,G) ,VAL)))

;;; Expand into explicit LAMBDA's in the rhs's of LABELSes, so as to 
;;; increase the information content of BACKTRACE output.  (Otherwise the
;;; local procedures handle IDENTIFICATION, and you get lots of LOOP's and
;;; DO.137's in the backtrace.)

(DEFINE-SYNTAX (DO SPECS END . BODY)
  (LET ((LOOP (GENERATE-SYMBOL 'DO)))
    `(,(t-syntax 'LABELS)
             ((,LOOP (,(t-syntax 'LAMBDA) ,(MAP CAR SPECS)
                       (,(t-syntax 'COND) ,END
                             (ELSE ,(BLOCKIFY
                                     `(,@BODY
                                       (,LOOP
                                        ,@(MAP (LAMBDA (Y)
                                                 (IF (AND (CDR Y) (CDDR Y))
                                                     (CADDR Y)
                                                   (CAR Y)))
                                               SPECS)))))))))
       (,LOOP ,@(MAP (LAMBDA (Y) (IF (CDR Y) (CADR Y) 'NIL)) SPECS)))))

(DEFINE-SYNTAX (ITERATE CLAUSE-1 . REST)
  (LET ((FORM-ITERATE (LAMBDA (NAME VARINITS BODY)
                        `(,(t-syntax 'LABELS)
			      ((,NAME (,(t-syntax 'LAMBDA) ,(MAP CAR VARINITS)
					. ,BODY)))
                           (,NAME . ,(MAP CADR VARINITS))))))
    (IF (SYMBOL? CLAUSE-1)
        (FORM-ITERATE CLAUSE-1 (CAR REST) (CDR REST))
      (FORM-ITERATE (CAR CLAUSE-1) (CDR CLAUSE-1) REST))))

(DEFINE-SYNTAX (LOCATIVE FORM)
  (COND ((ATOM? FORM)
         `(,(t-syntax 'VAR-LOCATIVE) ,FORM))
        (ELSE
         `(MAKE-LOCATIVE ,@FORM))))     ; ??

(DEFINE-SYNTAX (DELAY X)
  `(MAKE-DELAY (,(t-syntax 'LAMBDA) () ,X)))

(DEFINE-SYNTAX (SYNONYM FOO)
  `(MAKE-SYNONYM (,(t-syntax 'LOCATIVE) ,FOO) ',(IF (SYMBOL? FOO) FOO '())))

;;; (UNWIND-PROTECT body . unwind-forms)

(DEFINE-SYNTAX (UNWIND-PROTECT BODY . UNWIND-FORMS)
  `(UNWIND-PROTECT-HANDLER (,(t-syntax 'LAMBDA) () ,BODY)
                           (,(t-syntax 'LAMBDA) () . ,UNWIND-FORMS)))

;;; (WITH-OUTPUT-TO-STRING var . body) binds var to an output stream and
;;;  executes the body.  Anything written to the output stream during the
;;;  execution of body is accumulated in a string, which is returned as the
;;;  value of the WITH-OUTPUT-TO-STRING-expression.

(DEFINE-SYNTAX (WITH-OUTPUT-TO-STRING PAT . BODY)
  (LET ((VAR (IF (PAIR? PAT) (CAR PAT) PAT)))
    `(,(t-syntax 'LET) ((,VAR (MAKE-OUTPUT-TO-STRING-STREAM)))
       ,@BODY
       (CLOSE ,VAR))))

(DEFINE-SYNTAX (WITH-INPUT-FROM-STRING PAT . BODY)
  (LET ((PAT (CHECK-ARG PAIR? PAT 'WITH-INPUT-FROM-STRING)))
    (LET ((VAR (CAR PAT))
          (STRING (CADR PAT)))
      `(,(t-syntax 'LET) ((,VAR (STRING->INPUT-STREAM ,STRING)))
         (,(t-syntax 'BLOCK0) ,(BLOCKIFY BODY)
			      (CLOSE ,VAR))))))

;;; (WITH-OUTPUT-TO-LIST var . body) is like
;;;  (STRING->LIST (WITH-OUTPUT-TO-STRING var . body)).
;;;  It could be implemented more efficiently but I'm too lazy to do so.

(DEFINE-SYNTAX (WITH-OUTPUT-TO-LIST VAR . BODY)
  `(STRING->LIST (WITH-OUTPUT-TO-STRING ,VAR . ,BODY)))

;;; (WITH-OUTPUT-WIDTH-STREAM var . body) is like WITH-OUTPUT-TO-STRING, but
;;;  instead of accumulating characters in a string, it counts them.  The
;;;  value returned is the number of characters counted.
;;; For example of use see below definition of PRINTWIDTH.

(DEFINE-SYNTAX (WITH-OUTPUT-WIDTH-STREAM VAR . BODY)
  `(,(t-syntax 'LET) ((,VAR (MAKE-OUTPUT-WIDTH-STREAM)))
     ,@BODY
     (CLOSE ,VAR)))

;;; WITH-OPEN-STREAMS

(DEFINE-SYNTAX (WITH-OPEN-STREAMS SPECS . BODY)
  `(WITH-OPEN-STREAMS-HANDLER (,(t-syntax 'LAMBDA) ,(MAP CAR SPECS)
                                . ,BODY) 
                              ,@(MAP (LAMBDA (SPEC)
                                       `(,(t-syntax 'LAMBDA) () ,(CADR SPEC)))
                                     SPECS)))

;;; Random

(DEFINE-SYNTAX (IMPORT ENV . VARS)
  (LET ((G (GENERATE-SYMBOL 'IMPORT)))
    `(,(t-syntax 'LET) ((,G ,ENV))
       ,@(MAP (LAMBDA (VAR)
                (LET ((VAR (CHECK-ARG SYMBOL? VAR 'IMPORT)))
                  `(,(t-syntax 'DEFINE) ,VAR (*VALUE ,G ',VAR))))
              VARS))))

;(DEFINE-SYNTAX (EXPORT ENV . VARS)
;  (LET ((G (GENERATE-SYMBOL 'EXPORT)))
;    `(LET ((G ,ENV))
;       ,@(MAP (LAMBDA (VAR) `(*DEFINE ,G ',VAR ,VAR))
;              VARS))))

(DEFINE-SYNTAX (REQUIRE NAME . MAYBE-PATH)
  (COND (MAYBE-PATH
         `(*REQUIRE ',NAME ',(CAR MAYBE-PATH) (,(t-syntax 'THE-ENVIRONMENT))))
        (ELSE
         `(*REQUIRE '() ',NAME (,(t-syntax 'THE-ENVIRONMENT))))))

(DEFINE-SYNTAX (CATCH VAR . BODY)
  `(CALL-WITH-CURRENT-CONTINUATION (,(t-syntax 'LAMBDA) (,VAR) . ,BODY)))

(DEFINE-SYNTAX (COMMENT . REST)
  (IGNORE REST)
  ''COMMENT)                            ; A tradition of sorts.

(DEFINE-SYNTAX (GC-DEFER . REST)         ; Yick!
  (BLOCKIFY REST))

(DEFINE-SYNTAX (CEVAL . REST)       ; Horror!
  (IGNORE REST)
  ''CEVAL)

(DEFINE-SYNTAX (HERALD . REST)
  (SYNTAX-ERROR "HERALD form in illegal context~%  ~S"
                `(HERALD . ,REST)))

;;; Syntax-related stuff

(DEFINE-SYNTAX (MACRO-EXPANDER PAT . REST)
  (LET* ((PAT (CHECK-ARG PAIR? PAT 'MACRO-EXPANDER))
         (SYMBOL (CAR PAT))
         (ARGS   (CDR PAT))
         (Z (GENERATE-SYMBOL 'MACRO)))
    `(MAKE-MACRO-DESCRIPTOR (,(t-syntax 'NAMED-LAMBDA) ,SYMBOL (,Z)
                              ;; Careful!  Look at DISCLOSE-MACRO-EXPANDER.
                              (,(t-syntax 'DESTRUCTURE) (((#F . ,ARGS) ,Z))
                                . ,REST))
                            ',(ARGLIST->ARGSPECTRUM ARGS)
                            ',SYMBOL)))

(DEFINE-SYNTAX (DEFINE-SYNTAX PAT . REST)
  (LET ((CONSTRUCT
         (LAMBDA (SYMBOL DESCR)
           (LET ((SYMBOL (CHECK-ARG SYMBOL? SYMBOL 'DEFINE-SYNTAX)))
             `(*DEFINE-SYNTAX (,(t-syntax 'THE-ENVIRONMENT))
			      ,SYMBOL
			      ,DESCR)))))
    (COND ((PAIR? PAT)
           (CONSTRUCT (CAR PAT)
                      `(,(t-syntax 'MACRO-EXPANDER) ,PAT . ,REST)))
          (ELSE
           (COND ((NOT (NULL? (CDR REST)))
                  (SYNTAX-ERROR "too many subforms~%  ~S"
                                `(DEFINE-SYNTAX ,PAT . ,REST))))
           (CONSTRUCT PAT (CAR REST))))))

(DEFINE-SYNTAX (DEFINE-MACRO PAT . REST)        ;TC's definition differs
  `(,(t-syntax 'DEFINE-SYNTAX) ,PAT . ,REST))
