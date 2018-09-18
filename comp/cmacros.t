(HERALD (TCOMP CMACROS T 31)
        (ENV TCOMP))

;;; Copyright (c) 1983, 1984 Yale University

;;; Compiler macros.


(DEFINE-LOCAL-SYNTAX (DEFINE-COMPILER-MACRO PAT . BODY)
  (LET ((MAC (CAR PAT))
        (ARGLIST (CDR PAT)))
    `(BLOCK (SET-MACRO-DEFINITION ',MAC
               (LAMBDA (%%SEXPR%%)
                 (DESTRUCTURE (((,@ARGLIST) (CDR %%SEXPR%%)))
                   ,@BODY))
               ',((*VALUE *T-IMPLEMENTATION-ENV* 'ARGLIST->ARGSPECTRUM)
                  ARGLIST))
            ',MAC)))

;;;; Macros for standard special forms

(DEFINE-COMPILER-MACRO (BOUND? VAR)             ; Skafforp?  Banso!
  `(NOT (NONVALUE? ,VAR)))              ; depend on absence of error checking

;;; (DEFINE-LAP-PROCEDURE name (-options-) -body-)

(DEFINE-COMPILER-MACRO (DEFINE-LAP-PROCEDURE FN OPTIONS . REST) 
  (LET ((PTAG (MAKE-SPECIAL-TAG 'P FN))
        (TTAG (MAKE-SPECIAL-TAG 'T FN))
        (CTAG (MAKE-SPECIAL-TAG 'C FN)))
    `(DEFINE ,FN
       (LAP-PROCEDURE ,PTAG ,TTAG ,CTAG
                      ((DEFINEE ,FN) . ,OPTIONS)
                      . ,REST))))

;;; By special dispensation, LAP-PROCEDURE expressions evaluate to TPROC's.

(DEFINE-COMPILER-MACRO (LAP-PROCEDURE PTAG . REST)
  `',(QLOZURE (PTAG REST)
       (LAMBDA ()
         (GENERATE-LAP-TEMPLATE PTAG REST))
       ((FUNNY-CONSTANT? SELF) T)
       ((PRIN SELF STREAM) (FORMAT STREAM "#{Lap-procedure ...}"))
       ))

(DEFINE-COMPILER-MACRO (LAP-TEMPLATE . REST)
  `',(QLOZURE (REST)
       (LAMBDA ()
         (GENERATE-LAP-TEMPLATE NIL REST))
       ((FUNNY-CONSTANT? SELF) T)
       ((PRIN SELF STREAM) (FORMAT STREAM "#{Lap-template ...}"))
       ))

;;; Hack for xenoids.

(DEFINE-COMPILER-MACRO (%XENOID STRING)
  `',(QLOZURE (STRING)
       (LAMBDA () (GENERATE-XENOID STRING))
       ((FUNNY-CONSTANT? SELF) T)
       ((PRIN SELF STREAM) (FORMAT STREAM "#{Lap-xenoid ...}"))
       ))

;;; Hack.

(DEFINE-COMPILER-MACRO (IF-INTEGRATED CON ALT)
  (IF *INTEGRATED?* CON ALT))

;;; Horrible kludge.

(DEFINE-COMPILER-MACRO (DEFINE-INTEGRABLE-ONLY PAT . BODY)
  (COND ((PAIR? PAT)
         `(DEFINE-INTEGRABLE ,(CAR PAT)
            (IF-INTEGRATED (LAMBDA ,(CDR PAT) . ,BODY)
                           INTEGRABLE-ONLY)))
        (ELSE
         `(DEFINE-INTEGRABLE ,PAT
            (IF-INTEGRATED ,(CAR BODY)
                           INTEGRABLE-ONLY)))))
