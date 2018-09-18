(HERALD GUARD
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Guards

;;; Guards: the ultimate in modularity.
;;; (Gimme a break.)

;;; A GUARD expression evaluates to a coercion routine that converts
;;; an unsafe routine into a safe one.  Amazing.

;;; Examples:
;;;  A no-op guard:
;;;   (GUARD () (PROC) (TRUE) TRUE)
;;;  A guard which makes sure that both of two args are fixnums:
;;;   (GUARD () (PROC) (FIXNUM? FIXNUM?) TRUE)
;;;  A guard which makes sure that both args are of given type:
;;;   (GUARD () (PROC TYPE) (TYPE TYPE) TRUE)
;;;  A guard for VREF:
;;;   (GUARD () (PROC) (VECTOR? NONNEGATIVE-FIXNUM?)
;;;           (LAMBDA (VEC IDX) (< IDX (VECTOR-LENGTH VEC))))
;;;  A guard for any VREF-like thing:
;;;   (GUARD () (PROC TYPE LENGTHFN)
;;;           (LAMBDA (THING IDX) (< IDX (LENGTHFN THING))))

;;; E.g. one might define a primop as follows:
;;;   (DEFINE FOO ((GUARD () (PROC) (type1 type2 ...) consistent?)
;;;                (LAMBDA (arg1 arg2 ...) ((PRIMOP FOO) arg1 arg2 ...))))

;;; Think of a better name.  CENSOR / MEMBRANE / DIAPER / SHIELD /
;;; CONDOM / PROPHYLACTIC / PROTECTOR / GASMASK / SPACESUIT /
;;; CHASTITYBELT (because they keep you from getting screwed) / ARMOR /
;;; EXOSKELETON / CHITON / INSURER / UNDERWRITER / GATEKEEPER / EPA /
;;; AGENT / SENTRY / SENTINEL

;;; I like CENSOR.

(DEFINE-SYNTAX (GUARD NAME PARAMS ARGTYPES CONSISTENT? . MAYBE-RESULT-TYPE)
  (IGNORE MAYBE-RESULT-TYPE)
  (LET ((ARGVARS (MAP (LAMBDA (X) (IGNORE X) (GENERATE-SYMBOL 'GUARD)) ARGTYPES))
        (PROCVAR (CAR PARAMS)))
    `(NAMED-LAMBDA ,NAME ,PARAMS
       (OBJECT (LAMBDA ,ARGVARS
                 (LET ,(MAP (LAMBDA (GENVAR ARGSPEC)
                               `(,GENVAR (CHECK-ARG ,ARGSPEC
                                                    ,GENVAR
                                                    ,PROCVAR)))
                            ARGVARS
                            ARGTYPES)
                   (IF (NOT (,CONSISTENT? ,@ARGVARS))
                       (INCONSISTENT-ARGS ,PROCVAR ,@ARGVARS)
                     (,PROCVAR ,@ARGVARS))))
               ((IDENTIFICATION SELF) (IDENTIFICATION ,PROCVAR))
               ((UNGUARDED-VERSION SELF) ,PROCVAR)
               ))))

(DEFINE-SYNTAX (DEFINE-GUARD PAT . REST)
  `(DEFINE ,(CAR PAT) (GUARD ,(CAR PAT) ,(CDR PAT) . ,REST)))

;;; Primop stuff:

;;; Random macro for notating unsafe primop procedures...

(DEFINE-SYNTAX (PRIMOP-PROC FOO ARGS)
  `(NAMED-LAMBDA ,FOO ,ARGS ((PRIMOP ,FOO) ,@ARGS)))

;;; Define a "safe" primop.

(DEFINE-SYNTAX (DEFINE-PRIMOP NAME . REST)
  `(DEFINE ,NAME (GUARDED-PRIMOP ,NAME . ,REST)))

;;; Notate a "safe" primop.

(DEFINE-SYNTAX (GUARDED-PRIMOP NAME ARGSPECS . CHECK)
  (LET ((VARS (MAP (LAMBDA (SPEC)
                     (COND ((AND (PAIR? SPEC) (CAR SPEC))
                            (CAR SPEC))
                           (ELSE (GENERATE-SYMBOL 'GUARDED-PRIMOP))))
                   ARGSPECS))
        (TYPES (MAP (LAMBDA (SPEC) (IF (PAIR? SPEC) (CADR SPEC) SPEC))
                    ARGSPECS)))
    `(,(XCASE (LENGTH ARGSPECS)
         ((0) 'ZERO-ARG-PRIMOP)
         ((1) 'ONE-ARG-PRIMOP)
         ((2) 'TWO-ARG-PRIMOP)
         ((3) 'THREE-ARG-PRIMOP)
         ((4) 'FOUR-ARG-PRIMOP))
      (PRIMOP-PROC ,NAME ,VARS)
      ,(IF (NULL? CHECK) 'TRUE
         `(LAMBDA ,VARS (IGNORABLE . ,VARS) . ,CHECK))
      ,@TYPES)))
