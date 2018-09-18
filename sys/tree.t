(HERALD (TSYS TREE T 17)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Tree stuff.

;;; Utility used by system macros.

(DEFINE (MAKE-SYMBOL-GENERATOR)
  (LET ((COUNT 0))
    (OBJECT (LAMBDA (PREFIX)
              (SET COUNT (FX+ COUNT 1))
              (CONCATENATE-SYMBOL PREFIX "." COUNT))
            ((SYMBOL-GENERATOR-COUNT SELF) COUNT)
            (((SETTER SYMBOL-GENERATOR-COUNT) SELF VAL)
             (SET COUNT (CHECK-ARG NONNEGATIVE-FIXNUM?
                                   VAL
                                   SYMBOL-GENERATOR-COUNT)))
            ((PRINT-TYPE-STRING SELF) "Symbol-generator"))))

(DEFINE-SETTABLE-OPERATION (SYMBOL-GENERATOR-COUNT OBJ))

(DEFINE GENERATE-SYMBOL (MAKE-SYMBOL-GENERATOR))

(DEFINE (EQUIV? A B)
  (OR (EQ? A B)
      (LET ((TAG1 (POINTER-TAG A))
            (TAG2 (POINTER-TAG B)))
        (AND (FX= TAG1 TAG2)
             (SELECT TAG1
               ((%%STRING-TAG) (STRING-EQUAL? A B))
               ((%%FLONUM-TAG) (FL= A B))
               ((%%EXTEND-TAG) (AND (NUMBER? A) (NUMBER? B) (= A B)))
               (ELSE NIL))))))

(DEFINE (SUBST PRED NEW OLD EXP)
  (COND ((EQ? OLD EXP) NEW)             ; Pessimal microhack
        ((PAIR? EXP)
         (CONS (SUBST PRED NEW OLD (CAR EXP))
               (SUBST PRED NEW OLD (CDR EXP))))
        ((PRED OLD EXP) NEW)
        (ELSE EXP)))

(DEFINE-INTEGRABLE (SUBSTQ NEW OLD EXP) (SUBST EQ? NEW OLD EXP))

(DEFINE-INTEGRABLE (SUBSTV NEW OLD EXP) (SUBST EQUIV? NEW OLD EXP))

(DEFINE (COPY-TREE X) (SUBSTQ NIL NIL X))



(DEFINE (ALIKE? PRED EXP1 EXP2)
  (COND ((EQ? EXP1 EXP2) T)             ; speed hack
        ((ATOM? EXP1)
         (IF (ATOM? EXP2) (PRED EXP1 EXP2) NIL))
        ((ATOM? EXP2) NIL)
        ((ALIKE? PRED (CAR EXP1) (CAR EXP2))
         (ALIKE? PRED (CDR EXP1) (CDR EXP2)))
        (ELSE NIL)))

(DEFINE-INTEGRABLE (ALIKEQ? EXP1 EXP2) (ALIKE? EQ? EXP1 EXP2))  ; i like q, 2

(DEFINE-INTEGRABLE (ALIKEV? EXP1 EXP2) (ALIKE? EQUIV? EXP1 EXP2))


;;; TREE-HASH.  Maybe this should be an operation.

(DEFINE (TREE-HASH TREE)
  (LABELS (((HASH TREE)
            (COND ((PAIR? TREE)
                   (FX+ (HASH (CAR TREE))
                        (FX* (HASH (CDR TREE)) 2)))
                  ((SYMBOL? TREE)
                   (STRING-HASH (SYMBOL-PNAME TREE)))
                  ((STRING? TREE)
                   (STRING-HASH TREE))
                  ((NULL? TREE) 31415926)
                  ((CHAR? TREE)
                   (CHAR->ASCII TREE))
                  ((FIXNUM? TREE)
                   TREE)
                  (ELSE (HASH (ERROR "unhashable leaf~%  (~S ~S)"
                                     'TREE-HASH
                                     TREE))))))
    (FIXNUM-ABS (HASH TREE))))
