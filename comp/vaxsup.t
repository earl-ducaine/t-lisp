(herald vaxsup (env tcomp))
;This file is automatically generated by a shell script.
(set *namespace* *standard-support-env*)
;;; VAX TC support file for #[Filename VULCAN TSYS VAXSLINK T 0]
;;; TC version 1.4 (64)

(COMMENT (SOURCE-FILENAME #[Filename VULCAN TSYS VAXSLINK T 0]))
(COMMENT (HERALDED-FILENAME #[Filename () () VAXSLINK]))
(PUT 'PARANOID-ICALL 'SLINK-OFFSET -124)
(PUT 'CONFIDENT-ICALL 'SLINK-OFFSET -120)
(PUT 'ICALL 'SLINK-OFFSET -116)
(PUT 'IRETURN 'SLINK-OFFSET -112)
(PUT 'IAPPLY 'SLINK-OFFSET -108)
(PUT 'LEXPR-SETUP 'SLINK-OFFSET -104)
(PUT 'INAPPLICABLE 'SLINK-OFFSET -100)
(PUT 'COMPILER-LOSSAGE 'SLINK-OFFSET -96)
(PUT 'STACK-LIMIT 'SLINK-OFFSET -88)
(PUT 'HEAP-LIMIT 'SLINK-OFFSET -84)
(PUT 'JUMP-FROM 'SLINK-OFFSET -80)
(PUT 'BITS 'SLINK-OFFSET -76)
(PUT 'ADDRESS-MASK 'SLINK-OFFSET -68)
(PUT 'LOW-11-BITS 'SLINK-OFFSET -64)
(PUT 'LOW-19-BITS 'SLINK-OFFSET -60)
(PUT 'VFRAME-TEMPLATE 'SLINK-OFFSET -56)
(PUT 'TRUE 'SLINK-OFFSET -52)
(PUT 'NULL-CHARACTER 'SLINK-OFFSET -48)
(PUT 'ESCAPE-PROCEDURE-TEMPLATE 'SLINK-OFFSET -44)
(PUT 'SYMBOL-TEMPLATE 'SLINK-OFFSET -40)
(PUT 'VCELL-TEMPLATE 'SLINK-OFFSET -36)
(PUT 'VECTOR-TEMPLATE 'SLINK-OFFSET -32)
(PUT 'TEMPLATE-GUTS 'SLINK-OFFSET -24)
(PUT 'SAVED-HP 'SLINK-OFFSET -20)
(PUT 'SAVED-AP 'SLINK-OFFSET -16)
(PUT 'SAVED-FP 'SLINK-OFFSET -12)
(PUT 'NULL 'SLINK-OFFSET 0)
(PUT (QUOTE FALSE) (QUOTE SLINK-OFFSET) 0)(PUT 'PARANOID-IRETURN 'SLINK-OFFSET 12)
(PUT 'CONFIDENT-IRETURN 'SLINK-OFFSET 16)
(CPUT '*SLINK-TEMPLATE* 'DEFINED T)
(SYSBUILD-ITEM VALUE *THE-SLINK* THE-SLINK)
(CPUT '%%ICALL-INDEX 'CONSTANT (QUOTE -29))
(CPUT '%%ICALL-INDEX 'DEFINED T)
(CPUT '%%PARANOID-ICALL-INDEX 'CONSTANT (QUOTE -31))
(CPUT '%%PARANOID-ICALL-INDEX 'DEFINED T)
(CPUT '%%CONFIDENT-ICALL-INDEX 'CONSTANT (QUOTE -30))
(CPUT '%%CONFIDENT-ICALL-INDEX 'DEFINED T)
(CPUT '%%IRETURN-INDEX 'CONSTANT (QUOTE -28))
(CPUT '%%IRETURN-INDEX 'DEFINED T)
(CPUT '%%PARANOID-IRETURN-INDEX 'CONSTANT (QUOTE 3))
(CPUT '%%PARANOID-IRETURN-INDEX 'DEFINED T)
(CPUT '%%CONFIDENT-IRETURN-INDEX 'CONSTANT (QUOTE 4))
(CPUT '%%CONFIDENT-IRETURN-INDEX 'DEFINED T)
(CPUT '%%IAPPLY-INDEX 'CONSTANT (QUOTE -27))
(CPUT '%%IAPPLY-INDEX 'DEFINED T)
(CPUT '%%TRUE-INDEX 'CONSTANT (QUOTE -13))
(CPUT '%%TRUE-INDEX 'DEFINED T)
(CPUT '%%JUMP-FROM-INDEX 'CONSTANT (QUOTE -20))
(CPUT '%%JUMP-FROM-INDEX 'DEFINED T)
(CPUT '%%HEAP-LIMIT-INDEX 'CONSTANT (QUOTE -21))
(CPUT '%%HEAP-LIMIT-INDEX 'DEFINED T)
(CPUT '%%STACK-LIMIT-INDEX 'CONSTANT (QUOTE -22))
(CPUT '%%STACK-LIMIT-INDEX 'DEFINED T)
(CPUT '%%SAVED-HP-INDEX 'CONSTANT (QUOTE -5))
(CPUT '%%SAVED-HP-INDEX 'DEFINED T)
(SYSBUILD-ITEM STATIC *SLINK-TEMPLATE* DEF)
(SYSBUILD-ITEM STATIC %%ICALL-INDEX DEF)
(SYSBUILD-ITEM STATIC %%PARANOID-ICALL-INDEX DEF)
(SYSBUILD-ITEM STATIC %%CONFIDENT-ICALL-INDEX DEF)
(SYSBUILD-ITEM STATIC %%IRETURN-INDEX DEF)
(SYSBUILD-ITEM STATIC %%PARANOID-IRETURN-INDEX DEF)
(SYSBUILD-ITEM STATIC %%CONFIDENT-IRETURN-INDEX DEF)
(SYSBUILD-ITEM STATIC %%IAPPLY-INDEX DEF)
(SYSBUILD-ITEM STATIC %%TRUE-INDEX DEF)
(SYSBUILD-ITEM STATIC %%JUMP-FROM-INDEX DEF)
(SYSBUILD-ITEM STATIC %%HEAP-LIMIT-INDEX DEF)
(SYSBUILD-ITEM STATIC %%STACK-LIMIT-INDEX DEF)
(SYSBUILD-ITEM STATIC %%SAVED-HP-INDEX DEF)
(SYSBUILD-ITEM STATIC *THE-SLINK* DEF)
(SYSBUILD-ITEM SYMBOL VAXSLINK)
(SYSBUILD-ITEM SYMBOL T)
(SYSBUILD-ITEM SYMBOL **FILENAME**)
(SYSBUILD-ITEM SYMBOL VULCAN)
(SYSBUILD-ITEM SYMBOL TSYS)
(SYSBUILD-ITEM SYMBOL ENV)
(SYSBUILD-ITEM SYMBOL PRE-COOK)

;;; VAX TC support file for #[Filename VULCAN TSYS ALIASES T 0]
;;; TC version 1.4 (68)

(COMMENT (SOURCE-FILENAME #[Filename VULCAN TSYS ALIASES T 0]))
(COMMENT (HERALDED-FILENAME #[Filename () TSYS ALIASES T 24]))
(SYSBUILD-ITEM END)
(CPUT 'CHAR 'INTEGRABLE-FUNCTION (QUOTE STRING-HEAD))
(CPUT 'CHAR 'DEFINED T)
(CPUT 'CHDR 'INTEGRABLE-FUNCTION (QUOTE STRING-TAIL))
(CPUT 'CHDR 'DEFINED T)
(CPUT 'CHDR! 'INTEGRABLE-FUNCTION (QUOTE STRING-TAIL!))
(CPUT 'CHDR! 'DEFINED T)
(CPUT 'NTHCHDR 'INTEGRABLE-FUNCTION (QUOTE STRING-NTHTAIL))
(CPUT 'NTHCHDR 'DEFINED T)
(CPUT 'NTHCHDR! 'INTEGRABLE-FUNCTION (QUOTE STRING-NTHTAIL!))
(CPUT 'NTHCHDR! 'DEFINED T)
(CPUT 'NTHCHAR 'INTEGRABLE-FUNCTION (QUOTE STRING-ELT))
(CPUT 'NTHCHAR 'DEFINED T)
(CPUT 'CHAR+ 'INTEGRABLE-FUNCTION (QUOTE POINTER-ADD))
(CPUT 'CHAR+ 'DEFINED T)
(CPUT 'CHAR- 'INTEGRABLE-FUNCTION (QUOTE POINTER-SUBTRACT))
(CPUT 'CHAR- 'DEFINED T)
(CPUT 'FX+ 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-ADD))
(CPUT 'FX+ 'DEFINED T)
(CPUT 'FX- 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-SUBTRACT))
(CPUT 'FX- 'DEFINED T)
(CPUT 'FX* 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-MULTIPLY))
(CPUT 'FX* 'DEFINED T)
(CPUT 'FX/ 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-DIVIDE))
(CPUT 'FX/ 'DEFINED T)
(CPUT 'FX= 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-EQUAL?))
(CPUT 'FX= 'DEFINED T)
(CPUT 'FX< 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-LESS?))
(CPUT 'FX< 'DEFINED T)
(CPUT 'FX> 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-GREATER?))
(CPUT 'FX> 'DEFINED T)
(CPUT 'FXN= 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-NOT-EQUAL?))
(CPUT 'FXN= 'DEFINED T)
(CPUT 'FX>= 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-NOT-LESS?))
(CPUT 'FX>= 'DEFINED T)
(CPUT 'FX<= 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-NOT-GREATER?))
(CPUT 'FX<= 'DEFINED T)
(CPUT 'FX1+ 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-ADD1))
(CPUT 'FX1+ 'DEFINED T)
(CPUT 'FXREM 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-REMAINDER))
(CPUT 'FXREM 'DEFINED T)
(CPUT 'FL+ 'INTEGRABLE-FUNCTION (QUOTE FLONUM-ADD))
(CPUT 'FL+ 'DEFINED T)
(CPUT 'FL- 'INTEGRABLE-FUNCTION (QUOTE FLONUM-SUBTRACT))
(CPUT 'FL- 'DEFINED T)
(CPUT 'FL* 'INTEGRABLE-FUNCTION (QUOTE FLONUM-MULTIPLY))
(CPUT 'FL* 'DEFINED T)
(CPUT 'FL/ 'INTEGRABLE-FUNCTION (QUOTE FLONUM-DIVIDE))
(CPUT 'FL/ 'DEFINED T)
(CPUT 'FL= 'INTEGRABLE-FUNCTION (QUOTE FLONUM-EQUAL?))
(CPUT 'FL= 'DEFINED T)
(CPUT 'FL< 'INTEGRABLE-FUNCTION (QUOTE FLONUM-LESS?))
(CPUT 'FL< 'DEFINED T)
(CPUT 'FL> 'INTEGRABLE-FUNCTION (QUOTE FLONUM-GREATER?))
(CPUT 'FL> 'DEFINED T)
(CPUT 'FLN= 'INTEGRABLE-FUNCTION (QUOTE FLONUM-NOT-EQUAL?))
(CPUT 'FLN= 'DEFINED T)
(CPUT 'FL>= 'INTEGRABLE-FUNCTION (QUOTE FLONUM-NOT-LESS?))
(CPUT 'FL>= 'DEFINED T)
(CPUT 'FL<= 'INTEGRABLE-FUNCTION (QUOTE FLONUM-NOT-GREATER?))
(CPUT 'FL<= 'DEFINED T)
(CPUT 'BREF 'INTEGRABLE-FUNCTION (QUOTE BYTEV-ELT))
(CPUT 'BREF 'DEFINED T)
(CPUT 'BREF-8 'INTEGRABLE-FUNCTION (QUOTE BYTEV-ELT-8))
(CPUT 'BREF-8 'DEFINED T)
(CPUT 'BREF-16 'INTEGRABLE-FUNCTION (QUOTE BYTEV-ELT-16))
(CPUT 'BREF-16 'DEFINED T)
(CPUT 'BREF-32 'INTEGRABLE-FUNCTION (QUOTE BYTEV-ELT-32))
(CPUT 'BREF-32 'DEFINED T)
(CPUT 'BREF-POINTER 'INTEGRABLE-FUNCTION (QUOTE BYTEV-ELT-POINTER))
(CPUT 'BREF-POINTER 'DEFINED T)
(CPUT 'VREF 'INTEGRABLE-FUNCTION (QUOTE VECTOR-ELT))
(CPUT 'VREF 'DEFINED T)
(CPUT 'VSET 'INTEGRABLE-FUNCTION (QUOTE SET-VECTOR-ELT))
(CPUT 'VSET 'DEFINED T)
(CPUT 'XREF 'INTEGRABLE-FUNCTION (QUOTE EXTEND-ELT))
(CPUT 'XREF 'DEFINED T)
(CPUT 'XSET 'INTEGRABLE-FUNCTION (QUOTE SET-EXTEND-ELT))
(CPUT 'XSET 'DEFINED T)
(CPUT 'XENOID->INTEGER 'DEFINED T)

;;; VAX TC support file for #[Filename VULCAN TSYS OPEN T 0]
;;; TC version 1.4 (65)

(COMMENT (SOURCE-FILENAME #[Filename VULCAN TSYS OPEN T 0]))
(COMMENT (HERALDED-FILENAME #[Filename () TSYS OPEN T 108]))
(SYSBUILD-ITEM END)
(CPUT 'NIL 'CONSTANT NIL)
(CPUT 'NIL 'DEFINED T)
(CPUT 'T 'CONSTANT T)
(CPUT 'T 'DEFINED T)
(CPUT 'ELSE 'CONSTANT (QUOTE ELSE))
(CPUT 'ELSE 'DEFINED T)
(CPUT '*NULL-CHAR* 'CONSTANT (QUOTE #\NULL))
(CPUT '*NULL-CHAR* 'DEFINED T)
(CPUT '*NUMBER-OF-CHAR-CODES* 'CONSTANT (QUOTE 256))
(CPUT '*NUMBER-OF-CHAR-CODES* 'DEFINED T)
(CPUT '*STRING-LENGTH-LIMIT* 'CONSTANT (QUOTE 32767))
(CPUT '*STRING-LENGTH-LIMIT* 'DEFINED T)
(CPUT 'NOT 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF X (QUOTE ()) (QUOTE
T)))))
(CPUT 'NOT 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FALSE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF X (QUOTE ()) (QUOTE
T)))))
(CPUT 'FALSE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'TRUE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF X (QUOTE T) (QUOTE
())))))
(CPUT 'TRUE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'BOOLEAN? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (OR-AUX (EQ? X (QUOTE
T)) (LAMBDA () (EQ? X (QUOTE ())))))))
(CPUT 'BOOLEAN? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'ALWAYS 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (K) (LAMBDA IGNORED K))))
(CPUT 'ALWAYS 'DEFINED (QUOTE (1 . 1)))
(CPUT 'PROJ0 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X . IGNORED) X)))
(CPUT 'PROJ0 'DEFINED (QUOTE (1 . 9999)))
(CPUT 'PROJ1 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (() Y . IGNORED) Y)))
(CPUT 'PROJ1 'DEFINED (QUOTE (2 . 9999)))
(CPUT 'PROJ2 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (() () Z . IGNORED) Z)))
(CPUT 'PROJ2 'DEFINED (QUOTE (3 . 9999)))
(CPUT 'PROJ3 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (() () () W . IGNORED)
W)))
(CPUT 'PROJ3 'DEFINED (QUOTE (4 . 9999)))
(CPUT 'PROJN 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (N) (LAMBDA ARGLIST (NTH
ARGLIST N)))))
(CPUT 'PROJN 'DEFINED (QUOTE (1 . 1)))
(CPUT 'IDENTITY 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) X)))
(CPUT 'IDENTITY 'DEFINED (QUOTE (1 . 1)))
(CPUT 'COND-=>-AUX 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (P F A) (IF P ((F)
P) (A)))))
(CPUT 'COND-=>-AUX 'DEFINED (QUOTE (3 . 3)))
(CPUT 'OR-AUX 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (P R) (IF P P (R)))))
(CPUT 'OR-AUX 'DEFINED (QUOTE (2 . 2)))
(CPUT 'PROCLAIM 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (() OBJ) OBJ)))
(CPUT 'PROCLAIM 'DEFINED (QUOTE (2 . 2)))
(CPUT 'NULL? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF X (QUOTE ()) (QUOTE
T)))))
(CPUT 'NULL? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'ATOM? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (PAIR? X) (QUOTE
()) (QUOTE T)))))
(CPUT 'ATOM? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'LIST? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF X (PAIR? X) (QUOTE
T)))))
(CPUT 'LIST? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'NULL-LIST? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF X (IF (PAIR?
X) (QUOTE ()) (LOSING-NON-NULL-LIST X)) (QUOTE T)))))
(CPUT 'NULL-LIST? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'NONNEGATIVE-FIXNUM? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (FIXNUM?
X) (FIXNUM-NOT-NEGATIVE? X) (QUOTE ())))))
(CPUT 'NONNEGATIVE-FIXNUM? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'EOF? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (EQ? X *EOF*))))
(CPUT 'EOF? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'NEWLINE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (C) (EQ? C (QUOTE #\NEWLINE)))))
(CPUT 'NEWLINE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'VCELL? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND? X) (EQ?
(EXTEND-TEMPLATE X) *VCELL-TEMPLATE*) (QUOTE ())))))
(CPUT 'VCELL? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'SYMBOL? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND? X) (EQ?
(EXTEND-TEMPLATE X) (SYMBOL-TEMPLATE)) (QUOTE ())))))
(CPUT 'SYMBOL? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'VECTOR? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND? X) (EQ?
(EXTEND-TEMPLATE X) (VECTOR-TEMPLATE)) (QUOTE ())))))
(CPUT 'VECTOR? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'BITV? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND? X) (EQ?
(EXTEND-TEMPLATE X) *BITV-TEMPLATE*) (QUOTE ())))))
(CPUT 'BITV? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'BYTEV? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND? X) (EQ?
(EXTEND-TEMPLATE X) *BYTEV-TEMPLATE*) (QUOTE ())))))
(CPUT 'BYTEV? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'ESCAPE-PROCEDURE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND?
X) (EQ? (EXTEND-TEMPLATE X) *ESCAPE-PROCEDURE-TEMPLATE*) (QUOTE ())))))
(CPUT 'ESCAPE-PROCEDURE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'UNIT? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND? X) (EQ?
(EXTEND-TEMPLATE X) *UNIT-TEMPLATE*) (QUOTE ())))))
(CPUT 'UNIT? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'XENOID? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND? X) (EQ?
(EXTEND-TEMPLATE X) *XENOID-TEMPLATE*) (QUOTE ())))))
(CPUT 'XENOID? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'BOGUS-ENTITY? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (EXTEND?
X) (EQ? (EXTEND-TEMPLATE X) *BOGUS-ENTITY-TEMPLATE*) (QUOTE ())))))
(CPUT 'BOGUS-ENTITY? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'SET-VECTOR-ELT 'INTEGRABLE-FUNCTION (QUOTE SET-EXTEND-ELT))
(CPUT 'SET-VECTOR-ELT 'DEFINED T)
(CPUT 'VECTOR-ELT 'INTEGRABLE-FUNCTION (QUOTE EXTEND-ELT))
(CPUT 'VECTOR-ELT 'DEFINED T)
(CPUT 'MEM? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (PRED OBJ LIST) (IF (MEM
PRED OBJ LIST) (QUOTE T) (QUOTE ())))))
(CPUT 'MEM? 'DEFINED (QUOTE (3 . 3)))
(CPUT 'MEMQ 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (OBJ LIST) (MEM EQ? OBJ
LIST))))
(CPUT 'MEMQ 'DEFINED (QUOTE (2 . 2)))
(CPUT 'MEMQ? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (OBJ LIST) (IF (MEM EQ?
OBJ LIST) (QUOTE T) (QUOTE ())))))
(CPUT 'MEMQ? 'DEFINED (QUOTE (2 . 2)))
(CPUT 'NON-EMPTY-STRING? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (STRING?
X) (IF (STRING-EMPTY? X) (QUOTE ()) (QUOTE T)) (QUOTE ())))))
(CPUT 'NON-EMPTY-STRING? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'CHAR? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (MISC? X) (POINTER-LESS?
X 256) (QUOTE ())))))
(CPUT 'CHAR? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'CHAR->ASCII 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (C) (POINTER-ADDRESS
C))))
(CPUT 'CHAR->ASCII 'DEFINED (QUOTE (1 . 1)))
(CPUT 'ASCII->CHAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (C) (MAKE-POINTER
C 7))))
(CPUT 'ASCII->CHAR 'DEFINED (QUOTE (1 . 1)))
(CPUT 'STRING-TAIL 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (S) (STRING-TAIL!
(CHOPY S)))))
(CPUT 'STRING-TAIL 'DEFINED (QUOTE (1 . 1)))
(CPUT 'STRING-NTHTAIL 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (S N) (STRING-NTHTAIL!
(CHOPY S) N))))
(CPUT 'STRING-NTHTAIL 'DEFINED (QUOTE (2 . 2)))
(CPUT 'ENV-LOOKUP 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (ENV IDENTIFIER LOCAL?
CREATE?) (ENV IDENTIFIER LOCAL? CREATE?))))
(CPUT 'ENV-LOOKUP 'DEFINED (QUOTE (4 . 4)))
(CPUT 'VALUE->NONVALUE 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (POINTER-ADD
X *NONVALUE-HACK*))))
(CPUT 'VALUE->NONVALUE 'DEFINED (QUOTE (1 . 1)))
(CPUT 'NONVALUE->VALUE 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (POINTER-SUBTRACT
X *NONVALUE-HACK*))))
(CPUT 'NONVALUE->VALUE 'DEFINED (QUOTE (1 . 1)))
(CPUT 'NONVALUE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (MISC? X) (POINTER-GREATER?
X *NONVALUE-HACK*) (QUOTE ())))))
(CPUT 'NONVALUE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'VCELL-NONVALUE 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (VCELL) (POINTER-SUBTRACT
(VCELL-CONTENTS VCELL) *NONVALUE-HACK*))))
(CPUT 'VCELL-NONVALUE 'DEFINED T)
(CPUT 'SET-VCELL-NONVALUE 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (VCELL X)
(LET ((SET.32 (POINTER-ADD X *NONVALUE-HACK*))) (BLOCK (SET-VCELL-CONTENTS
VCELL SET.32) SET.32)))))
(CPUT 'SET-VCELL-NONVALUE 'DEFINED (QUOTE (2 . 2)))
(CPUT 'VCELL-NONVALUE 'SETTER (QUOTE SET-VCELL-NONVALUE))
(CPUT 'VCELL-HAS-VALUE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (VCELL) (LET
((X.33 (VCELL-CONTENTS VCELL))) (LET ((IFALT.35 (LAMBDA () (QUOTE T))))
(IF (MISC? X.33) (IF (POINTER-GREATER? X.33 *NONVALUE-HACK*) (QUOTE ())
(IFALT.35)) (IFALT.35)))))))
(CPUT 'VCELL-HAS-VALUE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-ABS 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (N) (IF (FIXNUM-LESS?
N 0) (FIXNUM-NEGATE N) N))))
(CPUT 'FIXNUM-ABS 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-MIN 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X Y) (IF (FIXNUM-LESS?
X Y) X Y))))
(CPUT 'FIXNUM-MIN 'DEFINED (QUOTE (2 . 2)))
(CPUT 'FIXNUM-MAX 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X Y) (IF (FIXNUM-GREATER?
X Y) X Y))))
(CPUT 'FIXNUM-MAX 'DEFINED (QUOTE (2 . 2)))
(CPUT 'FIXNUM-POSITIVE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (FIXNUM-GREATER?
X 0))))
(CPUT 'FIXNUM-POSITIVE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-NEGATIVE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (FIXNUM-LESS?
X 0))))
(CPUT 'FIXNUM-NEGATIVE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-ZERO? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (FIXNUM-EQUAL?
X 0))))
(CPUT 'FIXNUM-ZERO? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-NOT-POSITIVE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (FIXNUM-NOT-GREATER?
X 0))))
(CPUT 'FIXNUM-NOT-POSITIVE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-NOT-NEGATIVE? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (FIXNUM-NOT-LESS?
X 0))))
(CPUT 'FIXNUM-NOT-NEGATIVE? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-NOT-ZERO? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (FIXNUM-NOT-EQUAL?
X 0))))
(CPUT 'FIXNUM-NOT-ZERO? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-ADD1 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (FIXNUM-ADD X
1))))
(CPUT 'FIXNUM-ADD1 'DEFINED (QUOTE (1 . 1)))
(CPUT 'FIXNUM-EVEN? 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X) (IF (FIXNUM-ODD?
X) (QUOTE ()) (QUOTE T)))))
(CPUT 'FIXNUM-EVEN? 'DEFINED (QUOTE (1 . 1)))
(CPUT 'CHANGE-TAG 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (OBJ FROM-TAG TO-TAG)
(POINTER-ADD OBJ (FIXNUM->POINTER (FIXNUM-SUBTRACT TO-TAG FROM-TAG))))))
(CPUT 'CHANGE-TAG 'DEFINED (QUOTE (3 . 3)))
(CPUT 'BUFFER-SIZE 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (BUFFER) (TEXT-LENGTH
(STRING-POINTER BUFFER)))))
(CPUT 'BUFFER-SIZE 'DEFINED (QUOTE (1 . 1)))
(CPUT '%VECTOR-REPLACE 'INTEGRABLE-FUNCTION (QUOTE COPY-MEM))
(CPUT '%VECTOR-REPLACE 'DEFINED T)

;;; VAX TC support file for #[Filename VULCAN TSYS VAXOPEN T 0]
;;; TC version 1.4 (65)

(COMMENT (SOURCE-FILENAME #[Filename VULCAN TSYS VAXOPEN T 0]))
(COMMENT (HERALDED-FILENAME #[Filename () () VAXOPEN]))
(SYSBUILD-ITEM END)
(CPUT 'STRING-TEXT 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (STRING) (POINTER-SUBTRACT
(STRING-POINTER STRING) (FIXNUM->POINTER (STRING-BASE STRING))))))
(CPUT 'STRING-TEXT 'DEFINED (QUOTE (1 . 1)))
(CPUT 'TEMPLATE-GUTS? 'INTEGRABLE-FUNCTION (QUOTE REL-ITEM?))
(CPUT 'TEMPLATE-GUTS? 'DEFINED T)
(CPUT 'POINTER-ASHL 'INTEGRABLE-FUNCTION (QUOTE POINTER-ASH))
(CPUT 'POINTER-ASHL 'DEFINED T)
(CPUT 'FIXNUM-ASHL 'INTEGRABLE-FUNCTION (QUOTE FIXNUM-ASH))
(CPUT 'FIXNUM-ASHL 'DEFINED T)
(CPUT 'FIXNUM-ASHR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X Y) (FIXNUM-ASH
X (FIXNUM-SUBTRACT 0 Y)))))
(CPUT 'FIXNUM-ASHR 'DEFINED (QUOTE (2 . 2)))
(CPUT 'POINTER-ASHR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X Y) (POINTER-ASH
X (FIXNUM-SUBTRACT 0 Y)))))
(CPUT 'POINTER-ASHR 'DEFINED (QUOTE (2 . 2)))
(CPUT 'BYTEV-ELT 'INTEGRABLE-FUNCTION (QUOTE MREF-8-U))
(CPUT 'BYTEV-ELT 'DEFINED T)
(CPUT 'BYTEV-ELT-8 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (BV I) (LET ((Z (MREF-8-U
BV I))) (IF (FIXNUM-NOT-LESS? Z 128) (FIXNUM-SUBTRACT Z 256) Z)))))
(CPUT 'BYTEV-ELT-8 'DEFINED T)
(CPUT 'BYTEV-ELT-8 'SETTER (QUOTE (SETTER MREF-8-U)))
(CPUT 'BYTEV-ELT-16 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (BV I) (MREF-16
BV (FIXNUM-ASH I -1)))))
(CPUT 'BYTEV-ELT-16 'DEFINED T)
(CPUT 'BYTEV-ELT-32 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (BV I) (MREF-32
BV (FIXNUM-ASH I -2)))))
(CPUT 'BYTEV-ELT-32 'DEFINED T)
(CPUT 'BYTEV-ELT-POINTER 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (BV I) (MREF
BV (FIXNUM-ASH I -2)))))
(CPUT 'BYTEV-ELT-POINTER 'DEFINED T)
(CPUT 'FIXNUM-REMAINDER 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X Y) (FIXNUM-SUBTRACT
X (FIXNUM-MULTIPLY (FIXNUM-DIVIDE X Y) Y)))))
(CPUT 'FIXNUM-REMAINDER 'DEFINED (QUOTE (2 . 2)))
(CPUT 'FIXNUM-LOGAND 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X Y) (FIXNUM-LOGANDC2
X (FIXNUM-LOGNOT Y)))))
(CPUT 'FIXNUM-LOGAND 'DEFINED (QUOTE (2 . 2)))
(CPUT 'POINTER-LOGAND 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (X Y) (POINTER-LOGANDC2
X (POINTER-LOGNOT Y)))))
(CPUT 'POINTER-LOGAND 'DEFINED (QUOTE (2 . 2)))

;;; VAX TC support file for #[Filename VULCAN TSYS CARCDR T 0]
;;; TC version 1.4 (64)

(COMMENT (SOURCE-FILENAME #[Filename VULCAN TSYS CARCDR T 0]))
(COMMENT (HERALDED-FILENAME #[Filename () () CARCDR]))
(SYSBUILD-ITEM END)
(CPUT 'CARCDRABLE? 'INTEGRABLE-FUNCTION (QUOTE LIST?))
(CPUT 'CARCDRABLE? 'DEFINED T)
(CPUT 'MAKE-C*R 'DEFINED (QUOTE (2 . 2)))
(CPUT 'CARCDR 'DEFINED (QUOTE (4 . 4)))
(CPUT 'MAKE-C*R-BITS 'DEFINED (QUOTE (1 . 1)))
(CPUT '*DEFINE-C*R 'DEFINED (QUOTE (1 . 1)))
(CPUT 'CAAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CAR SEXPR)))))
(CPUT 'CAAR 'DEFINED T)
(CPUT 'CAAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CAR SEXPR))
VALUE))))
(CPUT 'CADR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CDR SEXPR)))))
(CPUT 'CADR 'DEFINED T)
(CPUT 'CADR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CDR SEXPR))
VALUE))))
(CPUT 'CDAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CAR SEXPR)))))
(CPUT 'CDAR 'DEFINED T)
(CPUT 'CDAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CAR SEXPR))
VALUE))))
(CPUT 'CDDR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CDR SEXPR)))))
(CPUT 'CDDR 'DEFINED T)
(CPUT 'CDDR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CDR SEXPR))
VALUE))))
(CPUT 'CAAAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CAR (CAR
SEXPR))))))
(CPUT 'CAAAR 'DEFINED T)
(CPUT 'CAAAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CAR (CAR SEXPR)))
VALUE))))
(CPUT 'CAADR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CAR (CDR
SEXPR))))))
(CPUT 'CAADR 'DEFINED T)
(CPUT 'CAADR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CAR (CDR SEXPR)))
VALUE))))
(CPUT 'CADAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CDR (CAR
SEXPR))))))
(CPUT 'CADAR 'DEFINED T)
(CPUT 'CADAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CDR (CAR SEXPR)))
VALUE))))
(CPUT 'CADDR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CDR (CDR
SEXPR))))))
(CPUT 'CADDR 'DEFINED T)
(CPUT 'CADDR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CDR (CDR SEXPR)))
VALUE))))
(CPUT 'CDAAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CAR (CAR
SEXPR))))))
(CPUT 'CDAAR 'DEFINED T)
(CPUT 'CDAAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CAR (CAR SEXPR)))
VALUE))))
(CPUT 'CDADR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CAR (CDR
SEXPR))))))
(CPUT 'CDADR 'DEFINED T)
(CPUT 'CDADR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CAR (CDR SEXPR)))
VALUE))))
(CPUT 'CDDAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CDR (CAR
SEXPR))))))
(CPUT 'CDDAR 'DEFINED T)
(CPUT 'CDDAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CDR (CAR SEXPR)))
VALUE))))
(CPUT 'CDDDR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CDR (CDR
SEXPR))))))
(CPUT 'CDDDR 'DEFINED T)
(CPUT 'CDDDR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CDR (CDR SEXPR)))
VALUE))))
(CPUT 'CAAAAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CAR (CAR
(CAR SEXPR)))))))
(CPUT 'CAAAAR 'DEFINED T)
(CPUT 'CAAAAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CAR (CAR (CAR
SEXPR)))) VALUE))))
(CPUT 'CAAADR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CAR (CAR
(CDR SEXPR)))))))
(CPUT 'CAAADR 'DEFINED T)
(CPUT 'CAAADR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CAR (CAR (CDR
SEXPR)))) VALUE))))
(CPUT 'CAADAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CAR (CDR
(CAR SEXPR)))))))
(CPUT 'CAADAR 'DEFINED T)
(CPUT 'CAADAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CAR (CDR (CAR
SEXPR)))) VALUE))))
(CPUT 'CAADDR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CAR (CDR
(CDR SEXPR)))))))
(CPUT 'CAADDR 'DEFINED T)
(CPUT 'CAADDR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CAR (CDR (CDR
SEXPR)))) VALUE))))
(CPUT 'CADAAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CDR (CAR
(CAR SEXPR)))))))
(CPUT 'CADAAR 'DEFINED T)
(CPUT 'CADAAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CDR (CAR (CAR
SEXPR)))) VALUE))))
(CPUT 'CADADR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CDR (CAR
(CDR SEXPR)))))))
(CPUT 'CADADR 'DEFINED T)
(CPUT 'CADADR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CDR (CAR (CDR
SEXPR)))) VALUE))))
(CPUT 'CADDAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CDR (CDR
(CAR SEXPR)))))))
(CPUT 'CADDAR 'DEFINED T)
(CPUT 'CADDAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CDR (CDR (CAR
SEXPR)))) VALUE))))
(CPUT 'CADDDR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CAR (CDR (CDR
(CDR SEXPR)))))))
(CPUT 'CADDDR 'DEFINED T)
(CPUT 'CADDDR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CAR (CDR (CDR (CDR
SEXPR)))) VALUE))))
(CPUT 'CDAAAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CAR (CAR
(CAR SEXPR)))))))
(CPUT 'CDAAAR 'DEFINED T)
(CPUT 'CDAAAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CAR (CAR (CAR
SEXPR)))) VALUE))))
(CPUT 'CDAADR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CAR (CAR
(CDR SEXPR)))))))
(CPUT 'CDAADR 'DEFINED T)
(CPUT 'CDAADR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CAR (CAR (CDR
SEXPR)))) VALUE))))
(CPUT 'CDADAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CAR (CDR
(CAR SEXPR)))))))
(CPUT 'CDADAR 'DEFINED T)
(CPUT 'CDADAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CAR (CDR (CAR
SEXPR)))) VALUE))))
(CPUT 'CDADDR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CAR (CDR
(CDR SEXPR)))))))
(CPUT 'CDADDR 'DEFINED T)
(CPUT 'CDADDR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CAR (CDR (CDR
SEXPR)))) VALUE))))
(CPUT 'CDDAAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CDR (CAR
(CAR SEXPR)))))))
(CPUT 'CDDAAR 'DEFINED T)
(CPUT 'CDDAAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CDR (CAR (CAR
SEXPR)))) VALUE))))
(CPUT 'CDDADR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CDR (CAR
(CDR SEXPR)))))))
(CPUT 'CDDADR 'DEFINED T)
(CPUT 'CDDADR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CDR (CAR (CDR
SEXPR)))) VALUE))))
(CPUT 'CDDDAR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CDR (CDR
(CAR SEXPR)))))))
(CPUT 'CDDDAR 'DEFINED T)
(CPUT 'CDDDAR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CDR (CDR (CAR
SEXPR)))) VALUE))))
(CPUT 'CDDDDR 'INTEGRABLE-FUNCTION (QUOTE (LAMBDA (SEXPR) (CDR (CDR (CDR
(CDR SEXPR)))))))
(CPUT 'CDDDDR 'DEFINED T)
(CPUT 'CDDDDR 'SETTER (QUOTE (LAMBDA (SEXPR VALUE) (SET (CDR (CDR (CDR (CDR
SEXPR)))) VALUE))))

