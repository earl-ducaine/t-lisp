(HERALD (TSYS VAXPRIM T 42)
        (ENV () (TSYS OPEN) (TSYS ALIASES))
        (SYNTAX-TABLE
         (BLOCK (*REQUIRE NIL '(TSYS GUARD) (THE-ENVIRONMENT))
                (ENV-SYNTAX-TABLE (THE-ENVIRONMENT)))))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Primop arithmetic functions (VAX)

;;; Binary comparisons:
;;; We could in theory assert BOOLEAN? on the result of the call to FN, for
;;; the benefit of the (currently non-existent) assertion system.

(DEFINE-LOCAL-SYNTAX (DEFINE-BINARY-COMPARISON FN TYPE)
  (COND ((EQ? TYPE 'TRUE)
         `(DEFINE ,FN (PRIMOP-PROC ,FN (X Y))))
        (ELSE
         `(DEFINE-PRIMOP ,FN (,TYPE ,TYPE)))))

;;; Universal:

(DEFINE-BINARY-COMPARISON EQ?  TRUE)
(DEFINE-BINARY-COMPARISON NEQ? TRUE)

(DEFINE-BINARY-COMPARISON POINTER-EQUAL?    TRUE)
(DEFINE-BINARY-COMPARISON POINTER-LESS?     TRUE)
(DEFINE-BINARY-COMPARISON POINTER-GREATER?  TRUE)
(DEFINE-BINARY-COMPARISON POINTER-NOT-EQUAL?   TRUE)
(DEFINE-BINARY-COMPARISON POINTER-NOT-LESS?    TRUE)
(DEFINE-BINARY-COMPARISON POINTER-NOT-GREATER? TRUE)

;;; Not universal:

(DEFINE-BINARY-COMPARISON CHAR=  CHAR?)
(DEFINE-BINARY-COMPARISON CHAR<  CHAR?)
(DEFINE-BINARY-COMPARISON CHAR>  CHAR?)
(DEFINE-BINARY-COMPARISON CHARN= CHAR?)
(DEFINE-BINARY-COMPARISON CHAR>= CHAR?)
(DEFINE-BINARY-COMPARISON CHAR<= CHAR?)

(DEFINE-BINARY-COMPARISON FIXNUM-EQUAL?    FIXNUM?)
(DEFINE-BINARY-COMPARISON FIXNUM-LESS?     FIXNUM?)
(DEFINE-BINARY-COMPARISON FIXNUM-GREATER?  FIXNUM?)
(DEFINE-BINARY-COMPARISON FIXNUM-NOT-EQUAL?   FIXNUM?)
(DEFINE-BINARY-COMPARISON FIXNUM-NOT-LESS?    FIXNUM?)
(DEFINE-BINARY-COMPARISON FIXNUM-NOT-GREATER? FIXNUM?)

(DEFINE-BINARY-COMPARISON FLONUM-EQUAL?    FLONUM?)
(DEFINE-BINARY-COMPARISON FLONUM-LESS?     FLONUM?)
(DEFINE-BINARY-COMPARISON FLONUM-GREATER?  FLONUM?)
(DEFINE-BINARY-COMPARISON FLONUM-NOT-EQUAL?   FLONUM?)
(DEFINE-BINARY-COMPARISON FLONUM-NOT-LESS?    FLONUM?)
(DEFINE-BINARY-COMPARISON FLONUM-NOT-GREATER? FLONUM?)

;;; XOP's
;;; We should provide for asserting result type.

(DEFINE-LOCAL-SYNTAX (DEFINE-STANDARD-XOP FN TYPE)
  (COND ((EQ? TYPE 'TRUE)
         `(DEFINE ,FN (PRIMOP-PROC ,FN (X))))
        (ELSE
         `(DEFINE-PRIMOP ,FN (,TYPE)))))

(DEFINE-STANDARD-XOP FLONUM->FIXNUM FLONUM?)
(DEFINE-STANDARD-XOP FIXNUM->FLONUM FIXNUM?)
(DEFINE-STANDARD-XOP FIXNUM-NEGATE  FIXNUM?)
(DEFINE-STANDARD-XOP FIXNUM-LOGNOT  FIXNUM?)
;(DEFINE-STANDARD-XOP POINTER-LOGNOT TRUE)      ; Useful?

;;; TOP's

(DEFINE-LOCAL-SYNTAX (DEFINE-STANDARD-TOP FN TYPE)
  (COND ((EQ? TYPE 'TRUE)
         `(DEFINE ,FN (PRIMOP-PROC ,FN (X Y))))
        (ELSE
         `(DEFINE-PRIMOP ,FN (,TYPE ,TYPE)))))

(DEFINE-STANDARD-TOP FIXNUM-ADD      FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-SUBTRACT FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-MULTIPLY FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-DIVIDE   FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-LOGIOR   FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-LOGXOR   FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-LOGANDC2 FIXNUM?)

(DEFINE-STANDARD-TOP FLONUM-ADD      FLONUM?)
(DEFINE-STANDARD-TOP FLONUM-SUBTRACT FLONUM?)
(DEFINE-STANDARD-TOP FLONUM-MULTIPLY FLONUM?)
(DEFINE-STANDARD-TOP FLONUM-DIVIDE   FLONUM?)

(DEFINE-STANDARD-TOP POINTER-ADD      TRUE)
(DEFINE-STANDARD-TOP POINTER-SUBTRACT TRUE)
(DEFINE-STANDARD-TOP POINTER-MULTIPLY TRUE)     ; Useful?
(DEFINE-STANDARD-TOP POINTER-DIVIDE   TRUE)     ; ?
(DEFINE-STANDARD-TOP POINTER-LOGIOR   TRUE)
(DEFINE-STANDARD-TOP POINTER-LOGXOR   TRUE)     ; ?
(DEFINE-STANDARD-TOP POINTER-LOGANDC2 TRUE)

(DEFINE-STANDARD-TOP POINTER-ASH      TRUE)

(DEFINE-STANDARD-TOP FIXNUM-ASH       FIXNUM?)

;;; Random

(DEFINE-PRIMOP FIXNUM-ODD? (FIXNUM?))

(DEFINE-PRIMOP FIXNUM-BIT? (FIXNUM? FIXNUM?))

;;; MREF and friends

(DEFINE (MAKE-MREF-PRIMOP P SET-P)
  (MAKE-ACCESSOR ((GUARD () (PROC) (TRUE FIXNUM?) TRUE)
                  P)
                 ((GUARD () (PROC) (TRUE FIXNUM? TRUE) TRUE) ; Ugh
                  SET-P)))

(DEFINE-LOCAL-SYNTAX (DEFINE-MREF-PRIMOP P)
  (LET ((SET-P (CONCATENATE-SYMBOL 'SET- P)))
    `(BLOCK (DEFINE ,P
              (MAKE-MREF-PRIMOP (PRIMOP-PROC ,P (VEC IDX))
                                (PRIMOP-PROC ,SET-P (VEC IDX VAL))))
            )))

(DEFINE-MREF-PRIMOP MREF-CHAR)
(DEFINE-MREF-PRIMOP MREF-8-U)   ; see comment in VAXOPEN.T
(DEFINE-MREF-PRIMOP MREF-16)
(DEFINE-MREF-PRIMOP MREF-32)
(DEFINE-MREF-PRIMOP MREF)

;;; Non-primops.

;;; (FIXNUM-HOWLONG n)
;;;   Returns the number of bits in N's binary representation.
;;;   Horrible name, after MACLISP function HAULONG.

(DEFINE-LAP-PROCEDURE FIXNUM-HOWLONG ((EXPR 1 0 0))
  (ROTL (LIT -3) (REG+ SP) R0)          "Depend on %%FIXNUM-TAG"
  (CLRL VAL)
  (BITL (LIT #x+0FFFF8000) R0)
  (BEQL HOWLONG1)
  (BISL2 (LIT (* 16. 8)) VAL)
  (ASHL (LIT -16.) R0 R0)
HOWLONG1
  (BITL (LIT #x+07F80) R0)
  (BEQL HOWLONG2)
  (BISL2 (LIT (* 8 8)) VAL)
  (ASHL (LIT -8) R0 R0)
HOWLONG2
  (BITL (LIT #x+078) R0)
  (BEQL HOWLONG3)
  (BISL2 (LIT (* 4 8)) VAL)
  (ASHL (LIT -4) R0 R0)
HOWLONG3
  (BITL (LIT #x+06) R0)
  (BEQL HOWLONG4)
  (BISL2 (LIT (* 2 8)) VAL)
  (ASHL (LIT -2) R0 R0)
HOWLONG4
  (BITL (LIT #x+01) R0)
  (BEQL HOWLONG5)
  (BISL2 (LIT (* 1 8)) VAL)
HOWLONG5
  (JMP (@ (SLINK IRETURN))))

(DEFINE-LAP-PROCEDURE FIXNUM-ADD-CARRY? ((EXPR 2 0 0))
  (ADDL3 (REG SP) (REG SP 4) R0)
  (BCC COMMON-RETURN-FALSE)
  (BRB COMMON-RETURN-TRUE))

(DEFINE-LAP-PROCEDURE FIXNUM-SUBTRACT-CARRY? ((EXPR 2 0 0))
  (SUBL3 (REG SP) (REG SP 4) R0)
  (BCC COMMON-RETURN-FALSE)
  (BRB COMMON-RETURN-TRUE))

(DEFINE-LAP-PROCEDURE FIXNUM-ADD-OVERFLOW? ((EXPR 2 0 0))
  (ADDL3 (REG SP) (REG SP 4) R0)
  (BVC COMMON-RETURN-FALSE)
  (BRB COMMON-RETURN-TRUE))

(DEFINE-LAP-PROCEDURE FIXNUM-SUBTRACT-OVERFLOW? ((EXPR 2 0 0))
  (SUBL3 (REG SP) (REG SP 4) R0)
  (BVC COMMON-RETURN-FALSE)
COMMON-RETURN-TRUE
  (MOVL (SLINK TRUE) VAL)
  (MOVAL (REG SP 8) SP)
  (JMP (@ (SLINK IRETURN)))
COMMON-RETURN-FALSE
  (MOVL (SLINK FALSE) VAL)
  (MOVAL (REG SP 8) SP)
  (JMP (@ (SLINK IRETURN))))

;;; (FLONUM-GUTS flonum cont)

(DEFINE-LAP-PROCEDURE FLONUM-GUTS ((EXPR 2 0 0))
  (MOVL (REG+ SP) FUN)          "Get continuation"
  (MOVL (REG+ SP) VAL)          "Get the flonum"
  (MOVZWL (REG VAL (- 0 %%FLONUM-TAG)) R0)
  (ASHL (LIT 3) R0 (-REG SP))
  (MOVZWL (REG VAL (- 2 %%FLONUM-TAG)) R0)
  (ASHL (LIT 3) R0 (-REG SP))
  (MOVZWL (REG VAL (- 4 %%FLONUM-TAG)) R0)
  (ASHL (LIT 3) R0 (-REG SP))
  (MOVZWL (REG VAL (- 6 %%FLONUM-TAG)) R0)
  (ASHL (LIT 3) R0 (-REG SP))
  (JMP (@ (SLINK ICALL))))

;;; (FIXNUMS-REPLACE-FLONUM flonum fixnum fixnum fixnum fixnum)

(DEFINE-LAP-PROCEDURE FIXNUMS-REPLACE-FLONUM ((EXPR 5 0 0))
  (MOVL (REG SP 16.) VAL)               "flonum"
  (ASHL (LIT -3) (REG SP) R0)
  (MOVW R0 (REG VAL (- 6 %%FLONUM-TAG)))
  (ASHL (LIT -3) (REG SP 4) R0)
  (MOVW R0 (REG VAL (- 4 %%FLONUM-TAG)))
  (ASHL (LIT -3) (REG SP 8) R0)
  (MOVW R0 (REG VAL (- 2 %%FLONUM-TAG)))
  (ASHL (LIT -3) (REG SP 12.) R0)
  (MOVW R0 (REG VAL (- 0 %%FLONUM-TAG)))
  (MOVAL (REG SP 20.) SP)
  (JMP (@ (SLINK IRETURN))))

;;; (DECODE-FLOAT N CONT)
;;;   Call 2nd argument passing it two numbers SIG and EXP such that
;;;   N = SIG * 2^EXP  and  -1 < SIG < 1.

(define-lap-procedure decode-float ((expr 2 0 0))
  ;; N = 4(SP), CONT = 0(SP)
  (movl (reg sp) fun)           ; Get continuation
  (movl (reg sp 4) xp)          ; Get the flonum
  (movaq (reg+ hp) val)         ; Cons a new flonum
  (adjust-tag val val %%flonum-tag)
  ;; SIG = 4(SP), EXP = (SP)
  (extzv (lit 7) (lit 8) (reg xp (~ %%flonum-tag)) r0)          ; Get exponent
  (subl2 (lit 128) r0)                                          ; Excess 128
  (ashl (lit 3) r0 (reg sp))                                    ; Normalize
  (movd (reg xp (~ %%flonum-tag)) (reg val (~ %%flonum-tag)))   ; Copy value
  (insv (lit 128) (lit 7) (lit 8) (reg val (~ %%flonum-tag)))   ; Clobber exp
  (movl val (reg sp 4))         ; Pass new flonum
  (moval (reg sp 8) ap)         ; Two args
  (jmp (@ (slink icall))))      ; Invoke continuation


;;; <n,s> means bit field of length s beginning at bit n of the first
;;; WORD (not longword)
;;;                    sign      exponent   MSB       fraction
;;; IEEE flonum        <15,1>    <4,11>     hidden    <0,4>+next 3 words
;;; VAX11 flonum (D)   <15,1>    <7,8>      hidden    <0,7>+next 3 words

(define (integer-decode-float x values)
  (flonum-guts x
       (lambda (a b c d)
         (values (+ d
                    (%ash (+ c
                             (%ash (fx+ b
                                      (fixnum-ashl
                                        (fx+ (fixnum-bit-field a 0 7)
                                             128)
                                        16))
                                   16))
                          16))
                 (fx- (fixnum-bit-field a 7 8) (fx+ 128 56))))))

;(format nil "~x" (integer-decode-float 1.0 list))

;(integer-decode-float 1.0 (lambda (s e) (* s (expt 2 e))))

;(define (foo x)
;  (flonum-guts x (lambda (a b c d)
;                   (format nil "~-4x.~-4x.~-4x.~-4x" a b c d))))
