(HERALD M68PRIMOPS
        (ENV () (TSYS ALIASES) (TSYS OPEN))
        (SYNTAX-TABLE
         (BLOCK (*REQUIRE 'GUARD '(TSYS GUARD) *SCRATCH-ENV*)
                (ENV-SYNTAX-TABLE *SCRATCH-ENV*))))

;;; Copyright (c) 1983, 1984 Yale University

;;; Random

(DEFINE-PRIMOP FIXNUM-ODD? (FIXNUM?))

(DEFINE-PRIMOP FIXNUM-BIT? (FIXNUM? NONNEGATIVE-FIXNUM?))

(DEFINE-PRIMOP FIXNUM-ASHR (FIXNUM? NONNEGATIVE-FIXNUM?))
(DEFINE-PRIMOP POINTER-ASHR (TRUE NONNEGATIVE-FIXNUM?))
(DEFINE-PRIMOP POINTER-ASHL (TRUE NONNEGATIVE-FIXNUM?))

(DEFINE (MAKE-VECTOR-PRIMOP PROC SET-PROC PRED LEN)
  (MAKE-ACCESSOR ((GUARD () (PROC) (PRED NONNEGATIVE-FIXNUM?)
                         (LAMBDA (VEC IDX)
                           (FX< IDX (LEN VEC))))
                  PROC)
                 ((GUARD () (PROC) (PRED NONNEGATIVE-FIXNUM? TRUE)      ; ugh
                         (LAMBDA (VEC IDX VAL)
                           (IGNORE VAL)
                           (FX< IDX (LEN VEC))))
                  SET-PROC)))

(DEFINE-LOCAL-SYNTAX (DEFINE-VECTOR-PRIMOP TYPE PROC)
  (LET ((SET-PROC (CONCATENATE-SYMBOL 'SET- PROC))
        (PRED (CONCATENATE-SYMBOL TYPE '?))
        (LEN (CONCATENATE-SYMBOL TYPE '-LENGTH)))
    `(DEFINE ,PROC
              (MAKE-VECTOR-PRIMOP (PRIMOP-PROC ,PROC (VEC IDX))
                                  (PRIMOP-PROC ,SET-PROC (VEC IDX VAL))
                                  ,PRED
                                  ,LEN))))
   
(DEFINE-VECTOR-PRIMOP BYTEV BYTEV-ELT-POINTER)
(DEFINE-VECTOR-PRIMOP BYTEV BYTEV-ELT-8      )
(DEFINE-VECTOR-PRIMOP BYTEV BYTEV-ELT-16     )
(DEFINE-VECTOR-PRIMOP BYTEV BYTEV-ELT-32     )

;;;; Primop arithmetic functions

;;; ... also, bytev primops.

;;; Binary comparisons:
;;; We could in theory assert BOOLEAN? on the result of the call to PROC, for
;;; the benefit of the (currently non-existent) assertion system.

(DEFINE-LOCAL-SYNTAX (DEFINE-BINARY-COMPARISON PROC TYPE)
  (COND ((EQ? PROC 'TRUE)
         `(DEFINE ,PROC (PRIMOP-PROC ,PROC (X Y))))
        (ELSE
         `(DEFINE-PRIMOP ,PROC (,TYPE ,TYPE)))))

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



;;; XOP's
;;; We should provide for asserting result type.

(DEFINE-LOCAL-SYNTAX (DEFINE-STANDARD-XOP PROC TYPE)
  (COND ((EQ? TYPE 'TRUE)
         `(DEFINE ,PROC (PRIMOP-PROC ,PROC (X))))
        (ELSE
         `(DEFINE-PRIMOP ,PROC (,TYPE)))))

(DEFINE-STANDARD-XOP FIXNUM-NEGATE  FIXNUM?)
(DEFINE-STANDARD-XOP FIXNUM-LOGNOT  FIXNUM?)
;(DEFINE-STANDARD-XOP POINTER-LOGNOT TRUE)      ; Useful?

;;; TOP's

(DEFINE-LOCAL-SYNTAX (DEFINE-STANDARD-TOP PROC TYPE)
  (COND ((EQ? TYPE 'TRUE)
         `(DEFINE ,PROC (PRIMOP-PROC ,PROC (X Y))))
        (ELSE
         `(DEFINE-PRIMOP ,PROC (,TYPE ,TYPE)))))

(DEFINE-STANDARD-TOP FIXNUM-ADD      FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-SUBTRACT FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-LOGIOR   FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-LOGAND   FIXNUM?)
(DEFINE-STANDARD-TOP FIXNUM-LOGXOR   FIXNUM?)

(DEFINE-STANDARD-TOP POINTER-ADD      TRUE)
(DEFINE-STANDARD-TOP POINTER-SUBTRACT TRUE)
(DEFINE-STANDARD-TOP POINTER-LOGIOR   TRUE)
(DEFINE-STANDARD-TOP POINTER-LOGAND   TRUE)     ; ?


;;; Non-primops.

;;; (FIXNUM-HOWLONG n)
;;;   Returns the number of bits in N's binary representation.
;;;   Horrible name, after MACLISP function HAULONG.

(DEFINE-LAP-PROCEDURE FIXNUM-HOWLONG ((EXPR 1 0 0))
  (MOVE.L (REG+ SP) D0)
  (LSR.L (LIT 3) D0)
  (CLR.L D2)                            "Result goes here"
  (MOVE.L D0 D1)
  (ANDI.L (LIT #xFFFF8000) D1)
  (BEQ.S HOWLONG1)
  (ADDI.W (LIT 16.) D2)
  (SWAP D0)
HOWLONG1
  (MOVE.W D0 D1)
  (ANDI.W (LIT #x7F80) D1)
  (BEQ.S HOWLONG2)
  (ADDQ.W (LIT 8) D2)
  (ASR.L (LIT 8) D0)
HOWLONG2
  (MOVE.W D0 D1)
  (ANDI.B (LIT #x78) D1)
  (BEQ.S HOWLONG3)
  (ADDQ.W (LIT 4) D2)
  (ASR.L (LIT 4) D0)
HOWLONG3
  (MOVE.W D0 D1)
  (ANDI.B (LIT #x6) D1)
  (BEQ.S HOWLONG4)
  (ADDQ.W (LIT 2) D2)
  (ASR.L (LIT 2) D0)
HOWLONG4
  (MOVE.W D0 D1)
  (ANDI.B (LIT #x1) D1)
  (BEQ.S HOWLONG5)
  (ADDQ.W (LIT 1) D2)
HOWLONG5
  (ASL.W (LIT 3) D2)
  (MOVE.L D2 VAL)
  (JMP (SLINK IRETURN)))

;;; Definitions for bignum package.

(block                   ; Ensure adjacency
(DEFINE-LAP-PROCEDURE FIXNUM-ADD-CARRY? ((EXPR 2 0 0))
  (MOVE.L (REG SP 4) D1)
  (ADD.L (REG SP) D1)
  (BCC.S COMMON-RETURN-FALSE)
  (BRA.S COMMON-RETURN-TRUE))

(DEFINE-LAP-PROCEDURE FIXNUM-SUBTRACT-CARRY? ((EXPR 2 0 0))
  (MOVE.L (REG SP 4) D1)
  (SUB.L (REG SP) D1)
  (BCC.S COMMON-RETURN-FALSE)
  (BRA.S COMMON-RETURN-TRUE))

(DEFINE-LAP-PROCEDURE FIXNUM-ADD-OVERFLOW? ((EXPR 2 0 0))
  (MOVE.L (REG SP 4) D1)
  (ADD.L (REG SP) D1)
  (BVC.S COMMON-RETURN-FALSE)
  (BRA.S COMMON-RETURN-TRUE))

(DEFINE-LAP-PROCEDURE FIXNUM-SUBTRACT-OVERFLOW? ((EXPR 2 0 0))
  (MOVE.L (REG SP 4) D1)
  (SUB.L (REG SP) D1)
  (BVC.S COMMON-RETURN-FALSE)
COMMON-RETURN-TRUE
  (MOVE.L (SLINK TRUE) VAL)
  (LEA (REG SP 8) SP)
  (JMP (SLINK IRETURN))
COMMON-RETURN-FALSE
  (MOVE.L (SLINK FALSE) VAL)
  (LEA (REG SP 8) SP)
  (JMP (SLINK IRETURN))))
  
;;; (FLONUM-GUTS flonum cont)
;;;   "cont" gets called with four fixnum arguments.

(DEFINE-LAP-PROCEDURE FLONUM-GUTS ((EXPR 2 0 0))
  (MOVE.L (REG+ SP) FUN)                "Get continuation"
  (MOVE.L (REG+ SP) VAL)                "Get the flonum"
  (CLR.L D0)                            "Get first word as a fixnum"
  (MOVE.W (REG VAL (- 0 %%FLONUM-TAG)) D0)
  (ASL.L (LIT 3) D0)
  (MOVE.L D0 (-REG SP))                 "First arg to continuation"
  (CLR.L D0)
  (MOVE.W (REG VAL (- 2 %%FLONUM-TAG)) D0)
  (ASL.L (LIT 3) D0)
  (MOVE.L D0 (-REG SP))                 "Second arg, etc."
  (CLR.L D0)
  (MOVE.W (REG VAL (- 4 %%FLONUM-TAG)) D0)
  (ASL.L (LIT 3) D0)
  (MOVE.L D0 (-REG SP))
  (CLR.L D0)
  (MOVE.W (REG VAL (- 6 %%FLONUM-TAG)) D0)
  (ASL.L (LIT 3) D0)
  (MOVE.L D0 (-REG SP))
  (JMP (SLINK ICALL)))

;;; (FIXNUMS-REPLACE-FLONUM flonum fixnum fixnum fixnum fixnum)

(define-lap-procedure fixnums-replace-flonum ((expr 5 0 0))
  (move.l (reg sp 16.) val)             "flonum"
  (move.l (reg sp) d0)
  (asr.l (lit 3) d0)
  (move.w d0 (reg val (- 6 %%flonum-tag)))
  (move.l (reg sp 4) d0)
  (asr.l (lit 3) d0)
  (move.w d0 (reg val (- 4 %%flonum-tag)))
  (move.l (reg sp 8) d0)
  (asr.l (lit 3) d0)
  (move.w d0 (reg val (- 2 %%flonum-tag)))
  (move.l (reg sp 12.) d0)
  (asr.l (lit 3) d0)
  (move.w d0 (reg val (- 0 %%flonum-tag)))
  (lea (reg sp 20.) sp)
  (jmp (slink ireturn)))

;;; (DECODE-FLOAT N CONT)
;;;   Call CONT, passing it two numbers SIG and EXP such that
;;;   N = SIG * 2^EXP  and  -1 < SIG < 1.

;;; value = (-1)^s * 2^(exponent-1023) * 1.significand
;;;       = (-1)^s * 2^(exponent-1022) * .1significand

(define-lap-procedure decode-float ((expr 2 0 0))
  ;; N = 4(SP), CONT = 0(SP)
  (move.l (reg sp) fun)             ; Get continuation
  (move.l (reg sp 4) xp)            ; Get the flonum
  (move.l hp val)                   ; Cons a new flonum
  (lea (reg hp 8) hp)
  (adjust-tag val val %%flonum-tag)
  (move.w (reg xp (~ %%flonum-tag)) d0)     ; Get word 0
  (move.w d0 d1)                    ; Save for later
  (and.w (lit #x7FF0) d0)           ; Extract exponent field
  (sub.w (lit #x3FE0) d0)           ; Excess 1022
  (ext.l d0)                        ; Turn it into a longword
  (asr.l (lit 1) d0)                ; Fixnumize it (note that 4 - 3 = 1)
  ;; SIG = 4(SP), EXP = (SP)
  (move.l d0 (reg sp))              ; Exponent is 2nd arg to cont
  (move.l (reg xp (- 0 %%flonum-tag)) (reg val (- 0 %%flonum-tag))) ; Copy
  (move.l (reg xp (- 4 %%flonum-tag)) (reg val (- 4 %%flonum-tag))) ; Copy
  (and.w (lit #x800F) d1)           ; Clear exponent field
  (add.w (lit #x3FE0) d1)           ; Set it to 0, excess 1022
  (move.w d1 (reg val (~ %%flonum-tag)))
  (move.l val (reg sp 4))           ; First arg is significand
  (lea (reg sp 8) ap)               ; Two args
  (jmp (slink icall)))              ; Invoke continuation

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
                                       (fx+ (fixnum-bit-field a 0 4) 16)
                                       16))
                                  16))
                         16))
                (fx- (fixnum-bit-field a 4 11) (fx+ 1024 51))))))

;(define (decode-float x values)
;  (values (let ((n (hacked-copy-flonum x)))
;            ...)))

;(format nil "~x" (integer-decode-float 1.0 list))

;(integer-decode-float 1.0 (lambda (s e) (* s (expt 2 e))))

;(define (foo x)
;  (flonum-guts x (lambda (a b c d)
;                   (format nil "~-4x.~-4x.~-4x.~-4x" a b c d))))
