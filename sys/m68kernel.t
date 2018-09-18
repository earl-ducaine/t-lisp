(HERALD M68KERNEL                               ; -*- Mode:SCHLAP; System:T -*-
        (PRE-COOK)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; System LAP code

;;; This is the "kernel" - that part of the 68000 Apollo T system which is not
;;; written in T itself, but in 68000 assembler language.

;;; This code should be system independent -- system dependent code
;;; is in XENO; that is where the real entry is.

;;; Entry into Scheme system, initialization

(LAP (DATA)
KERNEL-DATA
     (GLOBL KERNEL-DATA)
     (TEXT)
KERNEL-ENTRY
     (GLOBL KERNEL-ENTRY)
     (MOVE.L (SLINK KERNEL-DATA) TP)
     (TP-IS KERNEL-DATA)

     (MOVE.L SP (-REG SP))              "Align SP to odd peso..."
     (ANDI.B (LIT #b11111000) (REG SP 3))       "round down to next quadword"
     (MOVE.L (REG+ SP) SP)              "put SP there"
     (MOVE.L D7 (-REG SP))              "filler to get next odd peso"

     (MOVE.L (STATIC *STACK-END*) XP)   "Remember where our stack started"
     (MOVE.L SP (REG XP))
     (PEA (DATUM STACK-BASE))                 "Push return address"
     (MOVE.L SP AP)                     "Set AP to point to return frame"
     (MOVE.L (STATIC *THE-INITIAL-UNITS*) XP)
     (MOVE.L (REG XP) XP)               "Get vector of units"
     (MOVE.L (REG XP) FUN)              "Get unit"
     (MOVE.L (REG FUN  %%UNIT-THING-OFFSET) FUN)        "Get setup proc"
     (JMP (SLINK ICALL))                "Transfer control"
     )

(DEFINE *KERNEL-PROCESSOR* 'MC68000)

;;; Someday this template should have a special gc-method.

(DEFINE *STACK-BASE-TEMPLATE*
  (LAP-TEMPLATE STACK-BASE C$STACK-BASE
                ((RETURN)
                 (HANDLER P$HANDLE-STACK-BASE-STUB)) "Template for return point"
     (PUSH (SLINK *STACK-BASE-FAULT*))               "Shouldn't ever come here"
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     ))

(LAP (PROCEDURE P$HANDLE-STACK-BASE-STUB
                T$HANDLE-STACK-BASE-STUB C$HANDLE-STACK-BASE-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-STACK-BASE) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

;;; Control should rarely pass here, because the code which checks for wrong
;;; number of args will already catch inapplicable things.

(LAP (DEFINE-SLINK INAPPLICABLE)
     (PUSH (SLINK *INAPPLICABLE-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (JMP (SLINK ICALL)))

;;; We come here when we execute code which the compiler has marked as bogus.

(LAP (DEFINE-SLINK COMPILER-LOSSAGE)
     (PUSH (SLINK *COMPILER-LOSSAGE-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT)))


;;; ICALL: Apply a function to args on stack, and be paranoid about it.

(LAP (GLOBL ICALL)
ICALL
     (GLOBL PARANOID-ICALL)
PARANOID-ICALL

     ;; The following disappears if we have faith that the code
     ;; generator is actually doing its job correctly.
     (MOVE.L AP D7)
     (BTST (LIT 2) D7)                  "Check cont alignment"
     (BNE ICALL-BAD-AP)                 "Branch away if no good (bit set)"
     (MOVE.L (REG AP) D7)               "Get cont template copied to do mask"
     (ANDI.B (LIT 7) D7)                "Check its type"
     (CMPI.B (LIT %%TEMPLATE-TAG) D7)   "Is it really a template?"
     (BNE ICALL-BAD-AP-TEMPLATE)        "Barf if not"

     (MOVE.L FUN D7)                    "Copy FUN to do mask"
     (ANDI.B (LIT 7) D7)                "Check type of function"
     (CMPI.B (LIT %%EXTEND-TAG) D7)     "Is it EXTEND?"
     (BNE ICALL-BAD-FUN)                "Barf if not"

     (MOVE.L (REG FUN %%EXTEND-TEMPLATE-OFFSET) VAL)    "Get template from fn"
     (MOVE.L VAL D7)                    "Get copy to do mask on, and..."
     (ANDI.B (LIT 7) D7)                "Check out its type"
     (CMPI.B (LIT %%TEMPLATE-TAG) D7)   "Is it really a template?"
     (BNE ICALL-BAD-FUN-TEMPLATE)       "Barf if not"

     (MOVE.L (REG VAL %%TEMPLATE-CHUNK-OFFSET) XP)      "Get chunk ptr"

     (BTST (LIT %%PROCEDURE?-BIT-POS)
             (REG XP %%CHUNK-SUBTYPE-BITS-OFFSET))
     (BEQ ICALL-NOT-PROCEDURE)          "Error if not callable"

     (MOVE.L AP D7)                     "Copy AP to subtract from"
     (SUB.L SP D7)                      "Look at actual number of args"
     (ASR.L (LIT 2) D7)                 "Machine num"
     (CMP.B (REG XP %%CHUNK-NARGS-OFFSET) D7)
     (BEQ.S ICALL-OK)                   "All ok if match"
     (BLO   ICALL-WNA)                  "Error if too few supplied"
     (BTST (LIT %%LEXPR?-BIT-POS)
             (REG XP %%CHUNK-SUBTYPE-BITS-OFFSET))
     (BEQ ICALL-WNA)                    "If too many, err if not lexpr"
ICALL-OK
     (MOVE.L TP (SLINK JUMP-FROM))      "For debugging, set JUMP-FROM reg"
CONFIDENT-ICALL
FAST-ICALL
     (GLOBL CONFIDENT-ICALL)
     (MOVE.L (REG FUN %%EXTEND-TEMPLATE-OFFSET) TP)     "Extract template"
     (JMP (REG TP))                     "Transfer to function"

ICALL-BAD-AP
     (PUSH (SLINK *ICALL-AP-ALIGNMENT-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (JMP (SLINK ICALL))
ICALL-BAD-AP-TEMPLATE
     (PUSH (SLINK *ICALL-RETURN-NOT-TEMPLATE-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (JMP (SLINK ICALL))
ICALL-BAD-FUN
     (PUSH (SLINK *ICALL-NOT-EXTEND-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (JMP (SLINK ICALL))
ICALL-BAD-FUN-TEMPLATE
     (PUSH (SLINK *ICALL-TEMPLATE-NOT-TEMPLATE-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (JMP (SLINK ICALL))
ICALL-WNA
     (PUSH (SLINK *ICALL-WRONG-NUMBER-ARGS-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (JMP (SLINK ICALL))
ICALL-NOT-PROCEDURE
     (PUSH (SLINK *ICALL-NOT-PROCEDURE-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (JMP (SLINK ICALL))

ZIPPY-ICALL
     (GLOBL ZIPPY-ICALL)
     (MOVE.L FUN D7)                    "Copy FUN to do mask"
     (ANDI.B (LIT 7) D7)                "Check type of function"
     (CMPI.B (LIT %%EXTEND-TAG) D7)     "Is it EXTEND?"
     (BNE ICALL-BAD-FUN)                "Barf if not"

     (MOVE.L (REG FUN %%EXTEND-TEMPLATE-OFFSET) VAL)    "Get template"
     (MOVE.L (REG VAL %%TEMPLATE-CHUNK-OFFSET) XP)      "Get chunk ptr"
     (MOVE.L AP D7)                     "Copy AP to subtract from"
     (SUB.L SP D7)                      "Look at actual number of args"
     (ASR.L (LIT 2) D7)                 "Machine num"
     (CMP.B (REG XP %%CHUNK-NARGS-OFFSET) D7)
     (BEQ.S ZIPPY-ICALL-OK)                   "All ok if match"
     (BLO   ICALL-WNA)                  "Error if too few supplied"
     (BTST (LIT %%LEXPR?-BIT-POS) (REG XP %%CHUNK-SUBTYPE-BITS-OFFSET))
     (BEQ ICALL-WNA)                    "If too many, err if not lexpr"
ZIPPY-ICALL-OK
     (MOVE.L TP (SLINK JUMP-FROM))      "For debugging, set JUMP-FROM reg"
     (MOVE.L (REG FUN %%EXTEND-TEMPLATE-OFFSET) TP)     "Extract template"
     (JMP (REG TP))                     "Transfer to procedure"
)

;;; IRETURN: return from a procedure, and be paranoid about it.

(LAP (GLOBL IRETURN)
IRETURN
     (GLOBL PARANOID-IRETURN)
PARANOID-IRETURN
     (MOVE.L SP D7)                     "Copy Sp to do bit test on"
     (BTST (LIT 2) D7)                  "Check stack alignment"
     (BNE.S IRETURN-BAD-SP)             "Branch away if no good"
     (MOVE.L TP (SLINK JUMP-FROM))      "For debugging, set JUMP-FROM reg"
     (MOVE.L (REG SP) TP)              "Fetch return address"
     (MOVE.L TP D7)                     "Get copy to do mask with"
     (ANDI.B (LIT 7) D7)                "Get type code"
     (CMPI.B (LIT %%TEMPLATE-TAG) D7)   "Is type template?"
     (BNE IRETURN-BAD-SP-TEMPLATE)      "Barf if not"

CONFIDENT-IRETURN
     (GLOBL CONFIDENT-IRETURN)

     ;; GC invocation hack grossmeout
     (CMP.L (SLINK HEAP-LIMIT) HP)
     (BLO.S CONTINUE-IRETURN)
     (MOVE.L (SLINK KERNEL-DATA) TP)
     (TP-IS KERNEL-DATA)
     (MOVE.L (STATIC *HEAP-OVERFLOW-PENDING?*) XP)
     (MOVE.L (REG XP) D0)
     (CMP.L (SLINK FALSE) D0)
     (BNE.S CONTINUE-IRETURN)
     (MOVE.L (SLINK TRUE) (REG XP))
     (MOVE.L SP AP)
     (MOVE.L VAL (-REG SP))
     (MOVE.L (STATIC HANDLE-HEAP-OVERFLOW) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL))

CONTINUE-IRETURN
     (MOVE.L (REG+ SP) TP)              "Fetch return address"
     (JMP (REG TP))                     "Popj"

IRETURN-BAD-SP
     (PUSH (SLINK *IRETURN-BAD-SP-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
IRETURN-BAD-SP-TEMPLATE
     (PUSH (SLINK *IRETURN-BAD-SP-TEMPLATE-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT)))


;;; APPLY, in two flavors, and the LEXPR-arg conser

;;; IAPPLY: the compiler supposedly will some day generate references to
;;; this routine when compiling calls to APPLY.  Going to the APPLY
;;; procedure itself (see below) is bletcherous since the first argument
;;; (the function to be called) needs to get squeezed out of the way.
;;; The function to call is in FUN, list of args to spread is in VAL.
;;; Internally (APPLY FOO A B C) looks exactly like (FOO A B C) except
;;; that we jump not to ICALL but to IAPPLY.

(LAP (DEFINE-SLINK IAPPLY)
     (MOVE.L (REG+ SP) VAL)             "Fetch the list"
     (MOVE.L (SLINK NULL) D6)
     (BRA.S APTST)                      "Enter loop in middle"
APLOOP
     (MOVE.W VAL D7)                    "Get copy to test type"
     (ANDI.B (LIT 7) D7)                "Make sure it is a good list"
     (CMPI.B (LIT %%PAIR-TAG) D7)
     (BNE.S  APFAULT)                   "Barf if not"
     (MOVE.L (REG VAL %%CAR-OFFSET) (-REG SP))  "Push list's CAR"
     (MOVE.L (REG VAL %%CDR-OFFSET) VAL)        "Next list element"
APTST
     (CMP.L D6 VAL)                     "Done when we hit ()"
     (BNE.S APLOOP)                     "Keep spreading if any left"
     (JMP   (SLINK ICALL))              "Call the function"
APFAULT
     (PUSH (SLINK *IAPPLY-IMPROPER-LIST-FAULT*))
     (JSR (SLINK T-SYNCHRONOUS-FAULT))
     (BRA APLOOP))

(DEFINE-LAP-PROCEDURE APPLY ((LEXPR 2 0 0))
    (LEA (REG AP) XP)           "Calculate pointers for BLT"
    (LEA (REG AP -4) VAL)
    (MOVE.L (REG AP -4) FUN)            "Save the function"
    (BRA.S APPLY-LOOP-TEST)

    ;; (ENTER-CRITICAL GC)
APPLY-LOOP
       (MOVE.L (-REG VAL) (-REG XP))    "Move next arg down 1 slot on stack"
APPLY-LOOP-TEST
       (CMP.L VAL SP)                   "Moved last arg?"
       (BLT.S APPLY-LOOP)               "If not, keep BLTing"
    ;; (LEAVE-CRITICAL GC)

    (CLR.L D7)                          "Clear regs used to avoid GC screws"
    (MOVE.L D7 XP)
    (MOVE.L D7 VAL)
    (LEA (REG SP 4) SP)                 "Pop duplication"
    (JMP (SLINK IAPPLY)))

;;; LEXPR &REST-arg conser: pop args off stack into a list!
;;; Number of required args is passed in D7; return address
;;; is on the stack (entered via JSR).  (Is this safe?!)

(LAP (DEFINE-SLINK LEXPR-SETUP)
     ;; ??? can this return address be in a ptr reg ???
     (MOVE.L (REG+ SP) D5)              "Pop return address"
     (MOVE.L AP D6)
     (SUB.L  SP D6)                     "Number of args in D6"
     (ASR.L (LIT 2) D6)                 "# longwords to machine number"
     (SUB.L D7 D6)                      "Subtract out requireds"
     (MOVE.L (SLINK NULL) VAL)          "Initialize accumulator"
     (BRA.S LX-TEST)
LX-LOOP
     (MOVE.L VAL XP)                    "Stow list so far"
     (MOVE.L HP VAL)
     (LEA (REG HP 8) HP)                "Cons next cell"
     (ADJUST-TAG VAL VAL %%PAIR-TAG)
     (MOVE.L XP        (REG VAL %%CDR-OFFSET))  "Store CDR = prev"
     (MOVE.L (REG+ SP) (REG VAL %%CAR-OFFSET))  "Store CAR = next arg"
LX-TEST
     (DBRA D6 LX-LOOP)                  "Loop"
     (MOVE.L D5 XP)
     (JMP (REG XP))                     "Return"
     )

;;; The CATCH/THROW implementation consists of the following two little
;;; routines, which bracket a call to system code written in Scheme.
;;; Invoking an escape function runs the code at
;;; ESCAPE-PROCEDURE-TEMPLATE, which goes to INTERNAL-THROW, which eventually
;;; ends up calling PRIMITIVE-THROW to actually transfer control.

;;; CATCH creates closures over this template for its escape functions.

(DEFINE *ESCAPE-PROCEDURE-TEMPLATE*
  (LAP-TEMPLATE ESCAPE-PROCEDURE-TEMPLATE C$ESCAPE-PROCEDURE-TEMPLATE
                ((EXPR 1 0 0)
                 (SIZE %%ESCAPE-PROCEDURE-SIZE 0)
                 (HANDLER P$HANDLE-ESCAPE-PROCEDURE-STUB))
    (GLOBL ESCAPE-PROCEDURE-TEMPLATE)
    (MOVE.L (REG SP) (-REG SP))         "Arg becomes 2nd arg"
    (MOVE.L FUN (REG SP 4))             "First arg is SELF"
    (MOVE.L (STATIC INTERNAL-THROW) XP)
    (MOVE.L (REG XP) FUN)               "Punt to Scheme code"
    (JMP (SLINK ICALL))))               "AP is already set up"

(LAP (PROCEDURE P$HANDLE-ESCAPE-PROCEDURE-STUB
                T$HANDLE-ESCAPE-PROCEDURE-STUB C$HANDLE-ESCAPE-PROCEDURE-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-ESCAPE-PROCEDURE) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

;;; (PRIMITIVE-THROW cont value)
;;; Note that "cont" is an actual EXTEND pointer, NOT a pointer to a return
;;; address as one would find in AP on a function call.  The extend's
;;; template is the return address, thus one must subtract (or do a pop)
;;; in order to point to the return address.
;;; *** This is potentially a target for open-coding.

(DEFINE-LAP-PROCEDURE PRIMITIVE-THROW ((EXPR 2 0 0))
    (MOVE.L (REG+ SP) VAL)              "2nd arg is value to yield"
    (MOVE.L (REG SP) XP)
    (SUBQ.L (LIT 4) XP)
    (MOVE.L XP SP)                      "1st arg is continuation"
    (JMP (SLINK IRETURN)))              "Return"

;;; Scheme-callable functions

;;; Arithmetic - multiply, remainder (EARLY needs these)

(LAP (DEF P1 D3)
     (DEF P2 D4)
     (DEF ANS D5)
     (DEF TMP D6)
     (DEF SIGN D7))

(DEFINE-LAP-PROCEDURE FIXNUM-MULTIPLY ((EXPR 2 0 0))
     (MOVE.L (REG SP) P2)
     (ASR.L (LIT 3) P2)                 "Convert 1 fixnum to machine num"
     (MOVE.L (REG SP 4) P1)

     (SWAP P2)
     (MOVE.W P2 ANS)
     (MULU P1 ANS)
     (SWAP ANS)
     (CLR.W ANS)
     (MOVE.L ANS VAL)

     (SWAP P1)
     (MOVE.W P1 ANS)
     (SWAP P2)
     (MULU P2 ANS)
     (SWAP ANS)
     (CLR.W ANS)
     (ADD.L ANS VAL)

     (SWAP P1)
     (MULU P1 P2)
     (ADD.L P2 VAL)
     (LEA (REG SP 8) SP)
     (JMP (SLINK IRETURN)))

;;; FIXNUM-DIVIDE calls M$DIS$LLL and is therefore in AEXENO.T.

(DEFINE-LAP-PROCEDURE FIXNUM-REMAINDER ((EXPR 2 0 0))
     (MOVE.L (REG SP 4) P1)
     (BPL.S FR-FIRST-POSITIVE)
     (NEG.L P1)
FR-FIRST-POSITIVE
     (MOVE.L (REG SP) P2)
     (BPL.S FIXNUM-REMAINDER-START)
     (NEG.L P2)
FIXNUM-REMAINDER-START
     (MOVE.L (LIT 31) TMP)
     (CLR.L ANS)
FIXNUM-REMAINDER-LOOP
     (LSL.L (LIT 1) P1)
     (ROXL.L (LIT 1) ANS)
     (CMP.L P2 ANS)
     (BCS.S FIXNUM-REMAINDER-NEXT)
     (SUB.L P2 ANS)
FIXNUM-REMAINDER-NEXT
     (DBRA TMP FIXNUM-REMAINDER-LOOP)
     (TST.W (REG SP 4))
     (BPL.S FIXNUM-REMAINDER-END)
     (NEG.L ANS)                        "should already be a lisp fixnum"
FIXNUM-REMAINDER-END
     (MOVE.L ANS VAL)
     (LEA (REG SP 8) SP)
     (JMP (SLINK IRETURN)))


;;; (%MAKE-STRING size) - figure out later how to lock out interrupts.
;;; No error checking!
;;; We need enough space to account for:
;;; - 8 bytes for the header,
;;; - the 2-byte length count in the string,
;;; - the characters in the string,
;;; - a null byte at the end (for convenience)
;;; Thus the magic number 2 + 1 + 7 (7 to round up) = length to alloc for text
;;; <length to alloc for text> - 2 = value for TEXT-LENGTH
;;; <length to alloc for text> + 8 = total to alloc

(DEFINE-LAP-PROCEDURE %MAKE-STRING ((EXPR 1 0 0))
    (MOVE.L (REG SP) D7)                "Get argument"
    (ASR.L (LIT 3) D7)                  "STRING-LENGTH"
    (MOVEQ (LIT 10.) D6)                "Prep to calc text length"
    (ADD.L D7 D6)
    (ANDI.B (LIT #B11111000) D6)        "Round up to 8"
    (MOVE.L D6 D5)
    (SUBQ.L (LIT 2) D6)                 "TEXT-LENGTH"
    (ADDQ.L (LIT 8) D5)                 "Total to alloc"
    ;; (ENTER-CRITICAL HP)
    (ADJUST-TAG HP VAL %%STRING-TAG)    "Get pointer"
    (ADD.L D5 HP)                       "Bump top of heap"
    (MOVE.W D6 (REG VAL (- 8 %%STRING-TAG)))    "Store text length"
    (MOVE.L VAL XP)                     "Copy for pointer calculation"
    ;; Next instruction depends on %%STRING-TAG >= 2.
    (ADDQ.L (LIT (- 10. %%STRING-TAG)) XP)      "Calc ptr to text"
    (MOVE.L XP (REG VAL %%STRING-POINTER-OFFSET))       "Store pointer"
    (MOVE.W D7 (REG VAL %%STRING-LENGTH-OFFSET))        "Store length"
    ;; (LEAVE-CRITICAL HP)
    ;; Note that XP looks like fixnum, so no need to clear
    (MOVE.L AP SP)                      "All done"
    (JMP (SLINK IRETURN)))


;;; (%STRING-EQUAL? string1 string2)
;;; Rewrite to use CMP.L in inner loop?

(DEFINE-LAP-PROCEDURE %STRING-EQUAL? ((EXPR 2 0 0))
    (MOVE.L (SLINK FALSE) VAL)                  "Prepare for failure"
    (MOVE.L (REG AP -4) XP)                     "Fetch args"
    (MOVE.L (REG AP -8) YP)
    (MOVE.W (REG XP %%STRING-LENGTH-OFFSET) D6)
    (CMP.W (REG YP %%STRING-LENGTH-OFFSET) D6)
    (BNE.S RETF)                        "Lose if not same length"

    ;; (ENTER-CRITICAL GC)

    (MOVE.W (REG XP %%STRING-BASE-OFFSET) D7)   "Get offset to substring"
    (MOVE.L (REG XP %%STRING-POINTER-OFFSET) XP)
    "Get pointer to first arg's text"
    (ADDA.W D7 XP)                      "Get ptr to substring in string"

    (MOVE.W (REG YP %%STRING-BASE-OFFSET) D7)   "Get offset to substring"
    (MOVE.L (REG YP %%STRING-POINTER-OFFSET) YP)
    "Get pointer to first arg's text"
    (ADDA.W D7 YP)                      "Get ptr to substring in string"
    (MOVE.W (LIT 4) CCR)
    (BRA.S START-STRING-EQUAL-LOOP)     "Need EQ cond code for loop entry"
STRING-EQUAL-LOOP
    (CMPM.B (REG+ XP) (REG+ YP))
START-STRING-EQUAL-LOOP
    (DBNE D6 STRING-EQUAL-LOOP)
    (CLR.L D7)                          "Clear used regs to avoid GC screws"
    (MOVE.L D7 XP)
    (MOVE.L D7 YP)
    ;; (LEAVE-CRITICAL GC)
    (TST.W D6)
    (BGE.S RETF)
    (MOVE.L (SLINK TRUE) VAL)
RETF
    (MOVE.L AP SP)                              "All done"
    (JMP (SLINK IRETURN)))

;;; (STRING-REPLACE target source count)
;;; Clobbers the target string with the text of the source string.
;;; Returns target.

(DEFINE-LAP-PROCEDURE STRING-REPLACE ((EXPR 3 0 0))
     (MOVE.L (REG SP 4) XP)             "Fetch source"
     (MOVE.L (REG SP 8) VAL)            "Fetch target"
     (MOVE.L (REG SP) D0)
     (ASR.L (LIT 3) D0)
 ;;;(ENTER-CRITICAL GC)
     (CLR.L D1)
     (CLR.L D2)
     (MOVE.W (REG XP %%STRING-BASE-OFFSET) D1)
     (MOVE.W (REG VAL %%STRING-BASE-OFFSET) D2)
     (MOVE.L (REG XP %%STRING-POINTER-OFFSET) XP)       "Point to source text"
     (ADDA.L D1 XP)
     (MOVE.L (REG VAL %%STRING-POINTER-OFFSET) VAL)     "Point to target text"
     (ADDA.L D2 VAL)
     (BRA.S STRING-REPLACE-LOOP-START)
STRING-REPLACE-LOOP                     "Copy text"
         (MOVE.B (REG+ XP) (REG+ VAL))
STRING-REPLACE-LOOP-START
         (DBRA D0 STRING-REPLACE-LOOP)
     (CLR.L D0)                         "Clear out random pointers"
     (MOVE.L D0 XP)
     (MOVE.L D0 VAL)
;;;(LEAVE-CRITCAL GC)
     (MOVE.L (REG SP 8) VAL)            "Fetch target again"
     (LEA (REG SP 12.) SP)              "All done"
     (JMP (SLINK IRETURN)))

;;; (%STRING-POSQ char string)
;;; Find index of character within string.

(DEFINE-LAP-PROCEDURE %STRING-POSQ ((EXPR 2 0 0))
  (MOVE.L (REG SP 4) D0)                "Get character"
  (ASR.L (LIT 3) D0)                    "Make it ASCII"
  (MOVE.L (REG SP) XP)                  "Get string"
  (MOVE.L (SLINK FALSE) VAL)             "Prepare for failure"
  (MOVE.W (REG XP %%STRING-BASE-OFFSET) D7)
  (MOVE.W (REG XP %%STRING-LENGTH-OFFSET) D1)   "Get string length"
  (CLR.L D2)                            "68K loses big"
  (MOVE.W D1 D2)                        "Copy length for compute after loop"
;;;(ENTER-CRITCAL GC)
  (MOVE.L (REG XP %%STRING-POINTER-OFFSET) XP)  "Point to text"
  (LEA (IDX.W D7 XP 0) XP)              "substring in string"
  (MOVE.W (LIT 0) CCR)
  (BRA.S STRING-POSQ-LOOP-START)        "Need NEQ cond code for loop entry"
STRING-POSQ-LOOP
     (CMP.B (REG+ XP) D0)
STRING-POSQ-LOOP-START
     (DBEQ D1 STRING-POSQ-LOOP)
  (MOVE.L (SLINK NULL) XP)              "legitimize XP"
;;;(LEAVE-CRITCAL GC)
  (TST.W D1)
  (BLT.S SPOSQ-LOSE)
  (SUB.W D1 D2)
  (SUBQ.L (LIT 1) D2)
  (ASL.L (LIT 3) D2)
  (MOVE.L D2 VAL)
SPOSQ-LOSE
  (LEA (REG SP 8) SP)           "All done"
  (JMP (SLINK IRETURN)))

;;; (STRING-HASH string) ret number in [0,2^31)

(DEFINE-LAP-PROCEDURE STRING-HASH ((EXPR 1 0 0))
  (MOVE.L (REG SP) VAL)                 "get string"
  (CLR.L D5)                            "hash value so far"
  (MOVE.W (REG VAL %%STRING-LENGTH-OFFSET) D6)
  (TST.W D6)                            "done before we start?"
  (BEQ.S STRING-HASH-DONE)
  (MOVE.W (REG VAL %%STRING-BASE-OFFSET) D7)
  (MOVE.L (REG VAL %%STRING-POINTER-OFFSET) VAL)        "pnt to head of txt"
  (ADDA.W D7 VAL)                       "pnt to beg of substr"
  (SUBQ.W (LIT 1) D6)                   "index from 0"
STRING-HASH-LOOP
  (ROL.L (LIT 1) D5)
  (ADD.B (IDX.W D6 VAL 0) D5)
  (DBRA D6 STRING-HASH-LOOP)
STRING-HASH-DONE
  (MOVE.L D5 D4)
  (SWAP D4)
  (EOR.L D4 D5)                         "Keep significant high and low bits"
  (LSL.L (LIT 4) D5)                    "Positive fixnumize"
  (LSR.L (LIT 1) D5)
  (MOVE.L D5 VAL)
  (MOVE.L AP SP)                        "Return"
  (JMP (SLINK IRETURN)))


;;; (ZERO-MEM start count) start must be longword align, count is a fixnum
;;; number of quadwords

(DEFINE-LAP-PROCEDURE ZERO-MEM ((EXPR 2 0 0))
  (MOVE.L (REG SP) D0)                  "Count"

  (MOVE.L (REG SP 4) XP)
  (MOVE.W XP D1)
  (LSR.W (LIT 1) D1)
  (BCS.S ZERO-MEM-MISALIGNED)
  (LSR.W (LIT 1) D1)
  (BCS.S ZERO-MEM-MISALIGNED)

  (CLR.L D1)
  (BRA.S ZERO-MEM-LOOP-START)

ZERO-MEM-LOOP
  (MOVE.L D1 (REG+ XP))
ZERO-MEM-LOOP-START
  (SUBQ.L (LIT 4) D0)
  (BGE.S ZERO-MEM-LOOP)

ZERO-MEM-DONE
  (MOVE.L D1 VAL)
  (LEA (REG SP 8) SP)
  (JMP (SLINK IRETURN))

ZERO-MEM-MISALIGNED
  (BPT))

;;; (COPY-MEM target source count) - same arg order as REPLACE.  COUNT is
;;; fixnum number of longwords. source and target can't overlap, and must
;;; be longword aligned

(DEFINE-LAP-PROCEDURE COPY-MEM ((EXPR 3 0 0))
  (MOVE.L (REG SP) D0)
  (ASR.L (LIT 1) D0)                    "Number of bytes"
  (MOVE.L (REG SP 4) XP)                "Source"
  (MOVE.L (REG SP 8) VAL)               "Target"
  (BRA.S COPY-MEM-LOOP-START)

COPY-MEM-LOOP
  (MOVE.L (REG+ XP) (REG+ VAL))
COPY-MEM-LOOP-START
  (SUBQ.L (LIT 4) D0)
  (BGE.S COPY-MEM-LOOP)

  (MOVE.L (REG SP 8) VAL)               ; Return target
  (LEA (REG SP 12) SP)
  (JMP (SLINK IRETURN))
  )


;;; (PUSH-MAGIC-FRAME interesting-value procedure-to-call)
;;; sets up a recognizable configuration on the stack, for use in dynamic state
;;; winding and unwinding.  If (EQ? (EXTEND-TEMPLATE F) *MAGIC-FRAME-TEMPLATE*),
;;; then (XREF F 0) will be the first arg passed to PUSH-MAGIC-FRAME.

;;; (PUSH-MAGIC-FRAME chain token arg procedure)
;;; Sets up a recognizable configuration on the stack, for use in dynamic state
;;; winding and unwinding.
;;; If (EQ? (EXTEND-TEMPLATE F) *MAGIC-FRAME-TEMPLATE*),
;;; then (XREF F 2) => chain and (XREF F 1) => token.
;;; The procedure gets passed two arguments: the magic frame itself, and arg.
;;; Pretty kludgey, I admit.  So what.

(DEFINE-LAP-PROCEDURE PUSH-MAGIC-FRAME ((EXPR 4 3 0))
     (MOVE.L (REG+ SP) FUN)             "4th arg is proc to call"
     (PEA (DATUM MAGIC-FRAME-TEMPLATE))
     (MOVE.L SP AP)                     "Set up for call to proc"
     (PEA (REG SP 4))                   "Pointer to frame as extend"
     (MOVE.L (REG SP 8) (-REG SP))      "3rd arg becoms 2nd arg to proc"
     (JMP (SLINK ICALL)))

(DEFINE *MAGIC-FRAME-TEMPLATE*
  (LAP-TEMPLATE MAGIC-FRAME-TEMPLATE C$MAGIC-FRAME-TEMPLATE
                ((RETURN)
                 (SUPERIOR T$PUSH-MAGIC-FRAME)  ; GC needs
                 (HANDLER P$HANDLE-MAGIC-FRAME-STUB)
                 (SIZE -1 0))
    (LEA (REG SP 12) SP)
    (JMP (SLINK IRETURN))))

(LAP (PROCEDURE P$HANDLE-MAGIC-FRAME-STUB
                T$HANDLE-MAGIC-FRAME-STUB C$HANDLE-MAGIC-FRAME-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-MAGIC-FRAME) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

;;; V-frames...
;;; The only interesting thing about a VFRAME is that
;;;   (FRAME-PREVIOUS vframe)  <=>  (XREF vframe 0).

(LAP (PROCEDURE P$HANDLE-VFRAME-STUB
                T$HANDLE-VFRAME-STUB C$HANDLE-VFRAME-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-VFRAME) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(LAP (PROCEDURE P$GC-COPY-VFRAME-STUB
                T$GC-COPY-VFRAME-STUB C$GC-COPY-VFRAME-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC GC-COPY-VFRAME) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *VFRAME-TEMPLATE*
  (LAP-TEMPLATE VFRAME-TEMPLATE C$VFRAME-TEMPLATE
                ((GC-METHOD P$GC-COPY-VFRAME-STUB)
                 (HANDLER P$HANDLE-VFRAME-STUB))
    (GLOBL VFRAME-TEMPLATE)
    (JMP (SLINK INAPPLICABLE))))

;;; Random templates of various assorted sorts, types, and kinds.

;;; Symbol...
(LAP (PROCEDURE P$SYMBOL-HANDLER-STUB
                T$SYMBOL-HANDLER-STUB C$SYMBOL-HANDLER-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-SYMBOL) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *SYMBOL-TEMPLATE*
  (LAP-TEMPLATE SYMBOL-TEMPLATE C$SYMBOL-TEMPLATE
                ((SIZE %%SYMBOL-SIZE 0)
                 (HANDLER P$SYMBOL-HANDLER-STUB))
    (GLOBL SYMBOL-TEMPLATE)
    (JMP (SLINK INAPPLICABLE))))


;;; Vcell...
(LAP (PROCEDURE P$HANDLE-VCELL-STUB
                T$HANDLE-VCELL-STUB C$HANDLE-VCELL-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-VCELL) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *VCELL-TEMPLATE*
  (LAP-TEMPLATE VCELL-TEMPLATE C$VCELL-TEMPLATE
                ((EXPR 0 0 0)
                 (SIZE %%VCELL-SIZE 0)
                 (HANDLER P$HANDLE-VCELL-STUB))
     (GLOBL VCELL-TEMPLATE)
    ;; Actually, should crap out if it contains a nonvalue.  But that's a lot
    ;; of trouble.
    (MOVE.L (REG FUN) VAL)
    (MOVE.L (REG VAL %%VCELL-CONTENTS-OFFSET) VAL)
    (JMP (SLINK IRETURN))))

;;; Bogus entities...
(LAP (PROCEDURE P$HANDLE-BOGUS-ENTITY-STUB
                T$HANDLE-BOGUS-ENTITY-STUB C$HANDLE-BOGUS-ENTITY-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (REG SP 4) VAL)            "Entity is first arg"
     (MOVE.L (REG VAL %%BOGUS-ENTITY-HANDLER-OFFSET) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *BOGUS-ENTITY-TEMPLATE*
  (LAP-TEMPLATE BOGUS-ENTITY-TEMPLATE C$BOGUS-ENTITY-TEMPLATE
                ((LEXPR 0 0 0)
                 (HANDLER P$HANDLE-BOGUS-ENTITY-STUB)
                 (SIZE 2 0))
     (MOVE.L (REG FUN %%BOGUS-ENTITY-PROCEDURE-OFFSET) FUN)
     (JMP (SLINK ICALL))))

;;; Structures...

(LAP (DEFSYM %%STRUCTURE-HANDLER-OFFSET 0))

(LAP (PROCEDURE P$STRUCTURE-HANDLER
                T$STRUCTURE-HANDLER C$STRUCTURE-HANDLER
                ((EXPR 2 0 0)))
     (MOVE.L (REG SP 4) VAL)            "Entity is first arg"
     (MOVE.L (REG VAL %%STRUCTURE-HANDLER-OFFSET) FUN)
     (JMP (SLINK ICALL)))

(LAP (PROCEDURE P$GC-COPY-STRUCTURE-STUB
                T$GC-COPY-STRUCTURE-STUB
                C$GC-COPY-STRUCTURE-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC GC-COPY-STRUCTURE) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *STRUCTURE-TEMPLATE*
  (LAP-TEMPLATE STRUCTURE-TEMPLATE C$STRUCTURE-TEMPLATE
                ((HANDLER P$STRUCTURE-HANDLER)
                 (GC-METHOD P$GC-COPY-STRUCTURE-STUB))
    (JMP (SLINK INAPPLICABLE))))

;;; Units...
(LAP (PROCEDURE P$HANDLE-UNIT-STUB
                T$HANDLE-UNIT-STUB C$HANDLE-UNIT-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-UNIT) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(LAP (PROCEDURE P$GC-COPY-UNIT-STUB T$GC-COPY-UNIT-STUB C$GC-COPY-UNIT-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC GC-COPY-UNIT) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *UNIT-TEMPLATE*
  (LAP-TEMPLATE UNIT-TEMPLATE C$UNIT-TEMPLATE
                ((GC-METHOD P$GC-COPY-UNIT-STUB)
                 (HANDLER P$HANDLE-UNIT-STUB))
    (GLOBL UNIT-TEMPLATE)
    (JMP (SLINK INAPPLICABLE))))

;;; Vectors...
(LAP (PROCEDURE P$HANDLE-VECTOR-STUB
                T$HANDLE-VECTOR-STUB C$HANDLE-VECTOR-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-VECTOR) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(LAP (PROCEDURE P$GC-COPY-VECTOR-STUB
                T$GC-COPY-VECTOR-STUB C$GC-COPY-VECTOR-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC GC-COPY-VECTOR) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *VECTOR-TEMPLATE*
  (LAP-TEMPLATE VECTOR-TEMPLATE C$VECTOR-TEMPLATE
                ((GC-METHOD P$GC-COPY-VECTOR-STUB)
                 (HANDLER P$HANDLE-VECTOR-STUB))
    (GLOBL VECTOR-TEMPLATE)
    (JMP (SLINK INAPPLICABLE))))

;;; Bitvs...
(LAP (PROCEDURE P$HANDLE-BITV-STUB
                T$HANDLE-BITV-STUB C$HANDLE-BITV-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-BITV) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(LAP (PROCEDURE P$GC-COPY-BITV-STUB
                T$GC-COPY-BITV-STUB C$GC-COPY-BITV-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC GC-COPY-BITV) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *BITV-TEMPLATE*
  (LAP-TEMPLATE BITV-TEMPLATE C$BITV-TEMPLATE
                ((GC-METHOD P$GC-COPY-BITV-STUB)
                 (HANDLER P$HANDLE-BITV-STUB))
    (JMP (SLINK INAPPLICABLE))))

;;; Bytevs...
(LAP (PROCEDURE P$HANDLE-BYTEV-STUB
                T$HANDLE-BYTEV-STUB C$HANDLE-BYTEV-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-BYTEV) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(LAP (PROCEDURE P$GC-COPY-BYTEV-STUB
                T$GC-COPY-BYTEV-STUB C$GC-COPY-BYTEV-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC GC-COPY-BYTEV) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *BYTEV-TEMPLATE*
  (LAP-TEMPLATE BYTEV-TEMPLATE C$BYTEV-TEMPLATE
                ((GC-METHOD P$GC-COPY-BYTEV-STUB)
                 (HANDLER P$HANDLE-BYTEV-STUB))
    (JMP (SLINK INAPPLICABLE))))

;;; Xenoids...
(LAP (PROCEDURE P$HANDLE-XENOID-STUB
                T$HANDLE-XENOID-STUB C$HANDLE-XENOID-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-XENOID) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *XENOID-TEMPLATE*
  (LAP-TEMPLATE XENOID-TEMPLATE C$XENOID-TEMPLATE
                ((SIZE 0 %%XENOID-SIZE)
                 (HANDLER P$HANDLE-XENOID-STUB))
    (GLOBL XENOID-TEMPLATE)
    (JMP (SLINK INAPPLICABLE))))

;;; Populations...
(LAP (PROCEDURE P$HANDLE-POPULATION-STUB
                T$HANDLE-POPULATION-STUB C$HANDLE-POPULATION-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-POPULATION) XP)
     (MOVE.L (REG XP) FUN)
     (JMP (SLINK ICALL)))

(DEFINE *POPULATION-TEMPLATE*
  (LAP-TEMPLATE POPULATION-TEMPLATE C$POPULATION-TEMPLATE
                ((SIZE 1 1)     ;make sure this agrees with POPULATION.T
                 (HANDLER P$HANDLE-POPULATION-STUB))
    (JMP (SLINK INAPPLICABLE))))

(DEFINE *SFH-ECB-XENOID* (%XENOID SFH-ECB))      ; Can't go in AEXENO: ASM loses.
(DEFINE *SFH-PROCEDURE-XENOID* (%XENOID SFH-PROCEDURE))

;;; ()

(LAP (RANDOM-DATA                               "Here is () itself."
      (ALIGN 3)
      (SPACE (+ 8 (- %%NULL-TAG %%PAIR-TAG)))   "Hack for carcdrability"
NULL0
      (ADDRESS NULL)
      (ADDRESS NULL))

     (DEF NULL (+ NULL0 %%PAIR-TAG))
     (DEF FALSE NULL)
     (GLOBL FALSE)
     (GLOBL NULL)
     )


;; Local Modes:
;; Lisp LAP-TEMPLATE Indent:3
;; Lisp LAP-PROCEDURE Indent:4
;; Mode:SCHLAP
;; END:
