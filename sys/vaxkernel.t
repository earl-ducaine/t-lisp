(HERALD (TSYS VAXKERNEL T 281)
        (ENV TSYS)
        (PRE-COOK))

;;; Copyright (c) 1983, 1984 Yale University

;;; This is the "kernel" - that part of the Vax T system which is not
;;; written in T itself, but in Vax assembler language.
;;; Everything in this file should work on both Unix and VMS without
;;; change.

;;;; Initialization

;;; The real entry into the system is somewhere in XENO.  It basically just
;;; sets up the heap, then jumps here, to the tag KERNEL.

(LAP KERNEL
     (GLOBL KERNEL)

     (MOVAL (ABS THE-SLINK) SLP)        "Set up SLP"
     (BBS (LIT 2) SP FIXUP-1)           "Check SP alignment"
     (CLRL (-REG SP))                   "Clear filler word"
FIXUP-1
     (SUBL3 (@ (STATIC *GC-MARGIN*)) (@ (STATIC *HEAP-END*))
            (SLINK HEAP-LIMIT))
     (MOVL (SLINK HEAP-LIMIT) (@ (STATIC *HEAP-LIMIT*)))

     (MOVL SP (@ (STATIC *STACK-END*))) "Remember stack limit"
     (PUSHAL (DATUM STACK-BASE))                "Push return address"
     (MOVL SP AP)                       "Set up arg ptr"
     (MOVL (@ (STATIC *THE-INITIAL-UNITS*)) VAL)        "Get vector"
     (MOVL (REG VAL) VAL)               "Get zeroth unit in vector"
     (MOVL (REG VAL %%UNIT-THING-OFFSET) FUN)   "Get setup fn"
     (JMP (@ (SLINK ICALL)))            "Transfer control"
     )

(DEFINE *KERNEL-PROCESSOR* 'VAX11)

;;; Someday this template should have a special gc-method.

(DEFINE *STACK-BASE-TEMPLATE*
  (LAP-TEMPLATE STACK-BASE C$STACK-BASE
                ((RETURN)
                 (HANDLER P$HANDLE-STACK-BASE-STUB)) "Template for return point"
     (GLOBL STACK-BASE)
     (PUSHAL (DATUM STACK-BASE))
     (PUSHL (STATIC *STACK-BASE-FAULT*))        "Shouldn't ever come here"
     (BSBW FAULT1)
     ))

(LAP (PROCEDURE P$HANDLE-STACK-BASE-STUB
                T$HANDLE-STACK-BASE-STUB C$HANDLE-STACK-BASE-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-STACK-BASE)) FUN)  "Trampoline"
     (JMP (@ (SLINK ICALL))))


;;;; Low-level exception handling

;;; FAULT: standard internal error exit.  Jump here with return address
;;; and fault type on the stack (e.g. do PUSHL followed by JSB or BSBx).
;;; Fault type is a vcell; if it contains a procedure, then that procedure
;;; will be called with two args, the fault frame and the fault type.

(LAP FAULT
     FAULT1		;; Losing Unix assembler!!!
     (GLOBL FAULT)
;     (EXTERNAL SAVED-REGISTERS)
     (PUSHL SP)                         "Save SP"
     (PUSHL (REG SP))                   "Extra word for SAVED-AP"
     (PUSHL (REG SP))                   "Extra word for SAVED-HP"
     (PUSHL (REG SP))                   "Extra word for JUMP-FROM"
     (BBS (LIT 2) SP FAULT-SP-OK)       "Align SP"
     (PUSHL (REG SP))
FAULT-SP-OK                             "SP is odd peso address here"
     (PUSHR (LIT #x+3FFF))              "Stow R0-R13 for future examination"
     (MOVAL (ABS THE-SLINK) SLP)        "Restore slink pointer"
     (MOVL SP XP)                       "Get fault frame extend ptr"
     (MOVL (SLINK JUMP-FROM) (REG XP (* %%JF 4)))       "Save JUMP-FROM"
     (MOVL (SLINK SAVED-HP) (REG XP (* %%SHP 4)))       "Save SAVED-HP"
     (MOVL (SLINK SAVED-AP) (REG XP (* %%SAP 4)))       "Save SAVED-AP"
;     (MOVC3 (LIT (* 18. 4.)) (REG XP) (ABS SAVED-REGISTERS))"Stow regs for adb"
     (TSTL (SLINK SAVED-HP))            "See whether we need to restore HP"
     (BEQL FAULT-HP-OK)
     (MOVL (SLINK SAVED-HP) HP)
     (CMPZV (LIT 0) (LIT 3) HP (LIT %%HEAP-TAG))  "Weird intermediate state"
     (BEQL FAULT-HP-OK)
     (DECL HP)                  "This should make it right (see CALL-XENOID)"

FAULT-HP-OK
     (MOVL (REG XP (* %%SP 4)) VAL)     "Get saved SP"
     (MOVL (REG VAL 4) VAL)             "Get fault type (vcell)"
     (INITIAL-VALUE *RE-ENTER-ON-FAULT?* FALSE)
     (CMPL (SLINK FALSE) (@ (STATIC *RE-ENTER-ON-FAULT?*)))
     (BEQL FAULT-LOSSAGE)               "Give up if necessary"
     (MOVL (SLINK FALSE) (@ (STATIC *RE-ENTER-ON-FAULT?*)))

     (MOVL (REG VAL %%VCELL-CONTENTS-OFFSET) FUN)       "Handler is in vcell"
     (CMPZV (LIT 0) (LIT 3) FUN (LIT %%EXTEND-TAG))     "Is it an extend?"
     (BNEQ FAULT-LOSSAGE)               "If not, don't try to call it"

     ;; All okay now.  Time to set up for call to fault handler.
     (CLRL (SLINK SAVED-HP))            "Be re-entrant"
     (PUSHAL (DATUM FAULT-RET))         "Ret addr: SP was odd, now even"
     (MOVL SP AP)                       "So AP is even too, as desired"
     (PUSHL XP)                         "First handler arg is magic frame"
     (PUSHL VAL)                        "Second handler arg is fault type"
     (JMP (@ (SLINK ICALL)))

FAULT-LOSSAGE
     (EXTERNAL LOSING-FAULT)            "Last ditch"
     (JMP (ABS LOSING-FAULT))           "Defined by XENO"
     )

(DEFINE *FAULT-FRAME-TEMPLATE*
  (LAP-TEMPLATE FAULT-RET C$FAULT-RET
                ((RETURN)                  ; should have a gc-method
                 (HANDLER P$HANDLE-FAULT-FRAME-STUB))
     (GLOBL FAULT-RET)
     (MOVL (REG SP (* %%JF  4)) (SLINK JUMP-FROM))      "Restore jump-from"
     ;(MOVL (REG SP (* %%SHP 4)) (SLINK SAVED-HP))       "Restore saved HP"
     (MOVL (REG SP (* %%SAP 4)) (SLINK SAVED-AP))       "Restore saved AP"
     (POPR (LIT #x+7FFF))               "Restore registers"
     (MOVL (REG+ SP) (REG SP))          "Flush fault type ptr from stack"
     (RSB)))

(LAP (PROCEDURE P$HANDLE-FAULT-FRAME-STUB
                T$HANDLE-FAULT-FRAME-STUB C$HANDLE-FAULT-FRAME-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-FAULT-FRAME)) FUN) "Trampoline"
     (JMP (@ (SLINK ICALL))))

;;; Control should rarely pass here, because the code which checks for wrong
;;; number of args will already catch inapplicable things.

(LAP (DEFINE-SLINK INAPPLICABLE)
     (PUSHL (STATIC *INAPPLICABLE-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK IRETURN))))

;;; We come here when we execute code which the compiler has marked as bogus.
;;; This is a sort of stupid idea.

(LAP (DEFINE-SLINK COMPILER-LOSSAGE)
     (PUSHL (STATIC *COMPILER-LOSSAGE-FAULT*))
     (BSBW FAULT1))

;;; This tag is so that you can say "bb:c" as an adb command.

(LAP BB BB1
     (GLOBL BB)
     (PUSHL SLP)
     (MOVAL THE-SLINK SLP)
     (MOVL (SLINK TRUE) (@ (STATIC *RE-ENTER-ON-FAULT?*)))      "Force re-entry"
     (PUSHL (STATIC *BB-FAULT*))
     (BSBW FAULT1)                       "Get breakpoint"
     (MOVL (REG+ SP) SLP)
     (BPT)
     (BRB BB1))

;;; ICALL and IRETURN

;;; ICALL: Apply a procedure to args on stack, and be paranoid about it.

;;; GC must be especially careful here if it's being run while we've
;;; been interrupted out of ICALL, because reg R1 contains a chunk
;;; pointer.  If the GC moves the chunk, it must update R1.

(LAP

ICALL-STACK-OVERFLOW
     (CMPL (@ (STATIC *STACK-OVERFLOW-PENDING?*)) (SLINK NULL))
     (BNEQ CONTINUE-ICALL-AGAIN)
     (MOVL (SLINK TRUE) (@ (STATIC *STACK-OVERFLOW-PENDING?*)))
     (PUSHL (STATIC *ICALL-STACK-OVERFLOW-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK ICALL)))            "Proceed with interrupted call"

ICALL
     (GLOBL ICALL)
PARANOID-ICALL
     (GLOBL PARANOID-ICALL)
     (CMPL SP (SLINK STACK-LIMIT))      "Check proximity to disaster"
     (BLSSU ICALL-STACK-OVERFLOW)
     ;; ^^^ this should also go away sometime.

CONTINUE-ICALL-AGAIN
     (BICB3 (LIT #b11111000) HP R0)     "Verify type of heap pointer"
     (CMPB R0 (LIT %%HEAP-TAG))
     (BNEQ ICALL-BAD-HP-TAG)            "Panic if it's wrong"

     (BBS (LIT 2) AP ICALL-BAD-AP)      "Check cont alignment"
     ;; ^^^ this disappears if we have faith that the code
     ;; generator is actually doing its job correctly.

     (MOVL (REG AP) VAL)                "Get cont template"
     (BICB3 (LIT #b11111000) VAL R0)    "Check its type"
     (CMPB R0 (LIT %%TEMPLATE-TAG))     "Is it really a template?"
     (BNEQ ICALL-BAD-AP-TEMPLATE)       "Barf if not"
     ;; ^^^ this disappears if we have faith that the code
     ;; generator is actually doing its job correctly.

     (BICB3 (LIT #b11111000) FUN R0)    "Check type of procedure"
     (CMPB R0 (LIT %%EXTEND-TAG))       "Is it EXTEND?"
     (BNEQ ICALL-BAD-FUN)               "Barf if not"
     ;; ^^^ this could go away if we were satisfied with the "probabalistic"
     ;; approach.  The chance that the random bit-patterns we'd find by tracing
     ;; the pointers as we do below would both work and give reasonable values
     ;; is quite small.

     (MOVL (REG FUN %%EXTEND-TEMPLATE-OFFSET) VAL)      "Get template from fn"

     (BICB3 (LIT #b11111000) VAL R0)    "Check out its type"
     (CMPB R0 (LIT %%TEMPLATE-TAG))     "Is it really a template?"
     (BNEQ ICALL-BAD-FUN-TEMPLATE)      "Barf if not"
     ;; ^^^ similarly.  How often are there going to be extends in the
     ;; world whose template slot doesn't contain a template?...

     (MOVL (REG VAL %%TEMPLATE-CHUNK-OFFSET) R1)        "Get chunk ptr"

     (BBC (LIT %%PROCEDURE?-BIT-POS)
          (REG R1 %%CHUNK-SUBTYPE-BITS-OFFSET)
          ICALL-NOT-PROCEDURE)          "Error if not callable"
     ;; ^^^ this JBC is really pretty redundant, since the nargs slot of
     ;; non-procedures is going to be 255, which will probably cause the
     ;; next checks to catch the bug.

     (SUBL3 SP AP R0)                   "Look at actual number of args"
     (ASHL (LIT -2) R0 R0)              "Machine num"
;    (CMPL R0 (LIT 255.))               "If nargs is >= 255 we lose, sort of"
;    (BGEQU ICALL-MANYMANYARGS)         "This is unfortunate"
     (CMPB R0 (REG R1 %%CHUNK-NARGS-OFFSET))
     (BEQL ICALL-OK)                    "All ok if match"
     (BLSSU ICALL-WNA)                  "Error if too few supplied"
     (BBC (LIT %%LEXPR?-BIT-POS)
          (REG R1 %%CHUNK-SUBTYPE-BITS-OFFSET)
          ICALL-WNA)                    "If too many, err if not lexpr"
ICALL-OK
     (MOVL TP (SLINK JUMP-FROM))        "For debugging, set JUMP-FROM reg"
CONFIDENT-ICALL
     (GLOBL CONFIDENT-ICALL)
     (MOVL (REG FUN %%EXTEND-TEMPLATE-OFFSET) TP)       "Extract template"
     (JMP (REG TP))                     "Transfer to template"
;    (JMP (@ (REG TP %%TEMPLATE-CHUNK-OFFSET))) "This might be better, yes?"

ICALL-BAD-HP-TAG
;     (PUSHL (STATIC *ICALL-AP-ALIGNMENT-FAULT*))
;     (BSBW FAULT1)
     (BPT)
     (JMP (@ (SLINK ICALL)))
ICALL-BAD-AP
     (PUSHL (STATIC *ICALL-AP-ALIGNMENT-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK ICALL)))
ICALL-BAD-AP-TEMPLATE
     (PUSHL (STATIC *ICALL-RETURN-NOT-TEMPLATE-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK ICALL)))
ICALL-BAD-FUN
     (PUSHL (STATIC *ICALL-NOT-EXTEND-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK ICALL)))
ICALL-BAD-FUN-TEMPLATE
     (PUSHL (STATIC *ICALL-TEMPLATE-NOT-TEMPLATE-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK ICALL)))
ICALL-NOT-PROCEDURE
     (PUSHL (STATIC *ICALL-NOT-PROCEDURE-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK ICALL)))
ICALL-WNA
     (PUSHL (STATIC *ICALL-WRONG-NUMBER-ARGS-FAULT*))
     (BSBW FAULT1)
     (JMP (@ (SLINK ICALL)))
     )

(LSET *HEAP-OVERFLOW-PENDING?* '())
(LSET *STACK-OVERFLOW-PENDING?* '())

;;; IRETURN: return from procedure, and be paranoid about it.

(LAP
IRETURN
     (GLOBL IRETURN)
PARANOID-IRETURN
     (GLOBL PARANOID-IRETURN)

     (BBS (LIT 2) SP IRETURN-BAD-SP)    "Check stack alignment"
     (BICB3 (LIT #b11111000) (REG SP) R0)       "Get ret addr type code"
     (CMPB R0 (LIT %%TEMPLATE-TAG))     "Is type template?"
     (BNEQ IRETURN-BAD-SP-TEMPLATE)     "Barf if not"
     (MOVL TP (SLINK JUMP-FROM))        "For debugging, set JUMP-FROM reg"

CONFIDENT-IRETURN
     (GLOBL CONFIDENT-IRETURN)

     ;; Maybe invoke GC.
     (CMPL HP (SLINK HEAP-LIMIT))       "Check proximity to doom"
     (BGEQU IRETURN-HEAP-OVERFLOW)
CONTINUE-IRETURN
     (MOVL (REG+ SP) TP)                "Pop return address"
     (JMP (REG TP))                     "and go to it"

IRETURN-HEAP-OVERFLOW
     (CMPL (@ (STATIC *HEAP-OVERFLOW-PENDING?*)) (SLINK NULL))
     (BNEQ CONTINUE-IRETURN)
     (MOVL (SLINK TRUE) (@ (STATIC *HEAP-OVERFLOW-PENDING?*)))
     (MOVL SP AP)                       "Set up AP for upcoming call"
     (PUSHL VAL)
     (MOVL (@ (STATIC HANDLE-HEAP-OVERFLOW)) FUN)
     (JMP (@ (SLINK ICALL)))            "Call overflow handler"

IRETURN-BAD-SP
     (PUSHL (STATIC *IRETURN-BAD-SP-FAULT*))
     (BSBW FAULT1)
IRETURN-BAD-SP-TEMPLATE
     (PUSHL (STATIC *IRETURN-BAD-SP-TEMPLATE-FAULT*))
     (BSBW FAULT1))

;;; APPLY, in two flavors, and the LEXPR-arg conser

;;; IAPPLY: the compiler supposedly will some day generate references to
;;; this routine when compiling calls to APPLY.  Going to the APPLY
;;; procedure itself (see below) is bletcherous since the first argument
;;; (the procedure to be called) needs to get squeezed out of the way.
;;; The procedure to call is in FUN, list of args to spread is in VAL.
;;; Internally (APPLY FOO A B C) looks exactly like (FOO A B C) except
;;; that we jump not to ICALL but to IAPPLY.

(LAP (DEFINE-SLINK IAPPLY)
     (MOVL (REG+ SP) VAL)               "Fetch the list"
     (BRB APTST)                        "Enter loop in middle"
APLOOP
     (BICB3 (LIT #b11111000) VAL R0)    "Make sure it's a good list"
     (CMPB R0 (LIT %%PAIR-TAG))
     (BNEQ APFAULT)                     "Barf if not"
     (PUSHL (REG VAL %%CAR-OFFSET))     "Push list's CAR"
     (MOVL (REG VAL %%CDR-OFFSET) VAL)  "Next list element"
APTST
     (CMPL VAL (SLINK NULL))            "Done when we hit ()"
     (BNEQ APLOOP)                      "More to spread"
     (JMP (@ (SLINK ICALL)))            "Punt, at last"
APFAULT
     (PUSHL (STATIC *IAPPLY-IMPROPER-LIST-FAULT*))
     (BSBW FAULT1)
     (BRB APLOOP))

;;; The following is the T-callable definition for APPLY, e.g. as it
;;; might be called by interpreted code: (APPLY procedure -args- lastarg)
;;; Note: MOVC3 sets R5 (= VAL) to 0.
;;; Does this work when the length count for the MOVC3 is zero?

(DEFINE-LAP-PROCEDURE APPLY ((LEXPR 2 0 0))
     (MOVL (REG AP -4) FUN)             "Save the procedure"
     (SUBL3 SP AP R0)                   "Get # of args"
     (SUBL2 (LIT 4) R0)                 "Adjust count for upcoming BLT"
     ;; (ENTER-CRITICAL GC)
     (MOVC3 R0 (REG SP) (REG SP 4))     "Shift them down"
     ;; (LEAVE-CRITICAL GC)
     (TSTL (REG+ SP))                   "Pop duplication"
     (JMP (@ (SLINK IAPPLY))))

;;; Alternate code for APPLY's arg vector shift - might be more pleasant to
;;; use in the presence of random GC's in the middle of MOVC3 instructions,
;;; which might not understand the fact that R1 and R3 point into the middle
;;; of the stack, and R5 (= VAL) has some random counter in it.
;;; See page 235 of the Vax Architecture handbook, and beware.

(COMMENT
     (SUBL3 SP AP R1)                   "Get # of args"
     (ASHL (LIT 2) R1 R1)               "Turn it into an index"
     (SUBL3 (LIT 1) R1 R0)              "We step R0 in parallel with R1"
     (BRB AP-TEST1)                     "Jump to test"
AP-LOOP1
     (MOVL (IDX R0 (REG SP)) (IDX R1 (REG SP))) "Move one word"
AP-TEST1
     (DECL R1)                          "Step indices"
     (SOBGTR (LIT 0) R0 AP-LOOP1))

;;; LEXPR &REST-arg conser: pop args off stack into a list!
;;; Number of required args is passed in R0; return address
;;; is on the stack (entered via JSB).  (Is this safe?!)

(LAP (DEFINE-SLINK LEXPR-SETUP)
     (MOVL (REG+ SP) R2)                "Pop return address"
     (SUBL3 SP AP R1)                   "Get number of args in R1"
     (ASHL (LIT -2) R1 R1)
     (SUBL2 R0 R1)                      "Subtract requireds"
     (MOVL (SLINK NULL) VAL)            "Initialize accumulator"
     (BRB LX-TEST)
LX-LOOP
     (MOVL VAL XP)                      "Stow list so far"
     (MOVAQ (REG+ HP) VAL)              "Cons next cell"
     (ADJUST-TAG VAL VAL %%PAIR-TAG)
     (MOVL XP        (REG VAL %%CDR-OFFSET))    "Store CDR = prev"
     (MOVL (REG+ SP) (REG VAL %%CAR-OFFSET))    "Store CAR = next arg"
LX-TEST
     (SOBGEQ R1 LX-LOOP)                "Loop"
     (JMP (REG R2)))                    "Return"

;;; The CATCH/THROW implementation consists of the following two little
;;; routines, which bracket a call to system code written in T.
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
    (PUSHL (REG SP))                    "Arg becomes 2nd arg"
    (MOVL FUN (REG SP 4))               "First arg is SELF"
    (MOVL (@ (STATIC INTERNAL-THROW)) FUN)      "Punt to T code"
    (JMP (@ (SLINK ICALL)))))

(LAP (PROCEDURE P$HANDLE-ESCAPE-PROCEDURE-STUB
                T$HANDLE-ESCAPE-PROCEDURE-STUB C$HANDLE-ESCAPE-PROCEDURE-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-ESCAPE-PROCEDURE)) FUN)    "Trampoline"
     (JMP (@ (SLINK ICALL))))


;;; (PRIMITIVE-THROW cont value)
;;; Note that "cont" is an actual EXTEND pointer, NOT a pointer to a return
;;; address as one would find in AP on a function call.  The extend's
;;; template is the return address, thus one must subtract (or do a pop)
;;; in order to point to the return address.
;;; *** This is potentially a target for open-coding.

(DEFINE-LAP-PROCEDURE PRIMITIVE-THROW ((EXPR 2 0 0))
     (MOVL (REG+ SP) VAL)               "2nd arg is value to yield"
     (SUBL3 (LIT 4) (REG SP) SP)        "1st arg is continuation"
     (JMP (@ (SLINK IRETURN))))         "Return"


;;; T-callable procedures:

;;; Some of these are things that would prefer to be either open-coded or
;;; written in T or both.  Most lose on error-checking - beware!

;;; (%MAKE-STRING size) - figure out later how to lock out interrupts
;;; We need enough space to account for:
;;; - 8 bytes for the header,
;;; - the 2-byte length count in the string text,
;;; - the characters in the string,
;;; - a null byte at the end (Unix/Eunice only - a convenience).
;;; Thus the magic number 8 + 2 + 1 + 7 (7 for rounding up) = 18.

(DEFINE-LAP-PROCEDURE %MAKE-STRING ((EXPR 1 0 0))
     (ASHL (LIT -3) (REG SP) R0)        "Get length unnormalized"
     (ADDL3 (LIT 18) R0 R1)             "Round to quad past 11"
     (BICL2 (LIT 7) R1)                 "R0 = length, R1 = total size"
     ;; (ENTER-CRITICAL HP)
     (ADJUST-TAG HP VAL %%STRING-TAG)           "Get pointer"
     (ADDL2 R1 HP)                              "Bump top of heap"
     (ADDW3 (LIT 1) R0 (REG VAL (- 8 %%STRING-TAG)))    "Store text length"
     (ADDL3 (LIT (- 10 %%STRING-TAG))
            VAL
            (REG VAL %%STRING-POINTER-OFFSET))  "Store pointer"
     (CVTLW R0 (REG VAL %%STRING-LENGTH-OFFSET))   "Store length"
;    (CLRW     (REG VAL %%STRING-BASE-OFFSET))     "Store offset"
     ;; (LEAVE-CRITICAL HP)
     (MOVL AP SP)                               "All done"
     (JMP (@ (SLINK IRETURN))))

;;; (%STRING-EQUAL? string1 string2)
;;; ***  Potentially rewrite in lisp in terms of STRING-LENGTH and
;;; some new 3-arg primop which only does the comparison.

(DEFINE-LAP-PROCEDURE %STRING-EQUAL? ((EXPR 2 0 0))
     (MOVL (SLINK FALSE) VAL)                   "Prepare for failure"
     (MOVL (REG AP -4) XP)                      "Fetch args"
     (MOVL (REG AP -8) YP)
     (CMPW (REG XP %%STRING-LENGTH-OFFSET) (REG YP %%STRING-LENGTH-OFFSET))
     (BNEQU RETF)                               "Lose if length mismatch"
     ;; (ENTER-CRITICAL GC)
     (CMPC3 (REG XP %%STRING-LENGTH-OFFSET)
            (@ (REG XP))
            (@ (REG YP)))                       "Lose if text mismatch"
     ;; (LEAVE-CRITICAL GC)
     (BNEQU RETF)
     (MOVL (SLINK TRUE) VAL)
RETF
     (MOVL AP SP)                               "All done"
     (JMP (@ (SLINK IRETURN))))

;;; (STRING-REPLACE target source count)
;;; Clobbers the target string with the text of the source string.
;;; Returns target.

(DEFINE-LAP-PROCEDURE STRING-REPLACE ((EXPR 3 0 0))
     (MOVL (REG SP 4) XP)               "Fetch source"
     (MOVL (REG SP 8) VAL)                      "Fetch target"
     (ASHL (LIT -3) (REG SP) R0)
     (MOVC3 R0 (@ (REG XP)) (@ (REG VAL)))      "Copy text"
     ;; MOVC3 clobbers R5.
     (MOVL (REG SP 8) VAL)                      "Fetch target again"
     (MOVL AP SP)                               "All done"
     (JMP (@ (SLINK IRETURN))))

;;; (%STRING-POSQ char string)
;;; Find index of character within string.

(DEFINE-LAP-PROCEDURE %STRING-POSQ ((EXPR 2 0 0))
     (ASHL (LIT -3) (REG SP 4) R2)      "Get character"
     (MOVL (REG SP) XP)                 "Get string"
     (MOVL (SLINK NULL) VAL)            "Prepare for failure"
     (LOCC R2 (REG XP %%STRING-LENGTH-OFFSET) (@ (REG XP)))     "Locate char"
     ;; "Z is cleared if equality is detected."  BEQL branches if Z EQL 1.
     (BEQL SPOSQ-LOSE)                  "Return false if not found"
     ;; R0 = number of bytes remaining in string, including found byte
     (SUBW3 R0 (REG XP %%STRING-LENGTH-OFFSET) R0)
     (ASHL (LIT 3) R0 VAL)
SPOSQ-LOSE
     (MOVL AP SP)                               "All done"
     (JMP (@ (SLINK IRETURN))))


;;; (STRING-HASH string) => fixnum

(DEFINE-LAP-PROCEDURE STRING-HASH ((EXPR 1 0 0))
     (MOVL (REG SP) VAL)                        "Fetch string"
     ;; (CRC tbl.ab inicrc.rl strlen.rw stream.ab)  result to R0
     (CRC (REL CRCTAB)
          (LIT -1)
          (REG VAL %%STRING-LENGTH-OFFSET)
          (@ (REG VAL %%STRING-POINTER-OFFSET)))        "Hash"
     (BICL3 (LIT #x+080000007) R0 VAL)  "Fixnumize result"
     (MOVL AP SP)                       "Return"
     (JMP (@ (SLINK IRETURN)))
     (ALIGN 2)
     ;; CRC table stolen from NIL.  Thanks to Rick Bryan and Bob Kerns.
     ;; This is AUTODIN-II; polynomial is EDB88320.
CRCTAB
     (LONG #x+000000000 #x+0FDB71064 #x+0FB6E20C8 #x+006D930AC)
     (LONG #x+0F6DC4190 #x+00B6B51F4 #x+00DB26158 #x+0F005713C)
     (LONG #x+0EDB88320 #x+0100F9344 #x+016D6A3E8 #x+0EB61B38C)
     (LONG #x+01B64C2B0 #x+0E6D3D2D4 #x+0E00AE278 #x+01DBDF21C))


;;; (PUSH-MAGIC-FRAME chain token arg procedure)
;;; Sets up a recognizable configuration on the stack, for use in dynamic state
;;; winding and unwinding.
;;; If (EQ? (EXTEND-TEMPLATE F) *MAGIC-FRAME-TEMPLATE*),
;;; then (XREF F 2) => chain and (XREF F 1) => token.
;;; The procedure gets passed two arguments: the magic frame itself, and arg.
;;; Pretty kludgey, I admit.  So what.

(DEFINE-LAP-PROCEDURE PUSH-MAGIC-FRAME ((EXPR 4 3 0))
     (MOVL (REG+ SP) FUN)               "4th arg is proc to call"
     (PUSHAL (DATUM MAGIC-FRAME-TEMPLATE))
     (MOVL SP AP)                       "Set up for call to proc"
     (PUSHAL (REG SP 4))                "Pointer to frame as extend"
     (PUSHL (REG SP 8))                 "3rd arg becoms 2nd arg to proc"
     (JMP (@ (SLINK ICALL))))

(DEFINE *MAGIC-FRAME-TEMPLATE*
  (LAP-TEMPLATE MAGIC-FRAME-TEMPLATE C$MAGIC-FRAME-TEMPLATE
                ((RETURN)
                 (SUPERIOR (COMMA (MAKE-SPECIAL-TAG 'T 'PUSH-MAGIC-FRAME)))
                 (HANDLER P$HANDLE-MAGIC-FRAME-STUB)
                 (SIZE -1 0))
    (GLOBL MAGIC-FRAME-TEMPLATE)
    (MOVAL (REG SP 12) SP)
    (JMP (@ (SLINK IRETURN)))))

(LAP (PROCEDURE P$HANDLE-MAGIC-FRAME-STUB
                T$HANDLE-MAGIC-FRAME-STUB C$HANDLE-MAGIC-FRAME-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-MAGIC-FRAME)) FUN) "Trampoline"
     (JMP (@ (SLINK ICALL))))

;;; (ZERO-MEM start count)
;;; COUNT is fixnum number of quadwords (i.e. machine number of bytes).

(DEFINE-LAP-PROCEDURE ZERO-MEM ((EXPR 2 0 0))
  (MOVL (REG SP) R7)
  (MOVL (REG SP 4) R3)
  (BRB ZGO)                             ; Enter loop at end-test
ZLOOP
  (MOVC5 (LIT 0)                        ; srclen.rw
         (REG SP)                       ; srcaddr.ab
         (LIT 0)                        ; fill.rb
         (LIT 65528.)                   ; dstlen.rw
         (REG R3))                      ; dstaddr.ab
  ;; After MOVC5, R3 = address of one byte beyond destination string
ZGO
  (MOVL R7 R6)                          ; Save old count
  (ACBL (LIT 0) (LIT -65528.) R7 ZLOOP) ; Loop if new count is still positive
ZEND
  (MOVC5 (LIT 0)
         (REG SP)
         (LIT 0)
         R6                             ; Residue length was saved above
         (REG R3))
  (MOVL AP SP)                          ; Return 0 (left in R5 (= VAL) by MOVC5)
  (JMP (@ (SLINK IRETURN))))

;;; (COPY-MEM target source count) - same arg order as REPLACE.
;;; COUNT is fixnum number of pesos.

(DEFINE-LAP-PROCEDURE COPY-MEM ((EXPR 3 0 0))
  (ASHL (LIT -1) (REG SP) R7)
  (MOVL (REG SP 4) R1)
  (MOVL (REG SP 8) R3)
  (BRB CGO)                             ; Enter loop at end-test
CLOOP
  (MOVC3 (LIT 65528.)                   ; srclen.rw
         (REG R1)                       ; srcaddr.ab
         (REG R3))                      ; dstaddr.ab
  ;; After MOVC, R1 = address of one byte beyond source string
  ;;         and R3 = address of one byte beyond destination string
CGO
  (MOVL R7 R6)                          ; Save old count
  (ACBL (LIT 0) (LIT -65528.) R7 CLOOP) ; Loop if new count is still positive
CEND
  (MOVC3 R6                             ; Residue length was saved above
         (REG R1)
         (REG R3))
  (MOVL (REG SP 8) VAL)                 ; Return target
  (MOVL AP SP)
  (JMP (@ (SLINK IRETURN))))

;;; (CALL-XENOID xenoid argn ... arg2 arg1)
;;; Gross little internal thing for calling C or other random VAX code,
;;; e.g. the standard i/o library.  Handle with care!  Disable GC interrupts!
;;; On VMS we need to maintain the integrity of FP - we could save and restore
;;; it here but the system wants it to be valid all the time.  Lose lose.

(DEFINE-LAP-PROCEDURE CALL-XENOID ((LEXPR 1 0 0))
     (BSBB CALL-XENOID-1)
     (MOVL R0 VAL)                      "Value as machine pointer!"
     (MOVL AP SP)                       "Return"
     (JMP (@ (SLINK IRETURN)))

CALL-XENOID-1
;    (CLRL VAL)                         "Clear all regs"
;    (CLRL XP)                          " in case somehow we GC"
;    (CLRL YP)                          " while running the foreign routine"
;    (CLRL FUN)
;    (CLRL TP)
     (PUSHL (REG SP))                   "Copy return address"
     (MOVL (@ (REG AP -4)) R1)          "Function to R1"
     (SUBL3 SP AP R0)                   "Compute # args"
     (ASHL (LIT -2) R0 R0)
     (SUBL3 (LIT 3) R0 (REG SP 4))      "Put nargs where ret addr was"
     (MOVL HP (SLINK SAVED-HP))         "Save HP for interrupt re-entry"
     (MOVL AP (SLINK SAVED-AP))         "Save AP for stack groveling"
     (MOVL (SLINK SAVED-FP) FP)
     (CALLG (REG SP 4) (REG R1))        "Do the call"
     (MOVL FP (SLINK SAVED-FP))
     (INCL (SLINK SAVED-HP))            "What happens if we get inter-"
     (SUBL3 (LIT 1) (SLINK SAVED-HP) HP)"(*) rupted between (*) and (**)?"
     (CLRL (SLINK SAVED-HP))            "(**) See CLEANUP-INTERRUPT-FRAME."
     (RSB)                              "Pretty kludgey"
)

;;; This is really kind of weird... if the args are flonums, how do we get them
;;; onto the stack in the first place?  With FLONUM-HIGH-PESO and all that?
;;; For now, the main thing we need this for is "atof".

(DEFINE-LAP-PROCEDURE CALL-XENOID-YIELDING-FLONUM ((LEXPR 1 0 0))
     (BSBW CALL-XENOID-1)
     (MOVAQ (REG+ HP) VAL)
     (ADJUST-TAG VAL VAL %%FLONUM-TAG)
     (MOVD R0 (REG VAL (~ %%FLONUM-TAG)))       "Value as flonum"
     (MOVL AP SP)                       "Return"
     (JMP (@ (SLINK IRETURN))))


;;; Random templates of various assorted sorts, types, and kinds.

;;; Symbol...
(LAP (PROCEDURE P$HANDLE-SYMBOL-STUB
                T$HANDLE-SYMBOL-STUB C$HANDLE-SYMBOL-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-SYMBOL)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *SYMBOL-TEMPLATE*
  (LAP-TEMPLATE SYMBOL-TEMPLATE C$SYMBOL-TEMPLATE
                ((SIZE %%SYMBOL-SIZE 0)
                 (HANDLER P$HANDLE-SYMBOL-STUB))
    (GLOBL SYMBOL-TEMPLATE)
    (JMP (@ (SLINK INAPPLICABLE)))))

;;; Vcell...
(LAP (PROCEDURE P$HANDLE-VCELL-STUB
                T$HANDLE-VCELL-STUB C$HANDLE-VCELL-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-VCELL)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *VCELL-TEMPLATE*
  (LAP-TEMPLATE VCELL-TEMPLATE C$VCELL-TEMPLATE
                ((EXPR 0 0 0)
                 (SIZE %%VCELL-SIZE 0)
                 (HANDLER P$HANDLE-VCELL-STUB))
    (GLOBL VCELL-TEMPLATE)
    (MOVL (@ (REG FUN)) VAL)            ; Assert: %%VCELL-CONTENTS-OFFSET = 0
    (JMP (@ (SLINK IRETURN)))))

;;; Bogus entities...
(LAP (PROCEDURE P$HANDLE-ENTITY-STUB T$HANDLE-ENTITY-STUB C$HANDLE-ENTITY-STUB
                ((EXPR 2 0 0)))
     ;; Args are (OBJ STATE).  OBJ = 4(sp)  STATE = (sp)
     (MOVL (REG SP 4) VAL)
     (MOVL (REG VAL %%BOGUS-ENTITY-HANDLER-OFFSET) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *BOGUS-ENTITY-TEMPLATE*
  (LAP-TEMPLATE ENTITY-TEMPLATE C$ENTITY-TEMPLATE
                ((LEXPR 0 0 0)
                 (HANDLER P$HANDLE-ENTITY-STUB)
                 (SIZE %%BOGUS-ENTITY-SIZE 0))
    (GLOBL ENTITY-TEMPLATE)
    (MOVL (REG FUN %%BOGUS-ENTITY-PROCEDURE-OFFSET) FUN)
    (JMP (@ (SLINK ICALL)))))

;;; Units...
(LAP (PROCEDURE P$HANDLE-UNIT-STUB
                T$HANDLE-UNIT-STUB C$HANDLE-UNIT-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-UNIT)) FUN)
     (JMP (@ (SLINK ICALL))))

(LAP (PROCEDURE P$GC-COPY-UNIT-STUB T$GC-COPY-UNIT-STUB C$GC-COPY-UNIT-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC GC-COPY-UNIT)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *UNIT-TEMPLATE*
  (LAP-TEMPLATE UNIT-TEMPLATE C$UNIT-TEMPLATE
                ((GC-METHOD P$GC-COPY-UNIT-STUB)
                 (HANDLER P$HANDLE-UNIT-STUB))
    (GLOBL UNIT-TEMPLATE)
    (JMP (@ (SLINK INAPPLICABLE)))))

;;; Vframes...
(LAP (PROCEDURE P$HANDLE-VFRAME-STUB
                T$HANDLE-VFRAME-STUB C$HANDLE-VFRAME-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-VFRAME)) FUN)
     (JMP (@ (SLINK ICALL))))

(LAP (PROCEDURE P$GC-COPY-VFRAME-STUB
                T$GC-COPY-VFRAME-STUB C$GC-COPY-VFRAME-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC GC-COPY-VFRAME)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *VFRAME-TEMPLATE*
  (LAP-TEMPLATE VFRAME-TEMPLATE C$VFRAME-TEMPLATE
                ((GC-METHOD P$GC-COPY-VFRAME-STUB)
                 (HANDLER P$HANDLE-VFRAME-STUB))
    (GLOBL VFRAME-TEMPLATE)
    (MOVL (REG+ SP) XP)                 "Get superior frame"
    (BNEQ VF-FOO)
    (MOVL (REG+ SP) XP)
VF-FOO
    (MOVAL (REG XP -4) SP)              "Clobber SP"
    (JMP (@ (SLINK IRETURN)))))

;;; Vectors...
(LAP (PROCEDURE P$HANDLE-VECTOR-STUB
                T$HANDLE-VECTOR-STUB C$HANDLE-VECTOR-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-VECTOR)) FUN)
     (JMP (@ (SLINK ICALL))))

(LAP (PROCEDURE P$GC-COPY-VECTOR-STUB
                T$GC-COPY-VECTOR-STUB C$GC-COPY-VECTOR-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC GC-COPY-VECTOR)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *VECTOR-TEMPLATE*
  (LAP-TEMPLATE VECTOR-TEMPLATE C$VECTOR-TEMPLATE
                ((GC-METHOD P$GC-COPY-VECTOR-STUB)
                 (HANDLER P$HANDLE-VECTOR-STUB))
    (GLOBL VECTOR-TEMPLATE)
    (JMP (@ (SLINK INAPPLICABLE)))))

;;; Byte vectors...
(LAP (PROCEDURE P$HANDLE-BYTEV-STUB
                T$HANDLE-BYTEV-STUB C$HANDLE-BYTEV-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-BYTEV)) FUN)
     (JMP (@ (SLINK ICALL))))

(LAP (PROCEDURE P$GC-COPY-BYTEV-STUB
                T$GC-COPY-BYTEV-STUB C$GC-COPY-BYTEV-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC GC-COPY-BYTEV)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *BYTEV-TEMPLATE*
  (LAP-TEMPLATE BYTEV-TEMPLATE C$BYTEV-TEMPLATE
                ((GC-METHOD P$GC-COPY-BYTEV-STUB)
                 (HANDLER P$HANDLE-BYTEV-STUB))
    (GLOBL BYTEV-TEMPLATE)
    (JMP (@ (SLINK INAPPLICABLE)))))

;;; Structures... these will be done completely differently at some point...
;;; For now we get the handler from the 0th slot of the structure.
(LAP (PROCEDURE P$HANDLE-STRUCTURE-STUB
                T$HANDLE-STRUCTURE-STUB C$HANDLE-STRUCTURE-STUB
                ((EXPR 2 0 0)))
     ;; We ought to be saying %%STRUCTURE-HANDLER-OFFSET here.  Yuck.
     (MOVL (@ (REG SP 4)) FUN)
     (JMP (@ (SLINK ICALL))))

(LAP (PROCEDURE P$GC-COPY-STRUCTURE-STUB
                T$GC-COPY-STRUCTURE-STUB C$GC-COPY-STRUCTURE-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC GC-COPY-STRUCTURE)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *STRUCTURE-TEMPLATE*
  (LAP-TEMPLATE STRUCTURE-TEMPLATE C$STRUCTURE-TEMPLATE
                ((GC-METHOD P$GC-COPY-STRUCTURE-STUB)
                 (HANDLER P$HANDLE-STRUCTURE-STUB))
    (GLOBL STRUCTURE-TEMPLATE)
    (JMP (@ (SLINK INAPPLICABLE)))))

;;; Xenoids...
(LAP (PROCEDURE P$HANDLE-XENOID-STUB
                T$HANDLE-XENOID-STUB C$HANDLE-XENOID-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-XENOID)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *XENOID-TEMPLATE*
  (LAP-TEMPLATE XENOID-TEMPLATE C$XENOID-TEMPLATE
                ((SIZE 0 %%XENOID-SIZE)
                 (HANDLER P$HANDLE-XENOID-STUB))
    (GLOBL XENOID-TEMPLATE)
    (JMP (@ (SLINK INAPPLICABLE)))))

;;; Populations...
(LAP (PROCEDURE P$HANDLE-POPULATION-STUB
                T$HANDLE-POPULATION-STUB C$HANDLE-POPULATION-STUB
                ((EXPR 2 0 0)))
     (MOVL (@ (STATIC HANDLE-POPULATION)) FUN)
     (JMP (@ (SLINK ICALL))))

(DEFINE *POPULATION-TEMPLATE*
  (LAP-TEMPLATE POPULATION-TEMPLATE C$POPULATION-TEMPLATE
                ((SIZE 1 1)  ;make size consistent with MAKE-POPULATION-1
                 (HANDLER P$HANDLE-POPULATION-STUB))
    (GLOBL POPULATION-TEMPLATE)
    (JMP (@ (SLINK INAPPLICABLE)))))

;;; Operation dispatch.

(define *operation-template*
  (lap-template t-operation c-operation
		((lexpr 1 0 0)
		 (handler p-handle-operation)
		 (size 8 0))	; %%operation-size
    (subl3 sp ap r0)		; Number of args * 4
    (clrq (-reg sp))		; Make space for 3 new args
    (clrl (-reg sp))
    (movc3 r0 (reg sp 12) (reg sp))  ; Move everything down to make room
    (movl fun (reg ap -8))	; 2nd arg becoms the operation
    (movl (@ (static *the-buck-stops-here*)) 
	  (reg ap -12))  	; 3rd arg is the "next" frob
    (movl (@ (static operate)) fun)
    (jmp (@ (slink icall)))))

(lap (procedure p-handle-operation t-handle-operation c-handle-operation
		((expr 2 0 0)))
     (movl (reg sp 4) val)
     (movl (reg val %%%operation-handler-offset) fun)
     (jmp (@ (slink icall))))

;;; Return from operation dispatch - method gets invoked.

(define *dispatch-ret-template*
  (lap-template t-dispatch-ret c-dispatch-ret
		((return)
                 (handler p$handle-vframe-stub))
    (movl val fun)		; Returned value is method to invoke
    (tstl (reg sp))		; See whether the alignment word is there
    (bneq okay-2)
    (tstl (reg+ sp))		; If so, get rid of it
okay-2
    (subl3 (lit 4) (reg+ sp) ap) ; Move typeless frame pointer back to AP
    (jmp (@ (slink icall)))))	; Invoke the method

;;; (OPERATE obj op next self . args)
;;;    Perform generic operation dispatch.

(define operate
  (lap-procedure p-operate t-operate c-operate ((lexpr 4 0 0)
						(definee operate))
    (pushal (reg ap 4))		; Save frame pointer
    (bbs (lit 2) sp okay-1)
    (clrl (-reg sp))		; Align stack, maybe
 okay-1
    (pushal (uref t-dispatch-ret t-operate))	; Continuation
    (pushl (reg ap -4))		; 1st argument is object to dispatch on
    (pushal (reg ap 4))		; 2nd argument is pointer to state block
    (moval (reg sp 8) ap)	; Two args
    (movl (@ (static perform-dispatch)) fun)
    (movl (reg fun -4) tp)	; Jump off to dispatch code
    (jmp (reg tp))

  ;; ().  This comes at the end of the file to try to ensure that its
  ;; address doesn't fall in character range.    (This may no longer be a
  ;; consideration now that we have independent assembly?)

     (ALIGN 3)                          "Here is () itself."
     (SPACE (+ 8 (- %%NULL-TAG %%PAIR-TAG)))    "Hack for carcdrability"
 NULL0
     (DEF NULL  (+ NULL0 %%PAIR-TAG))
     (DEF FALSE (+ NULL0 %%PAIR-TAG))
     (PESO NULL NULL)
     (GLOBL NULL)
     (GLOBL FALSE)))
