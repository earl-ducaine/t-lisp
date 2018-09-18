(HERALD AEXENO
        (ENV TSYS)
        (PRE-COOK))

;;; Copyright (c) 1983, 1984 Yale University

;;;; LAP code for OS interface

;;; This file should contain the machine-level Apollo-specific operations.
;;; Initialization:  we don't need to do much here besides setup HP.

(DEFINE *XENO-OPERATING-SYSTEM* 'AEGIS)

(LAP (RANDOM-DATA
      (GLOBL XENO-DATA)
XENO-DATA
      (GLOBL SCHEME-ENTRY)
SCHEME-ENTRY
      (DEFSYM JUMP-OPCODE #x+4EF9)
      (HALFPESO JUMP-OPCODE)            "a jump long opcode to jump to..."
      (ADDRESS PURE-SCHEME-CODE)                "the address of our pure code"
      (EXTERNAL THE-SLINK)
      (ADDRESS THE-SLINK)               "the address of our data section"
      (HALFPESO 0)                     
      (ADDRESS SCHEME-ENTRY)            "Initial TP value, where we can get it"
      )

     (INITIAL-VALUE *GC-MARGIN* 32768)  

PURE-SCHEME-CODE
     (MOVE.L DB (-REG SP))              "Save caller's DB"
     (MOVE.L (LIT -1.) (-REG SP))       "should be 0,-1 to dbg (unit lst ptr)" 
     (MOVE.L A0 (-REG SP))              "Save our ECB pointer"
     (MOVE.L SB (-REG SP))              "save our caller's SB"
     (MOVE.L (REG A0 6.) SLP)           "Get our DB, we call it SLP"
     (MOVE.L (REG A0 12.) TP)           "Get initial TP"

     (TP-IS SCHEME-ENTRY)
     
XENO-INITIALIZE              
     (lea (reg sp 20) val)

     (move.l (static *as_$stack_low*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) (reg xp))

     (move.l (static *as_$guard1*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) (reg xp))
 
     (move.l (static *as_$guard2*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) (reg xp))
                                         
     (move.l (static *heap-1-path*) xp) 
     (move.l (reg+ val) (reg xp))
                                         
     (move.l (static *heap-1-path-length*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) (reg xp))

     (move.l (static *heap-1-begin*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) d7)
     (move.l d7 (reg xp))

     (move.l (static *heap-2-path*) xp) 
     (move.l (reg+ val) (reg xp))
                                         
     (move.l (static *heap-2-path-length*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) (reg xp))
                                         
     (move.l (static *heap-2-begin*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) (reg xp))

     (move.l (static *heap-size*) xp) 
     (move.l (reg+ val) fun)
     (move.l (reg fun) d5)
     (move.l d5 (reg xp))

     (MOVE.L D7 D6)                     "Save HP as fixnum"
     (MOVE.L (STATIC *HEAP-BEGIN*) XP)
     (MOVE.L D7 (REG XP))               "Set top-of-heap ptr"
     (ADDI.B (LIT %%HEAP-TAG) D7)       "set type of heap ptr"
     (MOVE.L D7 HP)                     "put it where it really belongs"
     (ADD.L D5 D6)
     (MOVE.L (STATIC *HEAP-END*) XP)
     (MOVE.L D6 (REG XP))
     (MOVE.L D6 (SLINK HEAP-LIMIT))

     (EXTERNAL KERNEL-ENTRY)
     (MOVE.L (VCON KERNEL-ENTRY) A0)
     (MOVE.L (SLINK KERNEL-DATA) TP)
     (TP-IS ())
     (JMP (REG A0)))

;;; Do a TRAP #9 (undefined TRAP instruction).

(DEFINE-LAP-PROCEDURE BPT ((EXPR 0 0 0))
     (BPT)                              "Mainly for exiting to debugger"
     (JMP (SLINK IRETURN)))

;;; Call the Apollo divide routine

(DEFINE-LAP-PROCEDURE FIXNUM-DIVIDE ((EXPR 2 0 0))
     (MOVE.L (REG SP 4) (-REG SP))
     (EXTERNAL (VERBATIM "M$DIS$LLL"))
     (MOVE.L (VCON (VERBATIM "M$DIS$LLL")) A0)
     (JSR (REG A0))
     (ASL.L (LIT 3) D0)
     (MOVE.L D0 A0)
     (LEA (REG SP 12) SP)
     (JMP (SLINK IRETURN))
     )

;;;; Fault Handling

;;; This is how we get control of machine synchronous & asynchronous faults
;;; The Aegis nucleus fim catches  fault & sets up the diagnostic frame on our
;;; stack, then it calls the static fault handler passing a pointer to the
;;; diagnostic frame and a deferred? flag.  We align the stack and then 
;;; format the fault frame for T.  "Machine fault" means asynchronous wrt
;;; T (odd addr, ill addr, stop, quit, etc ); "lisp fault" means T synchronous
;;; (wrong number of arguments, calling non procedure, etc).  I will fix
;;; these horrible terms sometime.

;;; Format of fault frame on stack (3 binary digits indicate alignment):
;;;
;;; 000 dc.l fault frame template  <--- SP
;;; 100 dc.l bytev length
;;; 000 dc.l bytev template
;;; 100 dc.w index in bytev to DF_D0
;;; 110 dc.w =0 means "lisp fault"; =1 means "machine fault"
;;; 000 dc.l SHP
;;; 100 dc.l SAP
;;; 000 dc.l JF
;;; 100 dc.l old SP
;;; 000 some number of bytes for alignment
;;;  ... some randomness in here, old SP points into this
;;;     dc.w $DFDF                <---- Apollo dignostic frame begins
;;;     dc.l fault status
;;;     dc.l D0

(LAP (INCLUDE "/us/ins/fault.ins.asm")
     (RANDOM-DATA
SFH-ECB
      (GLOBL SFH-ECB)
      (HALFPESO JUMP-OPCODE)
      (ADDRESS SFH-PURE)
      (ADDRESS THE-SLINK)
SFH-PROCEDURE
      (GLOBL SFH-PROCEDURE)
      (ADDRESS SFH-ECB)
      (ADDRESS SFH-ECB)
      )

SFH-PURE
     (MOVE.L DB (-REG SP))
     (MOVE.L SB (-REG SP))
     (MOVE.L (REG A0 6.) SLP)
     (BTST (LIT 0) (SLINK BITS))        "CALL-XENOID in transition"
     (BNE.S SFH-IGNORE-FAULT)
     (BTST (LIT 1) (SLINK BITS))        "In fault? interrupted while"
     (BEQ.S SFH-CONTINUE)               "returning from a fault"

SFH-IGNORE-FAULT
       (ST D0)                       "Ret val says proceed"
       (MOVE.L (REG+ SP) SB)
       (MOVE.L (REG+ SP) DB)
       (RTS)

SFH-CONTINUE
     (MOVE.L (REG SP 12.) A0)
     (MOVE.L SP D6)
     (MOVEQ.L (LIT 1) D7)
     (MOVE.L (SLINK *AEGIS-FAULT*) XP)
     (JMP FAULT-ALIGNMENT)   
     )

(LAP
T-SYNCHRONOUS-FAULT
     (GLOBL T-SYNCHRONOUS-FAULT)
     ;; Jump here with return address and identifying text on the stack
     ;; (e.g. do MOVE.L foo,(SP)- followed by JSR or BSR).

     ;; Make room for diagnostic frame. Since 2 longs already pushed by the
     ;; code that came here, we increment SP by 8 less than DF_SIZE
     ;; Assume for now that SLP is intact here.
     (LEA (REG SP (+ 8 (- 0 DF-SIZE))) SP)      "Alloc space for diag frame"

     (MOVE.W CCR (REG SP))              "Temporary save for CCR"
     ;                                  "Save registers indiag frame"
     (MOVEM.L (VERBATIM "D0-D7/A0-A7") (REG SP (- DF-REGS DF-START)))
     (ADD.L (LIT DF-SIZE) (REG SP (- DF-A7 DF-START)))  "Adjust saved SP"

     (MOVE.L (REG SP (- DF-SIZE 4)) XP) "Put fault type in XP"
     (MOVE.L XP (REG SP (- DF-STATUS DF-START)))
     (MOVE.L (REG SP (- DF-SIZE 8)) (REG SP (- DF-PC DF-START)))
     (MOVE.W (REG SP) (REG SP (- DF-SR DF-START)))      "Save CCR in properly"
     (MOVE.W (LIT DF-PATTERN-VAL) (REG SP (- DF-PATTERN DF-START)))

     (MOVE.L SP A0)                     "Save top of diagnostic frame"
     (MOVE.L A0 D6)                     "Known spot after alignment"
     (CLR.L D7)                         "Show this is LISP fault, not machine"

FAULT-ALIGNMENT
     ;; Assume A0 points to top of diagnostic frame, XP holds error vcell ptr
     ;; D6 points into stack at a known spot so that we can find our way
     ;; past the alignment bytes when we try to return.
     ;; (IF (= d7 0) <lisp fault> <machine fault>)
     ;; D5 is filled with the fault status code for use if we go to LOSING-FAULT.
     (MOVE.L (REG A0 (- DF-STATUS DF-START)) D5)  "get status code from fault"

     (MOVE.L SP D0)                     "Copy SP"
     (ANDI.B (LIT #b11111000) D0)       "D0 is quadword aligned version of SP"

     (MOVE.L A0 D1)                     "Copy ptr to diagnostic frame"
     (SUB.L D0 D1)                      "D1 is size of alignment area"

     (EXG D0 SP)                        "Put aligned SP into effect"

     (MOVE.L D6 (-REG SP))              "Save a spot after alignment, to return"
     (MOVE.L (SLINK JUMP-FROM) (-REG SP))       "Save JUMP-FROM"
     (MOVE.L (SLINK SAVED-AP) (-REG SP))        "Save SAVED-AP"
     (MOVE.L (SLINK SAVED-HP) (-REG SP))        "Save SAVED-HP"

     (MOVE.W D7 (-REG SP))              "Machine-fault-p"

     (MOVEQ.L (LIT 26.) D2)             "Calc offset in bytev to DF_D0"
     (ADD.L D1 D2)                      "...put it into D2"
     (MOVE.W D2 (-REG SP))              "...and save it in the bytev"

     (MOVE.L (SLINK XENO-DATA) TP)
     (TP-IS XENO-DATA)

     (MOVE.L (STATIC *BYTEV-TEMPLATE*) FUN)     "Put BYTEV temp on stack"
     (MOVE.L (REG FUN) (-REG SP))       "so that we can see frame like a bytev"
                                        "Now calculate length of bytev"
     (MOVEQ.L (LIT (- DF-SIZE 6.)) D3)  "(minus offset to DF_D0)"
     (ADD.L D2 D3)                      "Add in length of our random shit"
     (ASL.L (LIT 3) D3)                 "Turn it into a fixnum..."
     (MOVE.L D3 (-REG SP))              "and put in above the bytev template"

     (TST.L (SLINK SAVED-HP))           "Need to restore HP?"
     (BEQ.S FAULT-HP-OK)
     (MOVE.L (SLINK SAVED-HP) HP)
     (BRA FAULT-CONTINUE)
FAULT-HP-OK
     (MOVE.L (REG A0 (- DF-A5 DF-START)) A5)

FAULT-CONTINUE
     ;; Assumes that XP is set to error vcell pointer
     (INITIAL-VALUE *RE-ENTER-ON-FAULT?* FALSE)
     (MOVE.L (SLINK FALSE) D7)             ;is buggy.
     (MOVE.L (STATIC *RE-ENTER-ON-FAULT?*) FUN)
     (CMP.L (REG FUN) D7)
     (BEQ.S LOSING-FAULT)
     (MOVE.L D7 (REG FUN))              "disallow reentering for now"

FAULT-FORCE
     (MOVE.L (REG XP %%VCELL-CONTENTS-OFFSET) FUN)      "Handler in vcell"
     (MOVE.L FUN D7)                    "Is it an extend?"
     (ANDI.B (LIT 7) D7)
     (CMPI.B (LIT %%EXTEND-TAG) D7)
     (BNE.S LOSING-FAULT)                "Don't call it if not"

     ;; All OK now. Set up call to fault-handler.
     (CLR.L (SLINK SAVED-HP))           "Be re-entrent"
     (PEA (DATUM FAULT-RET))            "Ret addr: SP was odd, now even"

     (MOVE.L SP AP)                     "So AP is even too, as desired"
     (PEA (REG SP 4))                   "First handler arg is magic frame"
     (MOVE.L XP (-REG SP))              "Second arg is fault type"
     (JMP (SLINK ICALL))

     ;; We just couldn't cope with the fault
LOSING-FAULT 
     (MOVE.L (SLINK XENO-DATA) TP)
     (TP-IS XENO-DATA)
     (MOVE.L SLP (-REG SP))             "save for retry attempt"

     ;; Should the IN-FAULT bit be turned off here - who cares?
     (MOVE.L (REG XP %%VCELL-ID-OFFSET) YP)     "Get symbol (id) out of vcell"
     (MOVE.L (REG YP %%SYMBOL-PNAME-OFFSET) YP) "Get (pname) from symbol"        
     (MOVE.L D5 (-REG SP))                      "fault status"
     (MOVE.L SP (-REG SP))                      "pointer to status"
     (PEA (REG YP %%STRING-LENGTH-OFFSET))      
     (MOVE.L (REG YP %%STRING-POINTER-OFFSET) (-REG SP))
     
     ;; stop catching faults now?
;     (PEA (DATUM XENO-DEAD-STATUS))
;     (PEA (DATUM XENO-DEAD-PROC-ADDR))
;     (MOVE.L (VCON (VERBATIM "PFM_$RLS_STATIC_FAULT")) A0)
;     (JSR (REG A0))
;     (LEA (REG SP 8.) SP)

     (MOVE.L (SLINK XENO-DATA) TP)
     (MOVE.L (VCON (VERBATIM "T_$CORONER")) A0)
     (JSR (REG A0))
     (LEA (REG SP 16.) SP)

     (MOVE.L (REG+ SP) SLP)
     (MOVE.L (SLINK XENO-DATA) TP)
     (TP-IS XENO-DATA)
     (MOVE.L (STATIC *RE-ENTER-ON-FAULT?*) XP)
     (MOVE.L (SLINK TRUE) (REG XP))             "Force re-entry"
     (PUSH (SLINK *BB-FAULT*))
     (BSR T-SYNCHRONOUS-FAULT)

;     (MOVE.L (REG+ SP) SLP)             "restore slink hopefully"
;     (MOVE.L (SLINK XENO-DATA) TP)      "FAULT-FORCE expects this"
;     (BRA FAULT-FORCE)

     (RANDOM-DATA
XENO-DEAD-STATUS
      (PESO 0)
XENO-DEAD-PROC-ADDR
      (ADDRESS SFH-PROCEDURE)
      ))

;;; random

(LAP (PROCEDURE P$HANDLE-FAULT-FRAME-STUB
                T$HANDLE-FAULT-FRAME-STUB C$HANDLE-FAULT-FRAME-STUB
                ((EXPR 2 0 0)))
     (MOVE.L (STATIC HANDLE-FAULT-FRAME) XP)
     (MOVE.L (REG XP) FUN)              "Trampoline"
     (JMP (SLINK ICALL)))

;;; %%FAULT-LEADER-SIZE 26
;;; %%DIAG-DFD0-OFFSET 6

(DEFINE *FAULT-FRAME-TEMPLATE*
  (LAP-TEMPLATE FAULT-RET C$FAULT-RET   ; should have a gc-method
                ((RETURN)
                 (HANDLER P$HANDLE-FAULT-FRAME-STUB))
    (MOVE.L (REG SP (* %%JF 4)) (SLINK JUMP-FROM))       "Restore jump-from"
    (MOVE.L (REG SP (* %%SAP 4)) (SLINK SAVED-AP))       "Restore saved AP"

    (TST.W (REG SP %%MACHINE-FAULT?))
    (BNE.S RETURN-FROM-MACHINE-FAULT)

;RETURN-FROM-LISP-FAULT
    (MOVE.L (REG SP %%OLD-SP) SP)       "Get to our old SP"
    (MOVE.W (REG SP (- DF-SR DF-START)) CCR)    "restore the condition codes"

    ;; *** assumes the HP is a5
    (MOVEM.L (REG SP (- DF-REGS DF-START)) (VERBATIM "D0-D7/A0-A4"))
    ;; don't pop off HP -- the one we've got is the right one.
    (MOVE.L (REG SP (- DF-A6 DF-START)) A6)
    (LEA (REG SP (- DF-SIZE 4)) SP)     "pop diag frame EXCEPT for rtn pc"
    (RTS)                               "back to faulting code, weeeeee!"

RETURN-FROM-MACHINE-FAULT
    ;; CLEANUP-INERRUPT-FRAME should have done all the dirty work.

    ;; We did a disable in cleanup-interrupt-frame - enable here but turn
    ;; in-fault? bit on to say to ignore the queued quit if there is one.
    ;; If it were not ignored we would have 2 copies of the registers
    ;; on the stack and hacking HP would be very hard.
   
    (MOVE.L (SLINK XENO-DATA) TP)
    (TP-IS XENO-DATA)
    (BSET (LIT 1) (SLINK BITS))
    (MOVE.L (VCON (VERBATIM "PFM_$ENABLE")) A0)
    (JSR (REG A0))
    (BCLR (LIT 1) (SLINK BITS))

    (MOVE.L (REG SP %%OLD-SP) SP)       "Get to known spot on stack"
    (MOVEQ (LIT 1) D0)                  "pfm_$return_to_faulting_code"
    (MOVE.L (REG+ SP) SB)
    (MOVE.L (REG+ SP) DB)

    (RTS)
     ))


;;; This tag is so that you can say "bb;p" as a tddt command.

(LAP (RANDOM-DATA
     (GLOBL BB)
BB   (LEA BB A0)
     (HALFPESO #x+4EF9)
     (ADDRESS BB-PURE)
     (ADDRESS THE-SLINK)
     (HALFPESO 0))

BB-PURE
     (MOVE.L (REG A0 10.) SLP)
     (MOVE.L (SLINK XENO-DATA) TP)
     (TP-IS XENO-DATA)
     (MOVE.L (STATIC *RE-ENTER-ON-FAULT?*) XP)
     (MOVE.L (SLINK TRUE) (REG XP))             "Force re-entry"
     (PUSH (SLINK *BB-FAULT*))
     (BSR T-SYNCHRONOUS-FAULT)
     (BPT)
     (BRA.S BB-PURE)
     )


;;; Initial foreign procedures

;;; APOLLO file objects are not valid Lisp pointers, thus live inside
;;; EXTENDs.  It is assumed that the actual file object is the 0th slot of the
;;; EXTEND.

(DEFINE *STDIN-XENOID*                (%XENOID 0))
(DEFINE *STDOUT-XENOID*               (%XENOID #x+00010000))
(DEFINE *STDERRIN-XENOID*             (%XENOID #x+00020000))
(DEFINE *STDERR-XENOID*               (%XENOID #x+00030000))

(DEFINE *KG_$LOOKUP-XENOID*           (%XENOID "KG_$LOOKUP"))

(DEFINE *STREAM_$OPEN-XENOID*         (%XENOID "STREAM_$OPEN"))
(DEFINE *STREAM_$CREATE-XENOID*       (%XENOID "STREAM_$CREATE"))
(DEFINE *STREAM_$CLOSE-XENOID*        (%XENOID "STREAM_$CLOSE"))
(DEFINE *STREAM_$INQUIRE-XENOID*      (%XENOID "STREAM_$INQUIRE"))
(DEFINE *STREAM_$REDEFINE-XENOID*     (%XENOID "STREAM_$REDEFINE"))
(DEFINE *STREAM_$GET_REC-XENOID*      (%XENOID "STREAM_$GET_REC"))
(DEFINE *STREAM_$GET_BUF-XENOID*      (%XENOID "STREAM_$GET_BUF"))
(DEFINE *STREAM_$PUT_REC-XENOID*      (%XENOID "STREAM_$PUT_REC"))
(DEFINE *STREAM_$PUT_CHR-XENOID*      (%XENOID "STREAM_$PUT_CHR"))
(DEFINE *STREAM_$SEEK-XENOID*         (%XENOID "STREAM_$SEEK"))
(DEFINE *STREAM_$ISAVT-XENOID*        (%XENOID "STREAM_$ISAVT"))

(DEFINE *NAME_$CR_FILE-XENOID*        (%XENOID "NAME_$CR_FILE"))
(DEFINE *NAME_$DELETE_FILE-XENOID*    (%XENOID "NAME_$DELETE_FILE"))
(DEFINE *NAME_$RESOLVE-XENOID*        (%XENOID "NAME_$RESOLVE"))
(DEFINE *NAME_$ADD-XENOID*            (%XENOID "NAME_$ADD"))
(DEFINE *NAME_$DROP-XENOID*           (%XENOID "NAME_$DROP"))

(DEFINE *FILE_$TRUNCATE-XENOID*       (%XENOID "FILE_$TRUNCATE"))

(DEFINE *UID_$GEN-XENOID*             (%XENOID "UID_$GEN"))

(DEFINE *MS_$MAPL-XENOID*             (%XENOID "MS_$MAPL"))
(DEFINE *MS_$UNMAP-XENOID*            (%XENOID "MS_$UNMAP"))

(DEFINE *MST_$SET_GUARD-XENOID*       (%XENOID "MST_$SET_GUARD"))

(DEFINE *PGM_$GET_ARG-XENOID*         (%XENOID "PGM_$GET_ARG"))
(DEFINE *PGM_$GET_ARGS-XENOID*        (%XENOID "PGM_$GET_ARGS"))
(DEFINE *PGM_$EXIT-XENOID*            (%XENOID "PGM_$EXIT"))

(DEFINE *PFM_$INHIBIT-XENOID*         (%XENOID "PFM_$INHIBIT"))
(DEFINE *PFM_$ENABLE-XENOID*          (%XENOID "PFM_$ENABLE"))
(DEFINE *PFM_$SIGNAL-XENOID*          (%XENOID "PFM_$SIGNAL"))
(DEFINE *PFM_$ESTABLISH_FAULT_HANDLER-XENOID*
                                      (%XENOID "PFM_$ESTABLISH_FAULT_HANDLER"))
(DEFINE *PFM_$RELEASE_FAULT_HANDLER-XENOID*
                                      (%XENOID "PFM_$RELEASE_FAULT_HANDLER"))
(DEFINE *PFM-FAULT-HANDLE-XENOID*     (%XENOID 0))

(DEFINE *PROC2_$WHO_AM_I-XENOID*      (%XENOID "PROC2_$WHO_AM_I"))
(DEFINE *PROC2_$GET_INFO-XENOID*      (%XENOID "PROC2_$GET_INFO"))

(DEFINE *ERROR_$PRINT-XENOID*         (%XENOID "ERROR_$PRINT"))
(DEFINE *ERROR_$FIND_TEXT-XENOID*     (%XENOID "ERROR_$FIND_TEXT"))

(DEFINE *T_$SET_MOVE-XENOID*      (%XENOID "T_$SET_MOVE"))
(DEFINE *T_$OBJECT_FILE_STREAMP-XENOID* (%XENOID "T_$OBJECT_FILE_STREAMP"))
;(DEFINE *T_$UASC_LENGTH-XENOID*   (%XENOID "T_$UASC_LENGTH"))
(DEFINE *T_$DISK_FULL-XENOID*     (%XENOID "T_$DISK_FULL"))
(DEFINE *T_$GC_DEATH-XENOID*      (%XENOID "T_$GC_DEATH"))
(DEFINE *T_$UNMAP_AREA-XENOID*    (%XENOID "T_$UNMAP_AREA"))
(DEFINE *T_$READ_IMPURE-XENOID*   (%XENOID "T_$READ_IMPURE"))

(DEFINE *T_$FLADD-XENOID* (%XENOID "T_$FLADD"))
(DEFINE *T_$FLSUB-XENOID* (%XENOID "T_$FLSUB"))
(DEFINE *T_$FLMUL-XENOID* (%XENOID "T_$FLMUL"))
(DEFINE *T_$FLDIV-XENOID* (%XENOID "T_$FLDIV"))
(DEFINE *T_$SIN-XENOID* (%XENOID "T_$SIN"))
(DEFINE *T_$COS-XENOID* (%XENOID "T_$COS"))
(DEFINE *T_$TAN-XENOID* (%XENOID "T_$TAN"))
(DEFINE *T_$ATAN-XENOID* (%XENOID "T_$ATAN"))
(DEFINE *T_$LOG-XENOID* (%XENOID "T_$LOG"))
(DEFINE *T_$EXP-XENOID* (%XENOID "T_$EXP"))
(DEFINE *T_$SQRT-XENOID* (%XENOID "T_$SQRT"))
(DEFINE *T_$FTOA-XENOID* (%XENOID "T_$FTOA"))
(DEFINE *T_$ATOF-XENOID* (%XENOID "T_$ATOF"))

(DEFINE *T_$FLLESS-XENOID* (%XENOID "T_$FLLESS"))
(DEFINE *T_$FLEQUAL-XENOID* (%XENOID "T_$FLEQUAL"))
(DEFINE *T_$FLGREATER-XENOID* (%XENOID "T_$FLGREATER"))
(DEFINE *T_$FLOAT-XENOID* (%XENOID "T_$FLOAT"))
(DEFINE *T_$FIX-XENOID* (%XENOID "T_$FIX"))

(LAP (EXTERNAL XENOID-TEMPLATE)
     (EXTERNAL (VERBATIM "T_$CORONER"))
     )

;;; CALL-XENOID, this is too hairy & too much assembly code.

(LAP
  (DEF HI FUN)
  (DEF LO VAL)
  (DEF ITEM D0)
  (DEF TAG  D1)
  (DEF PREV D2))        ; Previously found string

(DEFINE-LAP-PROCEDURE CALL-XENOID ((LEXPR 3 0 0))       
     ;; syms for the first three arguments to CALL-XENOID 
     (DEFSYM RTN-PLACE -4.)             "'ADDR or 'DATA or 'VOID or NIL"
     (DEFSYM RTN-TYPE -8.)              "'POINTER or 'FIXNUM or NIL"
     (DEFSYM FIRST-ITEM -12.)           "the xenoid to call"

     (MOVE.L HP (SLINK SAVED-HP))
     (MOVE.L AP (SLINK SAVED-AP))       "Save AP for stack groveling"
     
     (LEA (REG AP FIRST-ITEM) HI)       "High limit of frame"  
     (MOVE.L SP LO)                     "Low limit of frame"
     (CLR.L PREV)
     (BSR XENOID-STACK-SCAN)

     (MOVE.L (REG AP FIRST-ITEM) A0)    "Get xenoid"
     (MOVE.L (REG A0) A0)               "Get pointer to xenoid routine"
     (JSR (REG A0))                     "SLP (A6) saved/restored by Apollo"
     (MOVE.L A0 D1)                     "Cache possible ret value, free up VAL"
     (MOVE.L (SLINK SAVED-AP) AP)
     (MOVE.L (REG AP RTN-TYPE) VAL)     "Prepare to return"
     (MOVE.L (SLINK XENO-DATA) TP)      "Get unit addressability"
     (TP-IS XENO-DATA)
     (MOVE.L VAL FUN)
     (MOVE.L (REG AP RTN-PLACE) XP)     "NIL and 'VOID are the same"
     (CMP.L (QUOTE VOID) XP)
     (BEQ.S XENOID-RETURN)
     (CMP.L (SLINK FALSE) XP)
     (BEQ.S XENOID-RETURN)

     (MOVE.L (QUOTE DATA) XP)           "Is return value in D0?"
     (CMP.L (REG AP RTN-PLACE) XP) 
     (BEQ.S XENOID-CONVERT)

     (MOVE.L (QUOTE ADDR) XP)           "Is return value in A0 (now in d1)"
     (CMP.L (REG AP RTN-PLACE) XP) 
     (BNE CX-BAD-RTN-PLACE)
     (MOVE.L D1 D0)
                        
XENOID-CONVERT
     (MOVE.W VAL D1)                    "See if ret-info-2 is a xenoid"
     (ANDI.B (LIT 7.) D1)               
     (CMPI.B (LIT %%EXTEND-TAG) D1)
     (BNE CX-RTN-TYPE-NOT-EXTEND)
     (MOVE.L (STATIC *XENOID-TEMPLATE*) XP)
     (MOVE.L (REG VAL %%EXTEND-TEMPLATE-OFFSET) D2)
     (CMP.L (REG XP) D2)
     (BNE.S XENOID-NON-XENOID-RETURNED)
     
     (MOVE.L D0 (REG VAL))              ;** Should check here that
     (BRA.S XENOID-RETURN)              ;** we really have a xenoid

XENOID-NON-XENOID-RETURNED     
     (MOVE.L (QUOTE POINTER) XP)        "Caller wants a pointer?"
     (CMP.L (REG AP RTN-TYPE) XP)
     (BEQ.S XENOID-RETURN-POINTER)

     (MOVE.L (QUOTE FIXNUM) XP)         "Caller wants a fixnum?"
     (CMP.L (REG AP RTN-TYPE) XP)               "Error if gave us bogus spec"
     (BNE CX-BAD-RTN-TYPE)                      
     (LSL.L (LIT 3.) D0)                "Convert return value to fixnum"

XENOID-RETURN-POINTER
     (MOVE.L D0 VAL)                    "Return converted value"
     
XENOID-RETURN
     (BSET (LIT 0) (SLINK BITS))        "Call-xenoid transition bit, ON"
     (MOVE.L (SLINK SAVED-HP) HP)       ;; Should do something smart if a quit
     (CLR.L (SLINK SAVED-HP))           ;; happens now. Currently, just ignore.
     (BCLR (LIT 0) (SLINK BITS))        "Off"
     (MOVE.L AP SP)
     (JMP (SLINK IRETURN))


;;; This  loop scans the stack one time fixing up  & creating arguments
;;; for the foreign code.  2 pointers are used to scan the stack -- one
;;; at each end of the remaining unscanned area.  When the pointers meet, 
;;; we are done scanning.
XENOID-SCAN-LOOP
     (LEA (REG LO 4.) LO)               "Advance to next item in frame"
XENOID-STACK-SCAN                       "(loop entry point)"
     (CMP.L HI LO)                      "Done converting stack?"
     (BLT.S PROCESS-NEXT-ITEM)          "If so, return to do the call"
     (RTS)
PROCESS-NEXT-ITEM
     (MOVE.L (REG LO) ITEM)             "Get the item"
     (MOVE.B ITEM TAG)                  "What is its type tag"
     (ANDI.W (LIT 7) TAG)
     (ASL.W (LIT 1) TAG)
     (JMP (REG TAG CX-BRANCH-TABLE))    "HACK: should have PC-IDX.W mode"
CX-BRANCH-TABLE
     (BRA.S NEARBY-CX-ERROR)            "fixnum"
     (BRA.S PROCESS-FLONUM)             "*flonum"
     (BRA.S NEARBY-CX-ERROR)            "template"
     (BRA.S NEARBY-CX-ERROR)            "gcfwd"
     (BRA.S XENOID-SCAN-LOOP)           "*extend"
     (BRA.S NEARBY-CX-ERROR)            "pair"
     (BRA.S PROCESS-STRING)             "*string"
     (BRA.S PROCESS-MISC)               "*misc"
NEARBY-CX-ERROR
     (BRA CX-SCAN-R-BAD-OBJ)

PROCESS-FLONUM
     (BCLR (LIT 0) (REG LO 3))
     (BRA.S XENOID-SCAN-LOOP)

PROCESS-STRING
     (MOVE.L ITEM XP)                   "Get lisp string pointer"
     (MOVE.L XP PREV)                   "Remember it"
     (MOVE.W (REG XP %%STRING-BASE-OFFSET) TAG)
     (MOVE.L (REG XP %%STRING-POINTER-OFFSET) XP)
     (ADDA.W TAG XP)                    "Compute ptr to substring"
     (MOVE.L XP (REG LO))               "Store substring pointer"
     (BRA.S XENOID-SCAN-LOOP)

PROCESS-MISC
     (ASR.L (LIT 3) ITEM)                       "Convert to ASCII"

     (CMPI.B (LIT #\l) ITEM)            "l means longword integer value"
     (BNE.S TRY-W)
     (LEA (REG HI -4) HI)                       "Advance to point to integer"
     (CMP.L HI LO)                              "be paranoid"
     (BGE CX-L-OVERFLOW)
     (MOVE.L (REG HI) TAG)                      "Get fixnum"
     (ASR.L (LIT 3) TAG)                        "Convert to machine number"
     (MOVE.L TAG (REG HI))                      "and put it back"
     (MOVE.L HI (REG LO))                       "Apollo frame points to integer"
     (BRA.S XENOID-SCAN-LOOP)

TRY-W
     (CMPI.B (LIT #\w) ITEM)            "w means word integer value"
     (BNE.S TRY-S)
     (LEA (REG HI -4) HI)                       "Advance to point to integer"
     (CMP.L HI LO)                              "be paranoid"
     (BGE CX-W-OVERFLOW)
     (MOVE.L (REG HI) TAG)                      "Get fixnum"
     (ASR.L (LIT 3) TAG)                        "Convert to machine number"
     (SWAP TAG)                         "Get low 16 into position to store"
     (MOVE.L TAG (REG HI))                      "and put it back"
     (MOVE.L HI (REG LO))                       "Apollo frame points to integer"
     (BRA.S XENOID-SCAN-LOOP)

TRY-S
     (CMPI.B (LIT #\s) ITEM)            "s means point to last string's len"
     (BNE.S TRY-HAT)
     (TST.L PREV)
     (BEQ.S CX-S-OVERFLOW)
     (MOVE.L PREV XP)
     (CLR.L PREV)
     (LEA (REG XP %%STRING-LENGTH-OFFSET) XP)
     (MOVE.L XP (REG LO))
     (BRA XENOID-SCAN-LOOP)

TRY-HAT                                 "^ means indirect through the xenoid"
     (CMPI.B (LIT #\^) ITEM)
     (BNE.S TRY-P)
     (LEA (REG HI -4) HI)
     (MOVE.L (REG HI) XP)
     (MOVE.L (REG XP) (REG LO))
     (BRA XENOID-SCAN-LOOP)

TRY-P
     (CMPI.B (LIT #\p) ITEM)
     (BNE.S CX-BAD-MISC)
     (LEA (REG HI -4) HI)               "Point to object that #\p refers to"
     (CMP.L HI LO)                      "Check for proper format of stack"
     (BGE.S CX-P-OVERFLOW)
     (MOVE.L HI (REG LO))               "Put reference into frame to object"
     (MOVE.L (REG HI) TAG)              "and now look at the object"
     (ANDI.B (LIT 7) TAG)
     (CMPI.B (LIT %%STRING-TAG) TAG)    "Adjust it if it is a string"
     (BNE XENOID-SCAN-LOOP)
     (MOVE.L (REG HI) XP)               "Get the string pointer"
     (MOVE.W (REG XP %%STRING-BASE-OFFSET) TAG) "Get offset to substr"
     (MOVE.L (REG XP %%STRING-POINTER-OFFSET) XP)       "Pointer to text"
     (ADDA.W TAG XP)                    "compute pointer to substring"
     (MOVE.L XP (REG HI))               "Store pointer to substring, reference"
     (BRA XENOID-SCAN-LOOP)             "in frame points here"

CX-BAD-RTN-PLACE
     (MOVE.L '"bad return info, should be one of {ADDR, DATA, VOID, ()}" VAL)
     (BRA.S CX-ERROR)

CX-RTN-TYPE-NOT-EXTEND
     (MOVE.L '"return type info was not an extend" VAL)
     (BRA.S CX-ERROR)

CX-BAD-RTN-TYPE
     (MOVE.L '"return type info was not one of {POINTER, FIXNUM, xenoid}" VAL)
     (BRA.S CX-ERROR)

CX-SCAN-R-BAD-OBJ
     (MOVE.L '"can't convert fixnum, pair, etc." VAL)
     (BRA.S CX-ERROR)

CX-L-OVERFLOW
     (MOVE.L '"ran out of args processing #\l" VAL)
     (BRA.S CX-ERROR)

CX-W-OVERFLOW
     (MOVE.L '"ran out of args processing #\w" VAL)
     (BRA.S CX-ERROR)

CX-S-OVERFLOW
     (MOVE.L '"ran out of args processing #\s" VAL)
     (BRA.S CX-ERROR)

CX-BAD-MISC
     (MOVE.L '"meaningless misc found (wasn't #\l #\p #\s or #\w)" VAL)
     (BRA.S CX-ERROR)

CX-P-OVERFLOW
     (MOVE.L '"ran out of args processing #\p" VAL)
;     (BRA.S CX-ERROR)    

CX-ERROR
     (MOVE.L (SLINK NULL) XP)
     (MOVE.L XP FUN)
     (MOVE.L (SLINK SAVED-HP) HP)
     (CLR.L (SLINK SAVED-HP))
     (PUSH (SLINK *CALL-XENOID-FAULT*))
     (BSR T-SYNCHRONOUS-FAULT)
     (BPT))
