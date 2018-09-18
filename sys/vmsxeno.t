(HERALD VMSXENO                         ; -*- Mode:SCHLAP; System:T -*-
        (PRE-COOK))

;;; Copyright (c) 1983 Yale University

;;;; Assembly-level VMS interface

;;; This file should contain the machine-level VMS-specific operations.

(DEFINE *XENO-OPERATING-SYSTEM* 'VMS)

;;; Entry into T system at process startup

;;; Operating-system-dependent initialization:
;;; - Set up SLP
;;; - Save FP for fault stuff and CALL-XENOID.
;;; - Set of the Heap variables
;;; - Set up HP.
;;; - Then jump to KERNEL


(LAP (GLOBL TSTART)
     (ALIGN 1)
TSTART
     (\.WORD #x+0)                   "Entry mask: dont bother saving regs"

     (INITIAL-VALUE *GC-MARGIN* (* 2000. 8))                 "Safety margin"

     ;; These values come from Initialize_T in ASSIST.
     (MOVL (ABS AREA_1_END)         (@ (STATIC *AREA1-END*)))
     (MOVL (ABS AREA_1_BEGIN)       (@ (STATIC *AREA1-BEGIN*)))
     (MOVL (ABS AREA_0_END)         (@ (STATIC *AREA0-END*)))
     (MOVL (ABS AREA_0_BEGIN)       (@ (STATIC *AREA0-BEGIN*)))
     (MOVL (ABS initial_heap_begin) (@ (STATIC *HEAP-BEGIN*)))
     (MOVL (ABS initial_heap_end)   (@ (STATIC *HEAP-END*)))

     ;; Set up TTY input and output channels
     ;++ Are these really necessary?
     (MOVL (ABS "sys_input_fab")      (ABS (+ TTYIN-CHANNEL 8)))
     (MOVL (ABS "sys_output_fab")     (ABS (+ TTYOUT-CHANNEL 8)))

     (PUSHL (LIT -1))                       "Mark top of stack"
     (MOVAL (ABS THE-SLINK) SLP)
     (MOVL FP (SLINK SAVED-FP))
     (BISL3 (LIT %%HEAP-TAG) (ABS initial_HP) HP) "Set heap pointer"
     (JMP (ABS KERNEL)))


;;; We come here if the FAULT code decided that jumping to <t> code just
;;; wouldn't work.  XP holds the "fault frame", and VAL holds the fault type,
;;; which is actually a VCELL.  We don't have much choice here but to print the
;;; pname of the symbol in the VCELL-ID slot of the vcell, and die.

(LAP LOSING-FAULT (GLOBL LOSING-FAULT)
     (MOVL (SLINK SAVED-FP) FP)
     (MOVL (REG VAL %%VCELL-ID-OFFSET) YP)      "Get symbol (id) out of vcell"
     (PUSHL (REG YP %%SYMBOL-PNAME-OFFSET))     "Get string (pname) from symbol"
     (CALLS (LIT 1) (ABS "DBG_BREAK"))          "Call assist routine which can"
                                                " never return"
     (MOVL (SLINK TRUE) VAL)                    "... but just in case"
     (MOVL (REG+ SP) FP)
     (JMP (@ (SLINK IRETURN))))


;;; (BPT)
;;; Do a BPT instruction.

(DEFINE-LAP-PROCEDURE BPT ((EXPR 0 0 0))
     (PUSHL FP)
     (MOVL (SLINK SAVED-FP) FP)         "Restore FP"
     (CLRL (-REG SP))                   "Return Exception Handler in R0"
     (PUSHL (LIT 3))                    "User mode"
     (CLRQ (-REG SP))                   "Vector slot and Handler both 0"
     (CALLS (LIT 4) (ABS "SYS$SETEXV")) "Change exception vector"
     (BPT)

CONT (GLOBL CONT)                       "Do 'DBG>g cont' in debugger"
     (CLRL (-REG SP))                   "Return Exception Handler in R0"
     (PUSHL (LIT 3))                    "User mode"
     (PUSHL EXCEPTION-HANDLER)          "Handler"
     (CLRL (-REG SP))                   "Vector Slot 0"
     (CALLS (LIT 4) (ABS "SYS$SETEXV")) "Change exception vector"
     (MOVL (SLINK TRUE) VAL)            "Setup Return value of T"
     (MOVL (REG+ SP) FP)                "Store the FP"
     (JMP (@ (SLINK IRETURN))))

;;; Exception handler

(LAP (ALIGN 1)
EXCEPTION-HANDLER
     (WORD 0)
     (PUSHL (STATIC *EXCEPTION-FAULT*))
     (JSB (ABS FAULT))
     (MOVL (LIT 1) R0)              "Move SS$_CONTINUE to R0"
     (RET))

(DEFINE *EXCEPTION-HANDLER* (%XENOID EXCEPTION-HANDLER))

;;; Interrupt handler

(LAP (ALIGN 1)
INTERRUPT-HANDLER
     (WORD 0)
     (MOVL (REG AP 16.) (STATIC *SAVED-PC*))
     (MOVAL TAKE-INTERRUPT (REG AP 16.))
     (RET)
TAKE-INTERRUPT                  "Interrupts should be disabled"
    ;Problem with PSL?
     (MOVL (STATIC *SAVED-PC*) (-REG SP))
     (PUSHL (STATIC *INTERRUPT-FAULT*))
     (JSB (ABS FAULT))
     (JMP (@ (REG+ SP))))

(DEFINE *INTERRUPT-HANDLER* (%XENOID INTERRUPT-HANDLER))

;;; STOP (ctrl-Y) handler

(LAP (ALIGN 1)
STOP-HANDLER
     (WORD 0)
     (MOVL (REG AP 16.) (STATIC *SAVED-PC*))
     (MOVAL TAKE-STOP   (REG AP 16.))
     (RET)
TAKE-STOP                       "Interrupts should be disabled"
    ;Problem with PSL?
     (MOVL (STATIC *SAVED-PC*) (-REG SP))
     (PUSHL (STATIC *STOP-FAULT*))
     (JSB (ABS FAULT))
     (JMP (@ (REG+ SP))))

(DEFINE *STOP-HANDLER* (%XENOID STOP-HANDLER))

;;; Channels

(lap (procedure p$handle-channel-stub
                t$handle-channel-stub c$handle-channel-stub
                ((expr 2 0 0)))
     (movl (@ (static handle-channel)) fun)
     (jmp (@ (slink icall))))

(define *channel-template*
  (lap-template channel-template c$channel-template
                ((size 2 2)         ;synchronize with make-channel
                 (handler p$handle-channel-stub))
    (globl channel-template)
    (jmp (@ (slink inapplicable)))))


;;; Initial I/O channels
;;; These cannot be allocated in the heap, because then ZFORMAT
;;; would lose during GC. See global declarations in VSTART.B32

(lap (random-data
      (globl ttyin_channel)
      (globl ttyin_buffer)
      (globl ttyin_pointer)
      (globl ttyout_channel)
      (globl ttyout_buffer)
      (globl ttyout_pointer)

      (align 3)
      (address channel-template)
ttyin-channel
      (address ttyin-buffer)      ; buffer
      (address ttyin-pointer)     ; pointer into buffer
      (long #x000000000)          ; pointer to FAB block
      (long #x000000000)          ; Mode
      (initial-value *ttyin-channel* ttyin-channel)

      (align 3)
      (word 130)
ttyin-buffer
      (address "tty_in_buf")
      (word 0)

      (align 3)
      (word 0)
ttyin-pointer
      (address "tty_in_buf")
      (word 0)

      (align 3)
      (address channel-template)
ttyout-channel
      (address ttyout-buffer)     ; buffer
      (address ttyout-pointer)    ; pointer into buffer
      (long #x000000000)          ; pointer to FAB block
      (long #x000000008)          ; Mode
      (initial-value *ttyout-channel* ttyout-channel)

      (align 3)
      (word 130)
ttyout-buffer
      (address "tty_out_buf")
      (word 0)

      (align 3)
      (word 0)
ttyout-pointer
      (address "tty_out_buf")
      (word 0)))


;;; List of initial system xenoids
;*** ===============================================================

;;; Access to assist routines ...
(DEFINE *ENABLE-CTRLC-XENOID*       (%XENOID    "enable_ctrlc"))
(DEFINE *DISABLE-CTRLC-XENOID*      (%XENOID    "disable_ctrlc"))
(DEFINE *ENABLE-CTRLY-XENOID*       (%XENOID    "enable_ctrly"))
(DEFINE *DISABLE-CTRLY-XENOID*      (%XENOID    "disable_ctrly"))
(DEFINE *EXCPT-MSG-XENOID*          (%XENOID    "exception_msg"))
(DEFINE *STOP-XENOID*               (%XENOID    "stop_process"))
(DEFINE *EXIT-XENOID*               (%XENOID    "sys$exit"))
;(DEFINE *ALLOC-AREA-XENOID*        (%XENOID    "alloc_area"))
(DEFINE *RESET-AREA-XENOID*         (%XENOID    "reset_area"))
(DEFINE *GUARD-AREA-XENOID*         (%XENOID    "guard_area"))
;(DEFINE *UNGUARD-AREA-XENOID*      (%XENOID    "unguard_area"))
(DEFINE *SAVE-PROCESS-XENOID*       (%XENOID    "save_process"))
(DEFINE *RESTORE-PROCESS-XENOID*    (%XENOID    "restore_process"))
                                                 
;;; Command line and current working directory
(DEFINE *CMD-BUF-XENOID*            (%XENOID    "cmd_buf"))
(DEFINE *WDIR-BUF-XENOID*           (%XENOID    "wdir_buf"))
(DEFINE *USERNAME-BUF-XENOID*       (%XENOID    "user_name_buf"))
(DEFINE *TTYIN-BUF-XENOID*          (%XENOID    "tty_input_buf"))
(DEFINE *TTYOUT-BUF-XENOID*         (%XENOID    "tty_output_buf"))
(DEFINE *BASE-POINT-STATS-XENOID*   (%XENOID    "initial_cpu"))
(DEFINE *BASE-CLOCK-XENOID*         (%XENOID    "initial_clock"))
                                                 
;;; VMS I/O Xeniods
(DEFINE *CHANNEL-OPEN-XENOID*       (%XENOID    "channel_open"))
(DEFINE *CHANNEL-CLOSE-XENOID*      (%XENOID    "channel_close"))
(DEFINE *CHANNEL-FORCE-XENOID*      (%XENOID    "channel_force"))
(DEFINE *CHANNEL-GET-XENOID*        (%XENOID    "channel_get"))
(DEFINE *CHANNEL-LOAD-XENOID*       (%XENOID    "channel_load"))
(DEFINE *CHANNEL-PUT-XENOID*        (%XENOID    "channel_put"))
(DEFINE *CHANNEL-ISTTY-XENOID*      (%XENOID    "channel_istty"))
(DEFINE *CHANNEL-WRITE-DATE-XENOID* (%XENOID    "channel_write_date"))
(DEFINE *VMS-ERROR-XENOID*          (%XENOID    "vms_error"))
                                                 
;;; Xenoids for U editor
(DEFINE *TTY-SAVE-XENOID*           (%XENOID    "tty_save"))
(DEFINE *TTY-RESTORE-XENOID*        (%XENOID    "tty_restore"))
(DEFINE *TTY-UMODE-XENOID*          (%XENOID    "tty_umode"))
(DEFINE *TTY-TYPE-AHEAD-XENOID*     (%XENOID    "tty_type_ahead"))
(DEFINE *TTY-GETCHAR-XENOID*        (%XENOID    "tty_getchar"))
(DEFINE *TTY-GETSTRING-XENOID*      (%XENOID    "tty_getstring"))
(DEFINE *TTY-PUTCHAR-XENOID*        (%XENOID    "tty_putchar"))
(DEFINE *TTY-PUTSTRING-XENOID*      (%XENOID    "tty_putstring"))

;;; Date, Time, and Metering Xenoids
(DEFINE *GET-TIME-XENOID*           (%XENOID    "get_time"))
(DEFINE *TIME->STRING-XENOID*       (%XENOID    "time_string"))
(DEFINE *STRING->TIME-XENOID*       (%XENOID    "string_time"))
(DEFINE *$GETTIM-XENOID*            (%XENOID    "sys$gettim"))
(DEFINE *PROCESS-STATS-XENOID*      (%XENOID    "get_stats"))

;;; Conversion Xenoids
(DEFINE *FLONUM->STRING-XENOID*     (%XENOID    "flonum_string"))
(DEFINE *STRING->FLONUM-XENOID*     (%XENOID    "string_flonum"))

;;; File System Xenoids
(DEFINE *FILE-PARSE-XENOID*         (%XENOID    "file_parse"))
(DEFINE *FILE-EXISTS-XENOID*        (%XENOID    "file_exists"))
(DEFINE *FILE-NAME-XENOID*          (%XENOID    "file_name"))
(DEFINE *FILE-DIR-XENOID*           (%XENOID    "file_dir_name"))
(DEFINE *FILE-LEAF-XENOID*          (%XENOID    "file_leaf_name"))
(DEFINE *FILE-PROBE-XENOID*         (%XENOID    "file_probe"))
(DEFINE *FILE-DELETE-XENOID*        (%XENOID    "file_delete"))
(DEFINE *FILE-RENAME-XENOID*        (%XENOID    "file_rename"))
(DEFINE *FILE-WRITE-DATE-XENOID*    (%XENOID    "file_write_date"))
(DEFINE *FILE-CREATION-DATE-XENOID* (%XENOID    "file_creation_date"))
(DEFINE *SET-WDIR-XENOID*           (%XENOID    "set_wdir"))
(DEFINE *SLEEP-XENOID*              (%XENOID    "sleep"))
                                                 
;;; VMS System Service Xenoids
(DEFINE *$SETEXV-XENOID*            (%XENOID    "sys$setexv"))


;;; Math from VMS runtime library
(DEFINE *MTH$DACOS*                 (%XENOID    "mth$dacos"))
(DEFINE *MTH$DASIN*                 (%XENOID    "mth$dasin"))
(DEFINE *MTH$DATAN*                 (%XENOID    "mth$datan"))
                                                 
(DEFINE *MTH$DATANH*                (%XENOID    "mth$datanh"))
(DEFINE *MTH$DCOSH*                 (%XENOID    "mth$dcosh"))
(DEFINE *MTH$DSINH*                 (%XENOID    "mth$dsinh"))
(DEFINE *MTH$DTANH*                 (%XENOID    "mth$dtanh"))
                                                 
(DEFINE *MTH$DCOS*                  (%XENOID    "mth$dcos"))
(DEFINE *MTH$DSIN*                  (%XENOID    "mth$dsin"))
;(DEFINE *MTH$DSINCOSD*              (%XENOID    "mth$dsincosd"))
(DEFINE *MTH$DTAN*                  (%XENOID    "mth$dtan"))
(DEFINE *MTH$DEXP*                  (%XENOID    "mth$dexp"))
(DEFINE *MTH$DLOG*                  (%XENOID    "mth$dlog"))
(DEFINE *MTH$DLOG10*                (%XENOID    "mth$dlog10"))
(DEFINE *MTH$DLOG2*                 (%XENOID    "mth$dlog2"))
(DEFINE *MTH$RANDOM*                (%XENOID    "mth$random"))
(DEFINE *MTH$DSQRT*                 (%XENOID    "mth$dsqrt"))
                                                 
;; Local Modes:
;; Lisp LAP-TEMPLATE Indent:3
;; Lisp LAP-PROCEDURE Indent:4
;; Mode:SCHLAP
;; END:
