(HERALD (TSYS UNXENO T 96)
        (ENV TSYS)
        (PRE-COOK))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Assembly-level Un*x interface

;;; This file should contain the machine-level un*x-specific operations.

(DEFINE *XENO-OPERATING-SYSTEM* 'UNIX)

;;; Entry into T system at process startup (see unassist.c).

;;; Operating-system-dependent initialization.

(LAP (GLOBL -BEGIN)              "Called from C"
     (ALIGN 1)
-BEGIN
     (\.WORD #x+0FFE)                   "Entry mask: save all regs"

     (MOVAL (ABS THE-SLINK) SLP)	"Initialize slink pointer"

     (INITIAL-VALUE *GC-MARGIN* (* 3000. 8))            "Safety margin"
     (ASHL (LIT 3) (REG AP 4) (@ (STATIC *ARGC*)))      "Get argc and argv"
     (MOVL (REG AP 8) (@ (STATIC *ARGV*)))              "Process them later"
     (MOVL (REG AP 12) R0)
     (MOVL R0 (@ (STATIC *HEAP-1-BEGIN*)))              "So kernel can set limit"
     (MOVL R0 (@ (STATIC *HEAP-BEGIN*)))
     (MOVL (REG AP 16) (@ (STATIC *HEAP-2-BEGIN*)))
     (MOVL (REG AP 20) (@ (STATIC *HEAP-SIZE*)))
     (BISL3 (LIT %%HEAP-TAG) R0 HP)                     "Set heap pointer"
     (ADDL3 (REG AP 12) (REG AP 20) R0)
     (MOVL R0 (@ (STATIC *HEAP-END*)))

     ;; Initialize standard channels
     (MOVL (@ (STATIC *STDIN-CHANNEL*)) R0)
     (MOVL (REG AP 24) (REG R0 %%XENOID-POINTER-OFFSET))
     (MOVL (@ (STATIC *STDOUT-CHANNEL*)) R0)
     (MOVL (REG AP 28) (REG R0 %%XENOID-POINTER-OFFSET))
     (MOVL (@ (STATIC *STDERR-CHANNEL*)) R0)
     (MOVL (REG AP 32) (REG R0 %%XENOID-POINTER-OFFSET))

     ;; Set 4.1 / 4.2 flag
     (MOVL (SLINK TRUE) (@ (STATIC *BSD4.2?*)))
     (BLBS (REG AP 36) BEGIN-1)
       (MOVL (SLINK FALSE) (@ (STATIC *BSD4.2?*)))
BEGIN-1

     (JMP (ABS KERNEL)))                             "Proceed with startup"

;;; Standard channels

(lap (random-data
      (begin-aligned-impure-datum)

      (address xenoid-template)
stdin-channel
      (globl stdin-channel)
      (peso 0)      ; gets filled in later
      (initial-value *stdin-channel* stdin-channel)

      (address xenoid-template)
stdout-channel
      (globl stdout-channel)
      (peso 0)      ; gets filled in later
      (initial-value *stdout-channel* stdout-channel)

      (address xenoid-template)
stderr-channel
      (globl stderr-channel)
      (peso 0)      ; gets filled in later
      (initial-value *stderr-channel* stderr-channel)
      ))

;;; We come here if the FAULT code decided that jumping to <t> code just
;;; wouldn't work.  XP holds the "fault frame", and VAL holds the fault type,
;;; which is actually a VCELL.  We don't have much choice here but to print the
;;; pname of the symbol in the VCELL-ID slot of the vcell, and die.

(LAP LOSING-FAULT (GLOBL LOSING-FAULT)
     (MOVL (REG VAL %%VCELL-ID-OFFSET) YP)      "Get symbol (id) out of vcell"
     (MOVL (REG YP %%SYMBOL-PNAME-OFFSET) YP)   "Get string (pname) from symbol"
     (PUSHL (REG YP %%STRING-POINTER-OFFSET))   "Get byte pointer from string"
     (PUSHAL (REL FAULT-FMT))                   "Push printf format string"
     (MOVL (@ (STATIC *STDERR-CHANNEL*)) VAL)    "Get xenoid"
     (PUSHL (REG VAL %%XENOID-POINTER-OFFSET))  "Get stderr"
     (CALLS (LIT 2) (ABS -FPRINTF))             "Print message"
     (BSBW FLUSHEM)                     "Make sure i/o bufs are flushed"
     (PUSHL (LIT 17))                   "Give ourselves a STOP signal"
     (CLRL (-REG SP))
     (CALLS (LIT 2) (ABS -KILL))
     (BPT)

FAULT-FMT
     (ASCIZ ">>> Unrecoverable T system error: %s
"))

;;; Trivial auxiliary subroutine.  Forces output on standard i/o lib stream
;;; buffers.  Uses no registers.

(LAP FLUSHEM                            "Enter with BSBx or JSB"
     (PUSHL (@ (STATIC *STDOUT-CHANNEL*)))
     (PUSHL (@ (REG SP)))       "Fetch from xenoid"
     (CALLS (LIT 1) -FFLUSH)
     (MOVL (@ (STATIC *STDERR-CHANNEL*)) (REG SP))
     (PUSHL (@ (REG SP)))       "Fetch from xenoid"
     (CALLS (LIT 1) -FFLUSH)
     (TSTL (REG+ SP))
     (RSB))

;;; (ERRNO) - get C library error number... we really ought to be able to
;;; say (FIXNUM->POINTER (MREF (XENOID-POINTER (%XENOID "_errno")) 0)) but
;;; MREF doesn't exist...

(DEFINE-LAP-PROCEDURE ERRNO ((EXPR 0 0 0))
     (EXTERNAL -ERRNO)
     (ASHL (LIT 3) (ABS "_errno") VAL)
     (JMP (@ (SLINK IRETURN))))

;;; (BPT)
;;; Do a BPT instruction.

(DEFINE-LAP-PROCEDURE BPT ((EXPR 0 0 0))
  (BSBW FLUSHEM)                     "?"
  (CLRL (-REG SP))			;SIG_DFL - default action - dump core
  (PUSHL (LIT 5))			;SIGTRAP - Trace/BPT trap
  (CALLS (LIT 2) (ABS "_signal"))
  (BPT)                                 "Mainly for exiting to debugger"
CONT                                    "Do cont:c to continue"
  (MOVL (SLINK TRUE) VAL)
  (JMP (@ (SLINK IRETURN))))

;;; Thing to pass to "signal"

(lap (globl -handle-unix-signal)
     (align 1)
  -handle-unix-signal
     (word 0)
     (pushl (static *unix-signal-fault*))
     (jsb (abs fault))
     (ret))

(define *handle-unix-signal-xenoid* (%xenoid -handle-unix-signal))

;;; List of initial system xenoids - all the ones we'll ever want (ha!).

(DEFINE *FOPEN-XENOID*   (%XENOID "_fopen"))
(DEFINE *FCLOSE-XENOID*  (%XENOID "_fclose"))
(DEFINE *FFLUSH-XENOID*  (%XENOID "_fflush"))
(DEFINE *FGETC-XENOID*   (%XENOID "_fgetc"))
(DEFINE *UNGETC-XENOID*  (%XENOID "_ungetc"))
(DEFINE *FGETS-XENOID*   (%XENOID "_fgets"))
(DEFINE *FPUTC-XENOID*   (%XENOID "_fputc"))
(DEFINE *FPUTS-XENOID*   (%XENOID "_fputs"))
(DEFINE *FPRINTF-XENOID* (%XENOID "_fprintf"))
(DEFINE *SPRINTF-XENOID* (%XENOID "_sprintf"))
(DEFINE *SSCANF-XENOID*  (%XENOID "_sscanf"))
(DEFINE *FREAD-XENOID*   (%XENOID "_fread"))
(DEFINE *FSEEK-XENOID*   (%XENOID "_fseek"))
(DEFINE *FTELL-XENOID*   (%XENOID "_ftell"))
(DEFINE *FWRITE-XENOID*  (%XENOID "_fwrite"))
(DEFINE *CLEARERR-XENOID* (%XENOID "_clearerr"))

;;; System calls

(DEFINE *ACCESS-XENOID*  (%XENOID "_access"))
(DEFINE *ALARM-XENOID*   (%XENOID "_alarm"))
(DEFINE *BRK-XENOID*     (%XENOID "_brk"))
(DEFINE *CHDIR-XENOID*   (%XENOID "_chdir"))
(DEFINE *CHMOD-XENOID*   (%XENOID "_chmod"))
(DEFINE *CHOWN-XENOID*   (%XENOID "_chown"))
(DEFINE *CLOSE-XENOID*   (%XENOID "_close"))
(DEFINE *CREAT-XENOID*   (%XENOID "_creat"))
(DEFINE *DUP2-XENOID*    (%XENOID "_dup2"))
(DEFINE *EXECVE-XENOID*  (%XENOID "_execve"))
(DEFINE *EXIT-XENOID*    (%XENOID "_exit"))
(DEFINE *FDATE-XENOID*    (%XENOID "_fork"))
(DEFINE *FORK-XENOID*    (%XENOID "_fork"))
(DEFINE *FSTAT-XENOID*   (%XENOID "_fstat"))
(DEFINE *FTIME-XENOID*   (%XENOID "_ftime"))
(DEFINE *GETLOGIN-XENOID* (%XENOID "_getlogin"))
(DEFINE *GETPID-XENOID*  (%XENOID "_getpid"))
(DEFINE *GETPGRP-XENOID* (%XENOID "_getpgrp"))
(DEFINE *GETITIMER-XENOID*  (%XENOID "_getitimer"))
(DEFINE *GETWD-XENOID*   (%XENOID "_getwd"))
(DEFINE *GTTY-XENOID*    (%XENOID "_gtty"))
(DEFINE *IOCTL-XENOID*   (%XENOID "_ioctl"))
(DEFINE *KILL-XENOID*    (%XENOID "_kill"))
(DEFINE *KILLPG-XENOID*  (%XENOID "_killpg"))
(DEFINE *LINK-XENOID*    (%XENOID "_link"))
(DEFINE *LSEEK-XENOID*   (%XENOID "_lseek"))
(DEFINE *NICE-XENOID*    (%XENOID "_nice"))
(DEFINE *OPEN-XENOID*    (%XENOID "_open"))
(DEFINE *PAUSE-XENOID*   (%XENOID "_pause"))
(DEFINE *PIPE-XENOID*    (%XENOID "_pipe"))
(DEFINE *PROFIL-XENOID*  (%XENOID "_profil"))
(DEFINE *PTRACE-XENOID*  (%XENOID "_ptrace"))
(DEFINE *READ-XENOID*    (%XENOID "_read"))
(DEFINE *RENAME-XENOID*  (%XENOID "_rename"))
(DEFINE *SBRK-XENOID*    (%XENOID "_sbrk"))
(DEFINE *SETITIMER-XENOID*  (%XENOID "_setitimer"))
(DEFINE *SETPGRP-XENOID* (%XENOID "_setpgrp"))
(DEFINE *SIGNAL-XENOID*  (%XENOID "_signal"))
(DEFINE *SLEEP-XENOID*   (%XENOID "_sleep"))
(DEFINE *STAT-XENOID*    (%XENOID "_stat"))
(DEFINE *STTY-XENOID*    (%XENOID "_stty"))
(DEFINE *SYSCALL-XENOID* (%XENOID "_syscall"))
(DEFINE *TELL-XENOID*    (%XENOID "_tell"))
(DEFINE *TIME-XENOID*    (%XENOID "_time"))
(DEFINE *TRUNCATE-XENOID* (%XENOID "_truncate"))
(DEFINE *UMASK-XENOID*   (%XENOID "_umask"))
(DEFINE *UNLINK-XENOID*  (%XENOID "_unlink"))
(DEFINE *UTIME-XENOID*   (%XENOID "_utime"))
(DEFINE *VADVISE-XENOID* (%XENOID "_vadvise"))
(DEFINE *VFORK-XENOID*   (%XENOID "_vfork"))
(DEFINE *VLIMIT-XENOID*  (%XENOID "_vlimit"))
(DEFINE *VTIMES-XENOID*  (%XENOID "_vtimes"))
(DEFINE *WAIT3-XENOID*   (%XENOID "_wait3"))
(DEFINE *WRITE-XENOID*   (%XENOID "_write"))

;;; C library randomness
(DEFINE *ISATTY-XENOID*  (%XENOID "_isatty"))
(DEFINE *ATOF-XENOID*    (%XENOID "_atof"))
(DEFINE *CTIME-XENOID*   (%XENOID "_ctime"))
(DEFINE *CALLOC-XENOID*  (%XENOID "_calloc"))
(DEFINE *GETENV-XENOID*  (%XENOID "_getenv"))
(DEFINE *ENVIRON-XENOID*     (%XENOID "_environ"))
(DEFINE *FREE-XENOID*        (%XENOID "_free"))
(DEFINE *LOCALTIME-XENOID*   (%XENOID "_localtime"))
(DEFINE *MALLOC-XENOID*      (%XENOID "_malloc"))
(DEFINE *NLIST-XENOID*       (%XENOID "_nlist"))
(DEFINE *RAND-XENOID*        (%XENOID "_rand"))
(DEFINE *REALLOC-XENOID*     (%XENOID "_realloc"))
(DEFINE *SRAND-XENOID*       (%XENOID "_srand"))
(DEFINE *SYSTEM-XENOID*      (%XENOID "_system"))
(DEFINE *SYS-ERRLIST-XENOID* (%XENOID "_sys_errlist"))  ;?
(DEFINE *VALLOC-XENOID*      (%XENOID "_valloc"))

;;; Math
(DEFINE *SIN-XENOID*     (%XENOID "_sin"))
(DEFINE *COS-XENOID*     (%XENOID "_cos"))
(DEFINE *TAN-XENOID*     (%XENOID "_tan"))
(DEFINE *ASIN-XENOID*    (%XENOID "_asin"))
(DEFINE *ACOS-XENOID*    (%XENOID "_acos"))
(DEFINE *ATAN-XENOID*    (%XENOID "_atan"))
(DEFINE *ATAN2-XENOID*   (%XENOID "_atan2"))
(DEFINE *EXP-XENOID*     (%XENOID "_exp"))
(DEFINE *LOG-XENOID*     (%XENOID "_log"))
(DEFINE *POW-XENOID*     (%XENOID "_pow"))
(DEFINE *SQRT-XENOID*    (%XENOID "_sqrt"))

;;; Terminal capabilities
(DEFINE *TGETENT-XENOID*  (%XENOID "_tgetent"))
(DEFINE *TGETNUM-XENOID*  (%XENOID "_tgetnum"))
(DEFINE *TGETFLAG-XENOID* (%XENOID "_tgetflag"))
(DEFINE *TGETSTR-XENOID*  (%XENOID "_tgetstr"))
(DEFINE *TGOTO-XENOID*    (%XENOID "_tgoto"))
(DEFINE *TPUTS-XENOID*    (%XENOID "_tputs"))

;;; Assist module stuff 
     
;; Support for channels.

(define *feof-xenoid*           (%xenoid "_feof_proc"))
(define *ferror-xenoid*         (%xenoid "_ferror_proc"))
(define *fileno-xenoid*         (%xenoid "_fileno_proc"))
(define *clear-ttyeof-xenoid*   (%xenoid "_clear_ttyeof"))

;; For loading C code dynamically.

(define *nlist-one-xenoid*      (%xenoid "_nlist_one"))  

;; To reenable signals in 4.2

(define *sigunblock-xenoid*     (%xenoid "_sigunblock"))
                        
;; Mostly for LIME the editor.

(DEFINE *SHRINK-XENOID*         (%XENOID "_shrink"))
(DEFINE *SAVE-TTY-XENOID*       (%XENOID "_save_tty"))
(DEFINE *RESTORE-TTY-XENOID*    (%XENOID "_restore_tty"))
(DEFINE *U-MODE-XENOID*         (%XENOID "_tty_u_mode"))
(DEFINE *INPUT-WAITING-XENOID*  (%XENOID "_input_waiting"))
(DEFINE *USLEEP-XENOID*         (%XENOID "_usleep"))
(DEFINE *TTY-RAW-XENOID*        (%XENOID "_tty_raw"))
(DEFINE *TTY-NORMAL-XENOID*     (%XENOID "_tty_normal"))
(DEFINE *FILE-LENGTH-XENOID*    (%XENOID "_file_length"))
(DEFINE *EXPAND-PATH-XENOID*    (%XENOID "_expand_path"))

