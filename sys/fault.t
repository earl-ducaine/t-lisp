(HERALD (TSYS FAULT T 21)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Common fault code

;;; VAX / 68000 common fault handling code.  Also, REASONABLE? predicate.

;;; Utilities for dealing with fault frames.

(DEFINE-INTEGRABLE (FAULT-FRAME? OBJ)
  (AND (EXTEND? OBJ)
       (EQ? (EXTEND-TEMPLATE OBJ) *FAULT-FRAME-TEMPLATE*)))

(DEFINE (MAP-POINTER-REGS! PROC FRAME)
  (DO ((REGNUM %%VAL (FX+ REGNUM 1)))
      ((FX> REGNUM %%FUN) T)
    (SET (FAULT-REG FRAME REGNUM)
         (PROC (FAULT-REG FRAME REGNUM)))))

(DEFINE-INTEGRABLE (FAULT-IN-LISP? FRAME)
  (FX= (FAULT-REG FRAME %%SHP) 0))

;;; Given a fault frame provided by ICALL, get the list of arguments
;;; to which the procedure is being applied.

(DEFINE (FAULT-FRAME-ARGLIST FRAME)
  (DO ((I (FX- (FAULT-FRAME-NARGS FRAME) 1) (FX- I 1))
       (L '() (CONS (FAULT-FRAME-ARG FRAME I) L)))
      ((FX< I 0) L)))

(DEFINE (FAULT-FRAME-ARG FRAME N)       ; Zero-based
  (XREF (CALL-FRAME FRAME) (FX- -2 N))) ; XREF not FRAME-REF, here

(DEFINE-INTEGRABLE (CALL-FRAME FRAME)
  (MAKE-POINTER (FAULT-REG FRAME %%AP) %%EXTEND-TAG))

(DEFINE (FAULT-FRAME-NARGS FRAME)
  (FX- (POINTER-ASHL (FAULT-REG FRAME %%AP) 1)
       (POINTER-ASHL (FAULT-REG FRAME %%SP) 1)))

;;; ...

(DEFINE (FAULT-BREAKPOINT MESSAGE FRAME)        ; Special interface BREAKPOINT
  (IGNORE FRAME)
  (SET *RE-ENTER-ON-FAULT?* T)
  (COND (*Z?*
         (ZFORMAT ZERROR-OUTPUT "~A~%" MESSAGE)
         (ZBREAKPOINT))
        (ELSE
         (ERROR-BREAKPOINT MESSAGE))))

(DEFINE (OTHER-RANDOM-FAULT S)
  (LAMBDA (FRAME TYPE)
    (SET *RE-ENTER-ON-FAULT?* T)
    (ERROR S)
    (NOT-PROCEEDABLE)))

(DEFINE (NOT-PROCEEDABLE)
  (ERROR "the error you encountered is not proceedable")
  (NOT-PROCEEDABLE))

;;; This list of fault types must be kept in sync with the kernel(s).

(DEFINE *STACK-BASE-FAULT*
  (OTHER-RANDOM-FAULT "attempt to throw off bottom of stack"))

(DEFINE *ICALL-AP-ALIGNMENT-FAULT*
  (OTHER-RANDOM-FAULT "misaligned argument pointer"))

(DEFINE *ICALL-RETURN-NOT-TEMPLATE-FAULT*
  (OTHER-RANDOM-FAULT "illegal return address in call"))

(DEFINE *IRETURN-BAD-SP-FAULT*
  (OTHER-RANDOM-FAULT "stack pointer misaligned on procedure return"))

(DEFINE *IRETURN-BAD-SP-TEMPLATE-FAULT*
  (OTHER-RANDOM-FAULT "illegal return address on return"))

(DEFINE *COMPILER-LOSSAGE-FAULT*
  (OTHER-RANDOM-FAULT "attempt to execute code with compiler error"))

(DEFINE (*BB-FAULT* FRAME TYPE)
  (IGNORE TYPE)
  (FAULT-BREAKPOINT "** Debugger breakpoint" FRAME))  

(DEFINE (*IAPPLY-IMPROPER-LIST-FAULT* FRAME TYPE)
  (IGNORE TYPE)
  (SET *RE-ENTER-ON-FAULT?* T)
  (LET ((PROC (FAULT-REG FRAME %%FUN)))
    (FRAME-THROW (CALL-FRAME FRAME)
                 (APPLY PROC
                        (ERROR "argument is not a proper list~%  ~S"
                               `(APPLY ,PROC
                                       ,(APPEND (FAULT-FRAME-ARGLIST FRAME)
                                                (FAULT-REG FRAME %%VAL))))))))

(DEFINE (*ICALL-WRONG-NUMBER-ARGS-FAULT* FRAME TYPE)
  (IGNORE TYPE)
  (SET *RE-ENTER-ON-FAULT?* T)
  (LET ((PROC (FAULT-REG FRAME %%FUN)))
    (FRAME-THROW (CALL-FRAME FRAME)
                 (ERROR "wrong number of arguments to procedure~%  ~S"
                        (CONS (OR (IDENTIFICATION PROC) PROC)
                              (FAULT-FRAME-ARGLIST FRAME))))))

(DEFINE (ILLEGAL-CALL-FAULT FRAME TYPE)
  (IGNORE TYPE)
  (SET *RE-ENTER-ON-FAULT?* T)
  (SET (FAULT-REG FRAME %%FUN)
       (ILLEGAL-CALL (FAULT-REG FRAME %%FUN)
                              (FAULT-FRAME-ARGLIST FRAME))))

(DEFINE *ICALL-TEMPLATE-NOT-TEMPLATE-FAULT* ILLEGAL-CALL-FAULT)
(DEFINE *ICALL-NOT-EXTEND-FAULT*            ILLEGAL-CALL-FAULT)
(DEFINE *ICALL-NOT-PROCEDURE-FAULT*         ILLEGAL-CALL-FAULT)
(DEFINE *INAPPLICABLE-FAULT*                ILLEGAL-CALL-FAULT)

;
;;; Clever interrupt handler code.
;;; This is for returns from asynchronous interrupts.

;;; CLEANUP-INTERRUPT-FRAME should be the last thing called from the
;;; interrupt handling routine. There must be NO CONSING from here on
;;; out.  Also, this code should run with interrupts locked out.  The
;;; FAULT-RET code should re-enable interrupts.  There are screws
;;; asociated with CALL-XENOID snarfing & clearing (SLINK SHP)
;;; atomically.  Currently a bit locks out interrupts for 2 instructions
;;; then, on the 68000 at least...

(DEFINE (CLEANUP-INTERRUPT-FRAME FRAME)
  (COND ((FAULT-IN-LISP? FRAME)
         (FIXUP-INTERRUPTED-CONSING FRAME)
         (SET (FAULT-REG FRAME %%HP) (HEAP-POINTER)))   
        ;; Interrupted out of foreign code.
        (ELSE
         (XSET (THE-SLINK) %%SAVED-HP-INDEX (HEAP-POINTER)))))

(LSET *WE-ARE-REALLY-SMART* NIL)

;;; Grovel the registers looking to see if anyone was doing
;;; consing at the time of a fault.  If so clobber the registers involved
;;; with the new HP (with the type tag appropriately adjusted).
;;; Assumes that the fault was out of T code, not out of foreign code -
;;; caller should enforce this.

(DEFINE (FIXUP-INTERRUPTED-CONSING FRAME)
  (MAP-POINTER-REGS!
      (LAMBDA (FROB)
        (COND ((AND (NOT (FIXNUM? FROB))
                    (FX= (POINTER-ADDRESS FROB)
                         (POINTER-ADDRESS (FAULT-REG FRAME %%HP))))
               (LET ((HP (POINTER-ADDRESS (HEAP-POINTER))))
                 (SET *WE-ARE-REALLY-SMART* HP)
                 (MAKE-POINTER HP (POINTER-TAG FROB))))
              (ELSE FROB)))
      FRAME))

;;; Methods for fault frames:

(DEFINE HANDLE-FAULT-FRAME
  (%HANDLER FRAME
	    ((FRAME-PREVIOUS SELF) (FAULT-FRAME-PREVIOUS FRAME))
            ((CRAWL-EXHIBIT SELF)  (CRAWL-EXHIBIT-FAULT-FRAME FRAME))
            ((GET-ENVIRONMENT SELF) NIL)
            ((GET-LOADED-FILE SELF) NIL)
            ((PRINT-TYPE-STRING SELF) "Fault-frame")))

;;; Heuristic.  But I'll put money on it.

(DEFINE (FAULT-FRAME-PREVIOUS FRAME)
  (COND ((FAULT-IN-LISP? FRAME)
         ;; If AP is less than (i.e. "beyond") SP, start from SP
         (LET ((F (MAKE-POINTER (IF (POINTER-LESS? (FAULT-REG FRAME %%AP)
                                                   (FAULT-REG FRAME %%SP))
                                    (POINTER-ADDRESS (FAULT-REG FRAME %%SP))
                                  (FAULT-REG FRAME %%AP))
                                %%EXTEND-TAG)))
           ;; Scan the depths of the stack until we come across something that
           ;;  could be reasonably construed as a frame
           (DO ((F F (POINTER-ADD F 1)))
               ((AND (REASONABLE? F) (FRAME? F)) F))))
        (ELSE
         (MAKE-POINTER (FAULT-REG FRAME %%SAP) %%EXTEND-TAG))))

(DEFINE (CRAWL-EXHIBIT-FAULT-FRAME FRAME)
  (LET ((OUT (TERMINAL-OUTPUT)))
    (COND ((FAULT-IN-LISP? FRAME)
           (LET ((PFOO
                  (LAMBDA (STRING VAL)
                    (FORMAT OUT " ~A = " STRING)
		    (COND ((REASONABLE? VAL)
			   (PRINT-ONE-LINE VAL OUT)
			   (COND ((GET-PROC-NAME VAL)
				  => (LAMBDA (NAME)
				       (FORMAT OUT "~_(~S)" NAME)))))
			  (ELSE
			   (FORMAT OUT "#{Unreasonable~_#x~X}"
				   (POINTER->INTEGER VAL))))
		    (NEWLINE OUT))))
             (PFOO "Procedure reg"    (FAULT-REG FRAME %%FUN))
             (PFOO "Value reg"        (FAULT-REG FRAME %%VAL))
             (PFOO "Temp reg"         (FAULT-REG FRAME %%XP))
             (PFOO "Template pointer" (FAULT-REG FRAME %%TP))
             (PFOO "Jump-from"        (FAULT-REG FRAME %%JF))))
          (ELSE
           (FORMAT OUT "Asynchronous fault from foreign code.~%")))))


;;; "Reasonableness" predicate
;;; This may become hairier.

(DEFINE (REASONABLE? OBJ)
  (SELECT (POINTER-TAG OBJ)
    ((%%FIXNUM-TAG) T)
    ((%%MISC-TAG)
     (OR (NULL? OBJ)
         (CHAR? OBJ)
         (AND (NONVALUE? OBJ)
              (REASONABLE? (NONVALUE->VALUE OBJ)))))
    ((%%PAIR-TAG %%FLONUM-TAG)
     (POINTS-TO-REASONABLE-MEMORY? OBJ))
    ((%%STRING-TAG)
     (AND (POINTS-TO-REASONABLE-MEMORY? OBJ)
          (POINTS-TO-REASONABLE-MEMORY? (STRING-POINTER OBJ))
          (FX>= (STRING-LENGTH OBJ) 0)))
    ((%%EXTEND-TAG)
     (AND (POINTS-TO-REASONABLE-MEMORY? OBJ)
          (TEMPLATE? (EXTEND-TEMPLATE OBJ))
          (REASONABLE? (EXTEND-TEMPLATE OBJ))))
    ((%%TEMPLATE-TAG)
     (AND (POINTS-TO-REASONABLE-MEMORY? OBJ)
          (FX= (TEMPLATE-JUMP-OPCODE OBJ)
               (TEMPLATE-JUMP-OPCODE (EXTEND-TEMPLATE REASONABLE?)))))
    (ELSE NIL)))

(DEFINE (POINTS-TO-REASONABLE-MEMORY? OBJ)
  (COND ((AND (POINTER-LESS? *PURE-INITIAL-MEM-BEGIN* OBJ)
              (POINTER-LESS? OBJ *PURE-INITIAL-MEM-END*))
         'PURE-INITIAL)
        ((AND (POINTER-LESS? *IMPURE-INITIAL-MEM-BEGIN* OBJ)
              (POINTER-LESS? OBJ *IMPURE-INITIAL-MEM-END*))
         'IMPURE-INITIAL)
        ((AND (POINTER-LESS? *HEAP-BEGIN* OBJ)
              (POINTER-LESS? OBJ (HEAP-POINTER)))
         'HEAP)
        ((AND (POINTER-LESS? (STACK-POINTER) OBJ)
              (POINTER-NOT-GREATER? OBJ *STACK-END*))
         'STACK)
        (ELSE NIL)))

;;; Assume system smaller than 1 gigabyte.
;;; Assume that the POINTER-FOO? comparisons are unsigned (!).

(ITERATE LOOP ((I 0)
               (  PURE-MIN -1)
               (  PURE-MAX  0)
               (IMPURE-MIN -1)
               (IMPURE-MAX  0))
  (COND ((FX< I (VECTOR-LENGTH *THE-INITIAL-UNITS*))
         (LET* ((U (VREF *THE-INITIAL-UNITS* I))
                (C (UNIT-CODE U))
                (UE (%XLOC U (CODE-UNIT-SIZE C)))
                (CE (FX+ C (CODE-SIZE C))))
           (LOOP (FX+ I 1)
                 (IF (POINTER-LESS?    C    PURE-MIN) C    PURE-MIN)
                 (IF (POINTER-GREATER? CE   PURE-MAX) CE   PURE-MAX)
                 (IF (POINTER-LESS?    U  IMPURE-MIN) U  IMPURE-MIN)
                 (IF (POINTER-GREATER? UE IMPURE-MAX) UE IMPURE-MAX))))
        (ELSE

         (DEFINE   *PURE-INITIAL-MEM-BEGIN*   PURE-MIN)
         (DEFINE   *PURE-INITIAL-MEM-END*     PURE-MAX)
         (DEFINE *IMPURE-INITIAL-MEM-BEGIN* (POINTER-ADDRESS IMPURE-MIN))
         (DEFINE *IMPURE-INITIAL-MEM-END*   (POINTER-ADDRESS IMPURE-MAX))

         )))
