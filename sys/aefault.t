(HERALD AEFAULT
        (PRE-COOK)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Fault handling for Aegis.

;;; DEFINE-WITH-STATUS - copied from AEGIS.T

(DEFINE-LOCAL-SYNTAX (DEFINE-WITH-STATUS PAT STS . REST)
  (DESTRUCTURE (((PROC . ARGS) PAT))
    `(DEFINE ,PROC
              (LET ((,STS (MAKE-XENOID 0)))
                (OBJECT (NAMED-LAMBDA ,PROC ,ARGS ,@REST)
                        ((LAST-STATUS SELF) ,STS))))))

;;; Fault types.

(DEFINE-CONSTANT FAULT_$STOP                   #x120018)
(DEFINE-CONSTANT FAULT_$PROCESS_INTERRUPT      #x12001F)
(DEFINE-CONSTANT FAULT_$QUIT                   #x120010)
(DEFINE-CONSTANT MST_$GUARD_FAULT              #x4000A)
(DEFINE-CONSTANT FAULT_$GUARD                  MST_$GUARD_FAULT)
(DEFINE-CONSTANT BAT_$DISK_FULL                #x10002)

;;; We arrive here whenever we get a T-asynchronous fault.
;;; Be careful not to do any consing, because the disk might be full.

(DEFINE (*AEGIS-FAULT* FRAME TYPE)
  (IGNORE TYPE)
  (LET ((STATUS (FAULT-STATUS FRAME)))
    (SELECT STATUS
      ((FAULT_$STOP)     (HANDLE-STOP-FAULT      FRAME))
      ((FAULT_$GUARD)    (HANDLE-GUARD-FAULT     FRAME))
      ((BAT_$DISK_FULL)  (HANDLE-DISK-FULL-FAULT FRAME))
      (ELSE
       (COND ((AND *REALLY-DOING-GC?*
                   (FX= (CALL-XENOID 'DATA 'FIXNUM *T_$GC_DEATH-XENOID*
                                     STATUS #\l)
                        0))
              ;; Luser told us to ignore the fault.
              (SET *RE-ENTER-ON-FAULT?* T))
             ((FAULT-HANDLER STATUS)
              => (LAMBDA (H) (H FRAME)))
             (ELSE
              (BIND ((*FAULT-FRAME* FRAME))
                (SET *RE-ENTER-ON-FAULT?* T)
                (ERROR "~A (~X)"
                       (APOLLO-ERROR-TEXT STATUS (LAMBDA (C M S) (IGNORE M S) C))
                       STATUS)))))))
  (COND ((FAULT-PROCEEDABLE? FRAME)
         (PFM_$INHIBIT)
         (CLEANUP-INTERRUPT-FRAME FRAME))
        (*REALLY-DOING-GC?*
         ;; Incredible gross hack!
         (PRIMITIVE-THROW (FAULT-FRAME-PREVIOUS FRAME) 0))
        (ELSE
         (NOT-PROCEEDABLE))))

;;; Disk full - tell user to delete some files, then try to proceed.

(DEFINE (HANDLE-DISK-FULL-FAULT F)
  (IGNORE F)
  (RELEASE-FAULT-HANDLER)
  (CALL-XENOID NIL NIL *T_$DISK_FULL-XENOID*)
  (ESTABLISH-FAULT-HANDLER)
  (SET *RE-ENTER-ON-FAULT?* T))

;;; Stop - get rid of the heaps and exit as expediently as possible.

(DEFINE (HANDLE-STOP-FAULT F) 
  (IGNORE F)
  (UNMAP-AREAS)
  (CALL-XENOID NIL NIL *PFM_$SIGNAL-XENOID* FAULT_$STOP #\l))

;;; Inhibit/enable Aegis faults; establish/release fault handler.

(DEFINE (PFM_$INHIBIT)   (CALL-XENOID NIL NIL *PFM_$INHIBIT-XENOID*))
(DEFINE (PFM_$ENABLE)    (CALL-XENOID NIL NIL *PFM_$ENABLE-XENOID*))
(DEFINE (PFM_$SIGNAL ST) (CALL-XENOID NIL NIL *PFM_$SIGNAL-XENOID* ST #\l))

(DEFINE-WITH-STATUS (ESTABLISH-FAULT-HANDLER) STS
  (CALL-XENOID 'ADDR *PFM-FAULT-HANDLE-XENOID* *PFM_$ESTABLISH_FAULT_HANDLER-XENOID*
    0 0 STS *SFH-PROCEDURE-XENOID* #\l #\l))

(DEFINE-WITH-STATUS (RELEASE-FAULT-HANDLER) STS
  (CALL-XENOID NIL NIL *PFM_$RELEASE_FAULT_HANDLER-XENOID*
    STS *PFM-FAULT-HANDLE-XENOID*))

;;; Fault-status / handler association.
;;; Careful; a-list may be corrupt in the middle of a GC.

(LSET *AEGIS-FAULT-HANDLERS* '())

(DEFINE FAULT-HANDLER
  (OBJECT (LAMBDA (STATUS)
            (COND ((%ASSQ STATUS *AEGIS-FAULT-HANDLERS*) => CDR)
                  (ELSE NIL)))
          ((SETTER SELF)
           (LAMBDA (STATUS H)
             (COND ((%ASSQ STATUS *AEGIS-FAULT-HANDLERS*)
                    => (LAMBDA (Z) (SET (CDR Z) H)))
                   (ELSE
                    (PUSH *AEGIS-FAULT-HANDLERS* (CONS STATUS H))))))))

;;; Handlers for the particular fault types.

;;; Control-Q

(SET (FAULT-HANDLER FAULT_$QUIT)
     (LAMBDA (F)
       (FAULT-BREAKPOINT "** Interrupt" F)))

;;; Display manager DQ -I command

(SET (FAULT-HANDLER FAULT_$PROCESS_INTERRUPT)
     (LAMBDA (F)
       (SET *RE-ENTER-ON-FAULT?* T)
       (ZBREAKPOINT)))

;;; Stack hacking
;;; See Aegis source file "/us/ins/as.ins.pas".

(SET *AS_$STACK_HACK* (FX+ *AS_$STACK_LOW* (FIXNUM->POINTER #x400)))
(DEFINE *STACK-GUARD* *AS_$STACK_LOW*)

(DEFINE (RESET-STACK-GUARD)             ; Called from *STANDARD-TOP-LEVEL*
  (SET-GUARD *STACK-UID* *STACK-GUARD*))

;;; Define a guard in segment below the apollo one (higher mem).

(DEFINE (INITIALIZE-STACK-GUARD)
  (SET *STACK-UID*
       (LET ((INFO (PROCESS-INFO))
             (SUID (MAKE-BYTEV 8)))
         (COND (INFO (SET (BREF-POINTER SUID 0) (BREF-POINTER INFO 0))
                     (SET (BREF-POINTER SUID 4) (BREF-POINTER INFO 4))
                     SUID)
               (ELSE (ERROR "couldn't get stack uid")))))
  (RESET-STACK-GUARD))

;;; Unsafe - random SP in the code here (this should be uninterruptible!)
;;; assumes guard areas are exactly 1 segment

(DEFINE (FAULT-SP-IN-SEGMENT? FRAME ADDRESS)
  (LET ((FSP (FAULT-REG FRAME %%SP)))
    (AND (POINTER-NOT-LESS? FSP ADDRESS)
         (POINTER-LESS? FSP (FX+ ADDRESS FIXNUM-SEG_SIZE)))))

;;;(SET *RE-ENTER-ON-FAULT?* T) - should this be hacked in *AEGIS-FAULT*?
;;; -- apollo bugs in formating diag frame on a "horrible" stack overflow.

(DEFINE (HANDLE-GUARD-FAULT F)
  (SET *RE-ENTER-ON-FAULT?* T)
  (COND ((FAULT-SP-IN-SEGMENT? F *AS_$STACK_LOW*)
         (SET-GUARD *STACK-UID* *AS_$GUARD1*)
         (BIND ((*FAULT-FRAME* F))
           (COND ((EQ? (FAULT-REG F %%SP) *AS_$STACK_HACK*)
                  (ERROR "horrible stack overflow - top of stack is trash")
                  (NOT-PROCEEDABLE))
                 (ELSE
                  (ERROR "stack overflow")))))
        (ELSE
         (ERROR "unclassifiable guard fault; \"(RET)\" may win here"))))

;;; Diagnostic frame (DF) manipulation

(DEFINE-INTEGRABLE DF-REF
  (OBJECT (LAMBDA (FRAME PROC INDEX)
            (LET ((BV (POINTER-ADD FRAME 1)))
              (PROC BV (FX+ INDEX (FX- (BREF-16 BV 0) 6)))))
          ((SETTER SELF) DF-SET)))

(DEFINE-INTEGRABLE (DF-SET FRAME PROC INDEX VALUE)
  (LET ((BV (POINTER-ADD FRAME 1)))      
    (SET (PROC BV (FX+ INDEX (FX- (BREF-16 BV 0) 6))) VALUE)))

(DEFINE-LOCAL-SYNTAX (DF-REF-PROC PROC INDEX)
  `(LET* ((PROC ,PROC)
          (INDEX ,INDEX)
          (SET-PROC
           (LAMBDA (FRAME VALUE)
             (DF-SET FRAME PROC INDEX VALUE))))
     (OBJECT (LAMBDA (FRAME)
               (DF-REF FRAME PROC INDEX))
             ((SETTER SELF) SET-PROC))))

(DEFINE-CONSTANT %%DF-STATUS   #x+2)
(DEFINE-CONSTANT %%DF-ADDR     #x+48)
(DEFINE-CONSTANT %%DF-MISC     #x+4E)
(DEFINE-CONSTANT %%DF-PC       #x+5C)

(DEFINE FAULT-STATUS  (DF-REF-PROC BYTEV-ELT-32      %%DF-STATUS))
(DEFINE FAULT-ADDRESS (DF-REF-PROC BYTEV-ELT-POINTER %%DF-ADDR))
(DEFINE FAULT-PC      (DF-REF-PROC BYTEV-ELT-POINTER %%DF-PC))

(DEFINE-CONSTANT %%DF-RTNP-BIT 7)

(DEFINE (FAULT-PROCEEDABLE? F)
  (FIXNUM-BIT? (DF-REF F BYTEV-ELT %%DF-MISC) %%DF-RTNP-BIT))

;;; The following is so asynchronous GC can frob faults out of foreign code.

;(DEFINE (SET-FAULT-UNPROCEEDABLE F)
;  (SET (DF-REF F BREF %%DF-MISC)
;       (FIXNUM-LOGAND (DF-REF F BREF %%DF-MISC) 
;                     (FX- 255 (FIXNUM-ASHL 1 %%DF-RTNP-BIT)))))

;;; Access register from a fault frame.  This is used by the
;;; machine-independent portion of the fault system.

(DEFINE FAULT-REG
  (OBJECT (LAMBDA (FRAME REGNUM)
            (COND ((PSEUDO-REG? REGNUM)
                   (XREF FRAME REGNUM))
                  (ELSE
                   (DF-REF FRAME BYTEV-ELT-POINTER
                           (FX+ (POINTER-ASHL REGNUM 2) 6)))))
          ((SETTER SELF) SET-FAULT-REG)))

;;; This is separated out for the GC, which can't do generic operations.

(DEFINE (SET-FAULT-REG FRAME REGNUM VALUE)
  (COND ((PSEUDO-REG? REGNUM)
         (XSET FRAME REGNUM VALUE))
        (ELSE
         (DF-SET FRAME
                 BYTEV-ELT-POINTER
                 (FX+ (POINTER-ASHL REGNUM 2) 6)
                 VALUE))))

(DEFINE (PSEUDO-REG? FROB)
  (OR (FX= FROB %%SHP) (FX= FROB %%SAP) (FX= FROB %%JF)))

;;; We arrive here when a bogus CALL-XENOID happens.

(DEFINE (*CALL-XENOID-FAULT* F TYPE) 
  (LET ((MSG (FAULT-REG F %%VAL)))
    (LET ((MSG (IF (STRING? MSG) MSG "(lost error information)")))
      (SET *RE-ENTER-ON-FAULT?* T)
      (ERROR "inconsistent arguments to ~S: ~A" 'CALL-XENOID MSG))))

;;; Initialization routine, called from INITIALIZE-OS-STUFF.

(DEFINE (INITIALIZE-FAULT-HANDLERS)
  (INITIALIZE-STACK-GUARD)
  (ESTABLISH-FAULT-HANDLER))
