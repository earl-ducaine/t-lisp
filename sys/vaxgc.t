(HERALD (TSYS VAXGC T 50)
        (PRE-COOK)
        (ENV TSYS (TSYS GC)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; VAX-specific GC things.


;;; GC forwarded flonum marker.

(DEFINE-CONSTANT *FLONUM-FORWARD-MARKER* #x+800F)     ; See VAX arch. handbook

(DEFINE (GC-COPY-FLONUM LOC OBJ)
  (COND ((FLONUM-FORWARDED? OBJ)
         (%SET-LOC-CONTENTS LOC (FLONUM-FORWARDED OBJ))
         (NOTE-GC-REPEAT OBJ))
        (ELSE
         ;; Rep converter for DWFLO->POINTER need a TN SCRIPT for
         ;; COPY-FLONUM to work correctly
         (LET ((NEW-OBJ (HACKED-COPY-FLONUM OBJ)))
           (NOTE-GC-COPY OBJ NEW-OBJ)
           (%SET-LOC-CONTENTS LOC NEW-OBJ)
           (SET-FLONUM-FORWARDED OBJ NEW-OBJ)))))

(DEFINE-LAP-PROCEDURE HACKED-COPY-FLONUM ((EXPR 1 0 0))
  (MOVL (REG+ SP) XP)
  (MOVAQ (REG+ HP) VAL)
  (ADJUST-TAG VAL VAL %%FLONUM-TAG)
  (MOVD (REG XP (~ %%FLONUM-TAG)) (REG VAL (~ %%FLONUM-TAG)))
  (MOVL (REG+ SP) TP)                   ; pop return address and
  (JMP (REG TP)))

;;; GC string stuff

;;; This is perhaps best done by LAP code.  Pretty yicky as it is.
;;; Should hack the string printer so that it can handle forwarded strings
;;; without utterly flipping its gahooey.

;;; Unfortunately, we always need to copy the string text, even if the length
;;; is zero.  (This is to accomodate the use of strings as "buffers", that is,
;;; extendable arrays with fill-pointers.)

(DEFINE-CONSTANT *STRING-FORWARD-MARKER* -1)

(DEFINE-INTEGRABLE (STRING-FORWARDED? OBJ)	;Called by ALIVE-AFTER-GC?
  (FIXNUM-EQUAL? (STRING-BASE OBJ) *STRING-FORWARD-MARKER*))

(DEFINE (GC-COPY-STRING LOC OBJ)
  (LET ((BASE (STRING-BASE OBJ)))
    (COND ((FIXNUM-EQUAL? BASE *STRING-FORWARD-MARKER*)
           (NOTE-GC-REPEAT OBJ)
           (%SET-LOC-CONTENTS LOC (STRING-POINTER OBJ)))     ; Yick.
          (ELSE
           (SET *GC-STRING-COUNT* (FX+ *GC-STRING-COUNT* 1))
           (LET ((NEW-OBJ (CHOPY OBJ)))
             (NOTE-GC-COPY OBJ NEW-OBJ)
             (%SET-LOC-CONTENTS LOC NEW-OBJ)
             (LET ((PTR (STRING-POINTER OBJ))
                   (BASEFOO (FIXNUM->POINTER BASE)))
               (IF (OLDSPACE-CONTAINS? PTR)
                   (SET-STRING-POINTER NEW-OBJ
                                       (POINTER-ADD (GC-COPY-STRING-TEXT (POINTER-SUBTRACT PTR BASEFOO))
                                                    BASEFOO))))
             (SET-STRING-POINTER OBJ NEW-OBJ)
             (SET-STRING-BASE    OBJ *STRING-FORWARD-MARKER*))))))

;;; The low 3 bits of a string text pointer tend to be 010.  We sort of assume
;;;  herein that %%TEXT-LENGTH-OFFSET is -2.  Sigh.

(DEFINE-LAP-PROCEDURE GC-COPY-STRING-TEXT ((EXPR 1 0 0))
  (MOVL (REG+ SP) XP)
  (CMPL (REG XP %%TEXT-LENGTH-OFFSET) (LIT -1)) ; If already moved, then
  (BEQL MOVED)                                  ; 0th peso is -1
  (CVTWL (REG XP %%TEXT-LENGTH-OFFSET) R0)
  (ADDL2 (LIT 9) R0)                    ; Round up to quadword
  (BICL2 (LIT 7) R0)
  (ADJUST-TAG HP YP 2)
  (ADDL2 R0 HP)
  (MOVC3 R0                                     ; Slam over the characters
         (REG XP %%TEXT-LENGTH-OFFSET)
         (REG YP %%TEXT-LENGTH-OFFSET))
  ;; MOVC3 zeroes R5 = VAL.  XP and YP are unharmed, we hope.
  (MNEGL (LIT 1) (REG XP %%TEXT-LENGTH-OFFSET)) ; Store forwarding ptr
  (MOVL YP (REG XP (+ %%TEXT-LENGTH-OFFSET 4)))
  (MOVL YP VAL)
  (MOVL (REG+ SP) TP)                   ; Return.
  (JMP (REG TP))
MOVED
  (MOVL (REG XP (+ %%TEXT-LENGTH-OFFSET 4)) VAL); 1th peso is new address
  (MOVL (REG+ SP) TP)                   ; Return.
  (JMP (REG TP)))


;;; This doesn't really belong in this file at all.

;;; Set or clear reckless mode
;;; E.g. (SET (RECKLESSNESS) 'HIGH)

(LET ((+RECKLESSNESS+ 'LOW))            ; own variable

(DEFINE RECKLESSNESS
  (OBJECT (NAMED-LAMBDA RECKLESSNESS () +RECKLESSNESS+)
          ((SETTER SELF)
           (LAMBDA (LEVEL)
             (CASE LEVEL
               ((HIGH)
                (XSET (THE-SLINK) %%ICALL-INDEX
                      (XREF (THE-SLINK) %%CONFIDENT-ICALL-INDEX))
                (XSET (THE-SLINK) %%IRETURN-INDEX
                      (XREF (THE-SLINK) %%CONFIDENT-IRETURN-INDEX)))
               ((LOW MEDIUM)
                (XSET (THE-SLINK) %%ICALL-INDEX
                      (XREF (THE-SLINK) %%PARANOID-ICALL-INDEX))
                (XSET (THE-SLINK) %%IRETURN-INDEX
                      (XREF (THE-SLINK) %%PARANOID-IRETURN-INDEX))))
             (SET +RECKLESSNESS+ LEVEL)))))

)                                       ; elbairav nwo
