(HERALD (TSYS THROW T 35)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Dynamic state manipulation

;;; Deficiencies:
;;;  - There's no way to temporarily "bind" the dynamic state (a la PDL
;;;    pointer args to EVAL in Maclisp).
;;;  - There's no proviso for coroutines/multitasking (stack groups).
;;; These both stem from the assumption rooted fairly deep that changes in
;;; the "dynamic state" are irreversible.  This will change.

;;; Preliminaries:

(DEFINE-INTEGRABLE (MAGIC-FRAME? FRAME)
  (EQ? (EXTEND-TEMPLATE FRAME) *MAGIC-FRAME-TEMPLATE*))

;;; State objects are actually the same as magic frames.

(DEFINE-INTEGRABLE (MAGIC-FRAME-STATE FRAME) FRAME)

(DEFINE-INTEGRABLE (STATE-PREVIOUS STATE)
  (XREF STATE 2))

(DEFINE-INTEGRABLE (STATE-UNWINDER STATE)
  (XREF STATE 1))

(DEFINE-INTEGRABLE (SET-STATE-UNWINDER STATE UNWINDER)
  (XSET STATE 1 UNWINDER))


;;; Binding:

(DEFINE (BIND-HANDLER WIND STUFF UNWIND)
  ;; Someday worry about doing things atomically.
  (WIND)
  (PUSH-MAGIC-FRAME *DYNAMIC-STATE*     ; (2) Chain
                    UNWIND              ; (1) Info
                    STUFF               ; (0) Arg to procedure
                    BIND-INTERNAL       ; Procedure
                    ))

(DEFINE (UNWIND-PROTECT-HANDLER STUFF UNWIND)
  (BIND-HANDLER FALSE STUFF UNWIND))

;;; The following really ought to be encoded in LAP in the code for the
;;; return into PUSH-MAGIC-FRAME.  Hmm.
(DEFINE (BIND-INTERNAL STATE STUFF)
  (SET *DYNAMIC-STATE* STATE)
  (BLOCK0 (STUFF)
          (PERFORM-UNWIND STATE)
          (SET *DYNAMIC-STATE* (STATE-PREVIOUS STATE))))

(DEFINE (PERFORM-UNWIND STATE)          ; Want better name!
  (LET ((UNWIND (STATE-UNWINDER STATE)))
    (SET-STATE-UNWINDER STATE (IF (EQ? UNWIND THROW-OUT-OF-UNWIND)      ; Kludge
                                  FALSE
                                THROW-OUT-OF-UNWIND))
    (UNWIND)
    (SET-STATE-UNWINDER STATE FALSE)))

(DEFINE (THROW-OUT-OF-UNWIND)
  (ERROR '("attempting to throw out of an unwind or unbind action~%"
           "Do (RET) to proceed with the throw anyhow")))

;;; Throwing: one-stack model.

;;; The following is invariant, for now at least:
;;;  (EQ? *DYNAMIC-STATE* (GET-SYNAMIC-STATE (CURRENT-FRAME)))

(DEFINE (INTERNAL-THROW TAG VAL)
  (FRAME-THROW (ESCAPE-PROCEDURE-FRAME TAG) VAL))

(LSET *THE-CURRENT-THROW-VALUE* NIL)    ; El hacko grossness
(LSET *THE-CURRENT-THROW-FRAME* NIL)

(DEFINE (FRAME-THROW FRAME VAL)
  (COND ((REASONABLE-FRAME? FRAME)
	 (LET ((A (SWAP *THE-CURRENT-THROW-VALUE* VAL))
	       (B (SWAP *THE-CURRENT-THROW-FRAME* FRAME))
	       (TO-STATE (GET-DYNAMIC-STATE FRAME)))
	   (UNWIND-TO-STATE TO-STATE)
	   (SET *THE-CURRENT-THROW-FRAME* B)
	   (SET *THE-CURRENT-THROW-VALUE* A)
	   (SET *DYNAMIC-STATE* TO-STATE))
         (PRIMITIVE-THROW FRAME VAL))
        (ELSE
         (FRAME-THROW (ERROR "invalid frame - (~S ~S ~S)"
                             'FRAME-THROW FRAME VAL)
                      VAL))))

(DEFINE (UNWIND-TO-STATE TO-STATE)
  (ITERATE LOOP ((STATE *DYNAMIC-STATE*))
    (COND ((EQ? STATE TO-STATE) 'DONE)
          ((NULL? STATE)
           (FORMAT (ERROR-OUTPUT)
                   ";** BLETCH!  Lost big while changing dynamic context to ~S!~
                  ~%;** Attempting to do the throw anyhow...~%"
                   TO-STATE))
          (ELSE
           (PERFORM-UNWIND STATE)
           (LOOP (STATE-PREVIOUS STATE))))))

(DEFINE (GET-DYNAMIC-STATE FRAME)
  (COND ((NULL? FRAME) '())
        ((MAGIC-FRAME? FRAME) (MAGIC-FRAME-STATE FRAME))
        (ELSE (GET-DYNAMIC-STATE (FRAME-PREVIOUS FRAME)))))

(DEFINE (REASONABLE-FRAME? FRAME)
  (AND (FRAME? FRAME)                   ; robust?
       (POINTER-GREATER? FRAME (STACK-POINTER))
       (POINTER-NOT-GREATER? FRAME *STACK-END*)))
