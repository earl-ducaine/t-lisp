(herald jhack)

;;; Load this in to a T 2.8 to prepare for compiling a T 2.9 with
;;; incompatible definition of OBJECT macro.

;(*define *standard-env* '*new-env*
;  (make-locale *standard-env* '*new-env*))

(load '(xsys object t) *scratch-env*)

(set (tc-syntax-table) (env-syntax-table *scratch-env*))

(eval '(block
	(DEFINE-LAP-CONSTANT %%PRECOOKED      1011)
	(DEFINE-LAP-CONSTANT %%INCORRECT-FASL 1012)
	(DEFINE-LAP-CONSTANT %%CORRECT-FASL   1013)
	(CPUT '%OBJECT 'ENTITY-CONSTRUCTOR T)

(define-primitive-accessfn %operation-default     extend #f 0)
(define-primitive-accessfn %operation-id          extend #f 4)
(define-primitive-accessfn %operation-argspectrum extend #f 8)
(define-primitive-accessfn %operation-handler     extend #f 12)
(define-primitive-accessfn %operation-cache-obj   extend #f 16)
(define-primitive-accessfn %operation-cache-new-obj  extend #f 20)
(define-primitive-accessfn %operation-cache-new-next extend #f 24)
(define-primitive-accessfn %operation-cache-method   extend #f 28)
(define-lap-constant %%operation-size 8)

(define-primitive-accessfn %state-obj   extend #f -8)
(define-primitive-accessfn %state-op    extend #f -12)
(define-primitive-accessfn %state-next  extend #f -16)
(define-primitive-accessfn %state-self  extend #f -20)

(DEFINE (BEGIN-TEXT-SECTION)
  (TEXT-SECTION)                        
  (WALKCDR PROCESS-LAP-ITEM
           `(;; Set up pure area (code-group) leader info.
             (ALIGN 3)          "Pure area begins here."
             ,*THE-CODE-TAG*
             ;; Make these fields agree with the accessor definitions
             ;;  given in COMMON.
             (PESO ,(LSH (COND (*PRE-COOK?* TARGET:%%PRECOOKED)
                               ((EQ? *ASSEMBLER-TYPE* 'VAX-VMS) 
                                TARGET:%%CORRECT-FASL)
                               (ELSE TARGET:%%INCORRECT-FASL))
                         *SHIFT*))
             (PESO (- ,*THE-CODE-END-TAG* ,*THE-CODE-TAG*))
             (PESO (* (- ,*THE-STRUCTURE-AREA-TAG* ,*THE-UNIT-TAG*) 2))
             (PESO (* (- ,*THE-STRING-AREA-TAG*    ,*THE-UNIT-TAG*) 2))
             (PESO (* (- ,*THE-SCRATCH-AREA-TAG*   ,*THE-UNIT-TAG*) 2))
             (PESO (* (- ,*THE-UNIT-END-TAG*       ,*THE-UNIT-TAG*) 2))
             (PESO (- ,*THE-CODE-CODE-END-TAG* ,*THE-CODE-TAG*))
             (PESO 0)                   ; For future expansion
             )))
	)
  *tc-env*)

(*require nil '(tsys files) *tc-env*)

(define (foo)
  (bind (((recklessness) 'high)
	 ((*value *tc-env* '*noisy?*) nil))
    (walk comfile
	  (all-system-files (get-system 'uvm)))))
