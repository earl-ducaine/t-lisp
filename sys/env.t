(HERALD (TSYS ENV T 150)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Code for value cells, environments, locales, and loaded-files.

;;; General remarks:
;;; - A locale is a kind of environment.
;;; - A loaded-file is a different kind of environment.
;;; - The interpreter creates other environments which are neither loaded-files
;;;    nor locales.
;;; - Every locale maintains a population of its inferior loaded-files.
;;; - Any environment may be used as a "shape" (see EVAL).
;;; - "ID" usually abbreviates "loaded-file identifier."

(DEFINE (REALLY-ENV-WARN MSG ID)        ; See EARLY.T for ENV-WARN.
  (COND (*PRINT-ENV-WARNINGS?*
         (LET ((OUT (ERROR-OUTPUT)))
           (FORMAT OUT "[~A~_~S]~_" MSG ID)
           (FORCE-OUTPUT OUT)))))

;;; Non-local reference.

(DEFINE *VALUE
  (OBJECT (LAMBDA (ENV ID)
            (COND ((ENV-LOOKUP ENV ID NIL NIL) => CONTENTS)
                  (ELSE
                   (ERROR "unbound variable~%  (~S ~S ~S)"
                          '*VALUE ENV ID))))
          ((SETTER SELF) *SET-VALUE)))

;;; Non-local definition.

(DEFINE (*DEFINE ENV ID VAL)
  (DEFINE-CONTENTS (ENV-LOOKUP ENV ID T T) VAL)
  (SET-IDENTIFICATION VAL ID)
  VAL)

(DEFINE (*LSET ENV ID VAL)
  (SET-CONTENTS (ENV-LOOKUP ENV ID T T) VAL))

(DEFINE (*SET-VALUE ENV ID VAL)
  (SET-CONTENTS (RELUCTANTLY-BIND ENV ID) VAL))

(DEFINE (RELUCTANTLY-BIND ENV ID)
  (OR (ENV-LOOKUP ENV ID NIL NIL)
      (BLOCK (ENV-WARN "Binding" ID)
             (ENV-LOOKUP ENV ID T T))))

(DEFINE (*BOUND? ENV ID)                ; ugh
  (COND ((ENV-LOOKUP ENV ID NIL NIL)
         => (LAMBDA (VC)
              (OR (NOT (VCELL? VC))
                  (NOT (NONVALUE? (VCELL-CONTENTS VC))))))
        (ELSE NIL)))

;;; VCELLs are locatives.

(DEFINE HANDLE-VCELL
  (%HANDLER VCELL
	    ((CONTENTS SELF)
             (LET ((Z (VCELL-CONTENTS VCELL)))
               (COND ((NONVALUE? Z)
                      (ERROR "bound variable ~S has no value"
			     (VCELL-ID VCELL)))
                     (ELSE Z))))
            ((SET-CONTENTS SELF VALUE)
             (CHECK-REBINDING VCELL NIL SET-CONTENTS)
             (SET (VCELL-CONTENTS VCELL) VALUE))
            ((DEFINE-CONTENTS SELF VALUE)
             ;; Kludge to allow (DEFINE X X)
             (COND ((OR (NULL? (VCELL-INFO VCELL))
                        (NEQ? (VCELL-CONTENTS VCELL) VALUE))
                    (CHECK-REBINDING VCELL T DEFINE-CONTENTS)))
             (SET (VCELL-CONTENTS VCELL) VALUE))
	    ((LOCATIVE? SELF) T)
	    ((IDENTIFICATION SELF) (VCELL-ID VCELL))
	    ((PRINT-TYPE-STRING SELF) "Value-cell")))

;;; ENV-LOOKUP is defined by OPEN.T to be effectively the same as CALL.

(DEFINE HANDLE-ENVIRONMENT
  (%HANDLER ((GET-ENVIRONMENT SELF) SELF)
	    ((GET-LOADED-FILE SELF)
	     (GET-LOADED-FILE (ENV-SUPERIOR SELF)))
	    ((ENVIRONMENT? SELF) T)
	    ((PRINT-TYPE-STRING SELF) "Environment")))

(DEFINE-OPERATION (ENV-SUPERIOR ENV))
(DEFINE-PREDICATE ENVIRONMENT?)

;;; SUPER is any environment, not necessarily a locale.

(DEFINE (MAKE-LOCALE SUPER . MAYBE-ID)
  (REALLY-MAKE-LOCALE SUPER
		      (IF (NULL? MAYBE-ID) NIL (CAR MAYBE-ID))
		      NIL))

(DEFINE (REALLY-MAKE-LOCALE SUPER ID BOOT?)
  (LET ((TABLE (MAKE-TABLE ID))
        (LPOP NIL)
        (FLAG NIL))
    (LET ((ENV
           (OBJECT (LAMBDA (ID LOCAL? CREATE?)
                     ;; Extremely crude.
                     ;; (LET ((ID (CHECK-ARG SYMBOL? ID ENV-LOOKUP))) ...)
                     (COND (BOOT?
                            (*SYSTEM-BOOT-ENV* ID LOCAL? CREATE?))
                           ((TABLE-ENTRY TABLE ID))
                           (LOCAL?
                            (COND (CREATE?
                                   (IF (AND *PRINT-ENV-WARNINGS?* ; hack
                                            FLAG       ; another hack
                                            SUPER
                                            (SUPER ID NIL NIL))
                                       (ENV-WARN "Shadowing" ID))
                                   (SET-TABLE-ENTRY TABLE ID (MAKE-VCELL ID)))
                                  (ELSE NIL)))
                           ((AND SUPER (SUPER ID NIL NIL)))
                           (CREATE?
                            (SET-TABLE-ENTRY TABLE ID (MAKE-VCELL ID)))
                           (ELSE NIL)))
                   ((ENV-SUPERIOR SELF) SUPER)
                   ((LOCALE-INFERIORS SELF)
                    (IF (NULL? LPOP)
                        (SET LPOP
                             (MAKE-POPULATION `(LOCALE-INFERIORS ,SELF))))
                    LPOP)
		   ((LOCALE-WALK SELF PROC)
		    (TABLE-WALK TABLE PROC))
                   ((LOCALE? SELF) T)
                   ((IDENTIFICATION SELF) ID)
		   ((SET-IDENTIFICATION SELF VAL) (IF (NOT ID) (SET ID VAL)))
                   ((PRINT-TYPE-STRING SELF) "Locale")
                   (=> HANDLE-ENVIRONMENT))))
      (COND (SUPER
             (ADD-TO-POPULATION (LOCALE-INFERIORS (ENV-LOCALE SUPER)) ENV))
            (ELSE
             (ADD-TO-POPULATION *TOP-LEVEL-ENVIRONMENTS* ENV)))
      (*DEFINE ENV '&&LOADED-FILES&&
               (MAKE-TABLE '&&LOADED-FILES&&))  ; Kludge
      (LET ((SYN (MAKE-SYNTAX-TABLE (IF SUPER (ENV-SYNTAX-TABLE SUPER) NIL)
                                    ID)))
        (SET (ENV-FOR-SYNTAX-DEFINITION SYN) ENV)
        (*DEFINE ENV '&&SYNTAX-TABLE&& SYN))
      (IF (SYMBOL? ID) (*DEFINE ENV ID ENV))
      (SET FLAG T)
      ENV)))

(DEFINE *TOP-LEVEL-ENVIRONMENTS* (MAKE-POPULATION '*TOP-LEVEL-ENVIRONMENTS*))

(DEFINE-OPERATION (LOCALE-INFERIORS OBJ))
(DEFINE-OPERATION (LOCALE-WALK OBJ PROC))
(DEFINE-PREDICATE LOCALE?)

(DEFINE-OPERATION (LOADED-FILES ENV) (*VALUE ENV '&&LOADED-FILES&&))    ; UGH

(DEFINE (ENV-SYNTAX-TABLE ENV)
  (*VALUE ENV '&&SYNTAX-TABLE&&))       ;???

(DEFINE (ENV-LOCALE ENV)
  (DO ((E ENV (ENV-SUPERIOR E)))
      ((LOCALE? E) E)))

(DEFINE (MAKE-EMPTY-LOCALE . MAYBE-ID)
  (MAKE-LOCALE NIL (IF (NULL? MAYBE-ID) NIL (CAR MAYBE-ID))))

(DEFINE *T-IMPLEMENTATION-ENV*
  (REALLY-MAKE-LOCALE NIL '*T-IMPLEMENTATION-ENV* T))

(SET *THE-BOOT-ENV* *T-IMPLEMENTATION-ENV*)

;;; Used by compilers' expansion of (THE-ENVIRONMENT) forms.

(define (*the-environment outer names . locs)
  (object (lambda (name local? create?)
	    (cond ((or local? create?)
		   (error '("illegal to create new bindings"
			    " in this environment~%  ~S")
			  `(env-lookup ... ,name ,local? ,create?)))
		  ((posq name names)
		   => (lambda (n) (nth locs n)))
		  (else
		   (env-lookup outer name local? create?))))
	  ((env-superior self) outer)
	  (=> handle-environment)))

;;; Called from expansion of %DEFINE-SYNTAX.

(DEFINE (*DEFINE-SYNTAX ENV SYMBOL DESCR)
  (SET (SYNTAX-TABLE-ENTRY (ENV-SYNTAX-TABLE ENV) SYMBOL) DESCR))

(DEFINE *PRIMITIVE-SYNTAX-TABLE*        ; See EVAL.T
  (ENV-SYNTAX-TABLE *T-IMPLEMENTATION-ENV*))

(SET *SYNTAX-SYSTEM-EXISTS?* T)         ; See BOOT.T
