(HERALD (TSYS VMREPL T 134)
        (PRE-COOK)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Read-eval-print loop for VM

;;; Use this in place of START if you want the system to start up in the
;;; Z read-eval-print loop.

;;; Setting *Z?* to true here will make life a little easier if we happen to
;;; incur some sort of error while doing system initialization.

(LSET *Z?* T)

(LSET ZTERMINAL-INPUT  *TTYIN-CHANNEL*)
(LSET ZTERMINAL-OUTPUT *TTYOUT-CHANNEL*)
(LSET ZERROR-OUTPUT       *TTYOUT-CHANNEL*)
(LSET GC-OUTPUT   ZERROR-OUTPUT)        ; On vax, should change this.
(DEFINE GC-FORMAT ZFORMAT)

;;; We assume that BIND works.  If BIND doesn't work then nothing can.

(DEFINE (ZBREAKPOINT)
  (ZFORMAT ZTERMINAL-OUTPUT "~%T virtual machine~%")
  (IF (AND *SYNTAX-SYSTEM-EXISTS?* (NOT *REALLY-DOING-GC?*))
      (ZFORMAT ZTERMINAL-OUTPUT
               "  [Type (RET) or (T-RESET) to attempt return to T]~%"))
  (CATCH RET
         (BIND ((**RET** RET)
                (*BREAK-LEVEL* (FX+ *BREAK-LEVEL* 1)))
           (ZREAD-EVAL-PRINT-LOOP))))

(SET *TOP-LEVEL* ZBREAKPOINT)

(DEFINE (ZRESET)
  (SET *TOP-LEVEL* ZBREAKPOINT)
  (**RESET** NIL))

;;; Read-eval-print loop.

(LSET *THE-ZREPL-ENV* *THE-BOOT-ENV*)

(DEFINE (ZREAD-EVAL-PRINT-LOOP)
  (ZFORMAT ZTERMINAL-OUTPUT
           (IF (FX= *BREAK-LEVEL* 1) "~&> " "~&~S(VM): ")
           (FX- *BREAK-LEVEL* 1))
  (LET ((-- (ZREAD ZTERMINAL-INPUT)))   ; Don't handle EOF.
    (COND ((EOF? --) NIL)
          (ELSE
           (SET ** (ZEVAL -- *THE-ZREPL-ENV*))
           (ZPRINT ** ZTERMINAL-OUTPUT)
           (SET ++ --)
           (ZREAD-EVAL-PRINT-LOOP)))))


;;; Error system.

(DEFINE (ZERROR . FORMAT-ARGS)
  (ZFORMAT ZTERMINAL-OUTPUT "~%** Error: ")
  (APPLY ZFORMAT ZTERMINAL-OUTPUT FORMAT-ARGS)
  (ZBREAKPOINT))

;;; ZLOAD.

(LSET *Z-PRINT-LOAD-MESSAGE?* T)

(DEFINE (ZLOAD PATH ENV)
  (ZLOAD-AUX PATH ENV NIL))

(DEFINE (ZLOAD-BIN PATH ENV)
  (ZLOAD-AUX PATH ENV T))

(DEFINE (ZLOAD-AUX PATH ENV BIN?)
  (LET ((PATH (CHECK-ARG STRING? PATH ZLOAD)))
    (LET ((CH (CHANNEL-OPEN PATH '(IN))))
      (COND ((NULL? CH)
             (ZERROR "file not found: (LOAD ~S)" PATH))
            (ELSE
             ;(SET *LOAD-CHANNEL* CH)
             (COND (*Z-PRINT-LOAD-MESSAGE?*
                    (ZFORMAT ZTERMINAL-OUTPUT ";Loading ~S~%" PATH)))
             (COND (BIN?
                    (LET ((U (CHANNEL-LOAD-RAW-UNIT CH NIL)))
                      (CHANNEL-CLOSE CH)
                      ((UNIT-THING (RELOCATE-UNIT U ENV NIL)))))
                   (ELSE
                    (ZLOAD-INTERPRETED-FILE CH ENV))))))))

(DEFINE (ZLOAD-INTERPRETED-FILE CH ENV)
  (LET ((FIRSTFROB (ZREAD CH)))
    (ITERATE LOOP ((FROB (COND ((AND (PAIR? FIRSTFROB)
                                     (EQ? (CAR FIRSTFROB) 'HERALD))
                                (ZREAD CH))
                               (ELSE FIRSTFROB)))
                   (VAL NIL))
      (COND ((EOF? FROB)
             (CHANNEL-CLOSE CH)
             VAL)
            (ELSE (LET ((VAL (ZEVAL FROB ENV)))
                    (LOOP (ZREAD CH) VAL)))))))

;;; Reader

;;; Needs (MAKE-SYMBOL pname-string)
;;;       (%MAKE-STRING size)
;;;       (SYMBOL-PNAME symbol)
;;;       (CHANNEL-READC channel)

(LSET *ZINPUT-RADIX* 10.)

(DEFINE (BREAK-CHAR? C)
  (IF (OR (CHAR= C #\()
          (CHAR= C #\))
          (CHAR= C #\.)
          (CHAR= C #\;)
          (ZWHITESPACE? C))
      T
    NIL))

(DEFINE-CONSTANT *CLOSE-PAREN* (CONS '**CLOSE-PAREN** '()))
(DEFINE-CONSTANT *DOT*         (CONS '**DOT** '()))

(LSET *PEEK-BUFFER* NIL)

(DEFINE (ZPEEKC STREAM)
  (COND (*PEEK-BUFFER*)
        (ELSE (LET ((X (ZREADC STREAM)))
                (COND ((EOF? X) X)
                      (ELSE (SET *PEEK-BUFFER* X)))))))

(DEFINE (ZREADC STREAM)
  (COND ((CHANNEL? STREAM)
         (LET ((C *PEEK-BUFFER*))
           (COND (C (SET *PEEK-BUFFER* NIL) C)
                 (ELSE (CHANNEL-READC STREAM)))))
        ((NEQ? READC ZREADC) (READC STREAM))
        (ELSE (ERROR "(ZREADC ~S)?  Bad stream." STREAM))))

(DEFINE (ZREAD STREAM)
  (SET *ZINPUT-RADIX* 10.) ;we don't have fluid binding!!
  (LET ((FORM (SUB-ZREAD STREAM)))
    (COND ((OR (EQ? FORM *DOT*) (EQ? FORM *CLOSE-PAREN*))
           (ZREAD STREAM))
          (ELSE FORM))))

(DEFINE (SUB-ZREAD STREAM)
  (LET ((C (ZREADC STREAM)))
    (COND ((EOF? C)   C)
          ((ZWHITESPACE? C) (SUB-ZREAD STREAM))
          ((CHAR= C #\()   (SUB-ZREAD-A-LIST STREAM))
          ((CHAR= C #\))   *CLOSE-PAREN*)
          ((CHAR= C #\.)   *DOT*)
          ((CHAR= C #\')   (LIST 'QUOTE (SUB-ZREAD STREAM)))
          ((CHAR= C #\`)   (LIST *BACKQUOTE* (SUB-ZREAD STREAM)))
          ((CHAR= C #\,)   (LIST (COND ((CHAR= (ZPEEKC STREAM) #\@)
                                        (ZREADC STREAM)
                                        *COMMA-ATSIGN*)
                                       (ELSE *COMMA*))
                                 (SUB-ZREAD STREAM)))
          ((CHAR= C #\#)   (ZHARPSIGN-READ-MACRO STREAM))
          ((CHAR= C #\")   (SUB-ZREAD-A-STRING STREAM))
          ((CHAR= C #\;)   (SUB-ZREAD-COMMENT STREAM))
          ((CHAR= C #\-)   (IF (ZDIGIT (ZPEEKC STREAM) *ZINPUT-RADIX*)
                               (SUB-ZREAD-A-SIGNED-NUMBER -1 STREAM)
                             (SUB-ZREAD-A-SYMBOL C STREAM)))
          ((CHAR= C #\+)   (IF (ZDIGIT (ZPEEKC STREAM) *ZINPUT-RADIX*)
                               (SUB-ZREAD-A-SIGNED-NUMBER  1 STREAM)
                             (SUB-ZREAD-A-SYMBOL C STREAM)))
          ((ZDIGIT C *ZINPUT-RADIX*) (SUB-ZREAD-A-NUMBER C STREAM))
          (ELSE            (SUB-ZREAD-A-SYMBOL C STREAM)))))

(DEFINE (ZHARPSIGN-READ-MACRO STREAM)
  (LET ((C (ZREADC STREAM)))
    (COND ((OR (CHAR= C #\O) (CHAR= C #\o)) (ZREAD-BASED-NUMBER  8. STREAM))
          ((OR (CHAR= C #\X) (CHAR= C #\x)) (ZREAD-BASED-NUMBER 16. STREAM))
          ((CHAR= C #\\) (ZREADC STREAM))
          ((CHAR= C #\.) (ZEVAL (SUB-ZREAD STREAM) *THE-ZREPL-ENV*))
          (ELSE (CONS 'SHARPSIGN C)))))

(DEFINE (ZREAD-BASED-NUMBER NEW-BASE STREAM)
  (LET ((OLD-INPUT-RADIX *ZINPUT-RADIX*))
    (SET *ZINPUT-RADIX* NEW-BASE)
    (LET ((RESULT (SUB-ZREAD STREAM)))
      (SET *ZINPUT-RADIX* OLD-INPUT-RADIX)
      RESULT)))
  
(DEFINE (SUB-ZREAD-A-LIST STREAM)
  (LET ((FORM (SUB-ZREAD STREAM)))
    (COND ((EOF? FORM) FORM)
          ((EQ? FORM *CLOSE-PAREN*) '())
          ((EQ? FORM *DOT*) (SUB-ZREAD-A-TAIL STREAM))
          (ELSE (CONS FORM (SUB-ZREAD-A-LIST STREAM))))))

(DEFINE (SUB-ZREAD-A-TAIL STREAM)
  (LET ((LAST-FORM (SUB-ZREAD STREAM)))
    (COND ((EOF? LAST-FORM) LAST-FORM)
          ((EQ? LAST-FORM *CLOSE-PAREN*) '())
          ((EQ? LAST-FORM *DOT*)         (SUB-ZREAD-A-TAIL STREAM))
          (ELSE
           (LET ((ANOTHER-FORM (SUB-ZREAD STREAM)))
             (COND ((EQ? ANOTHER-FORM *CLOSE-PAREN*) LAST-FORM)
                   ((EQ? ANOTHER-FORM *DOT*) (CONS LAST-FORM
                                                   (SUB-ZREAD-A-TAIL STREAM)))
                   (ELSE (CONS LAST-FORM (SUB-ZREAD-A-LIST STREAM)))))))))

(DEFINE (SUB-ZREAD-A-SIGNED-NUMBER SIGN STREAM) ; Loses on most negative fixnum.
  (FX* SIGN (SUB-ZREAD-A-NUMBER (ZREADC STREAM) STREAM)))

(DEFINE (SUB-ZREAD-A-NUMBER C STREAM)
  (ITERATE LOOP ((N (ZDIGIT C *ZINPUT-RADIX*)))
    (LET ((C (ZPEEKC STREAM)))
      (LET ((V (ZDIGIT C *ZINPUT-RADIX*)))
        (COND (V (ZREADC STREAM)        ;accept the character
                 (LOOP (FX+ V (FX* N *ZINPUT-RADIX*))))
              (ELSE (IF (CHAR= C #\.) (ZREADC STREAM));eat trailing dots on numbers
                    N))))))

(DEFINE (SUB-ZREAD-A-STRING STREAM)
  (ITERATE LOOP ((L '()))
    (LET ((C (ZREADC STREAM)))
      (COND ((CHAR= C #\") (BACKWARDS-LIST->STRING L))
            (ELSE (LOOP (CONS C L)))))))

(DEFINE (SUB-ZREAD-A-SYMBOL C STREAM)
  (ITERATE LOOP ((L (CONS (ZCHAR-UPCASE C) '())))
    (COND ((BREAK-CHAR? (ZPEEKC STREAM))
           (REALLY-STRING->SYMBOL (BACKWARDS-LIST->STRING L)))
          (ELSE (LOOP (CONS (ZCHAR-UPCASE (ZREADC STREAM)) L))))))

(DEFINE (SUB-ZREAD-COMMENT STREAM)
  (ITERATE LOOP ()
    (LET ((C (ZREADC STREAM)))
      (COND ((EOF? C) C)
            ((CHAR= C #\NEWLINE) (SUB-ZREAD STREAM))
            (ELSE (LOOP))))))

;;; Random auxiliaries (actually duplicates of CHARACTER and STRING code!)

(DEFINE (ZWHITESPACE? C)
  (IF (OR (CHAR= C #\SPACE) (CHAR= C #\TAB) (CHAR= C #\NEWLINE)) T NIL))

(DEFINE (ZCHAR-UPCASE C)
  (COND ((AND (CHAR>= C #\a) (CHAR<= C #\z))
         (CHAR- C (CHAR- #\a #\A)))
        (ELSE C)))

(DEFINE (ZDIGIT C RADIX)
  (LET ((C (ZCHAR-UPCASE C)))
    (COND ((AND (CHAR>= C #\0) (CHAR<= C #\9))
           (CHAR- C #\0))
          ((AND (FX> RADIX 10.)
                (CHAR>= C #\A)
                (CHAR< C (CHAR+ #\A (FX- RADIX 10))))
           (FX+ (CHAR- C #\A) 10.))
          (ELSE NIL))))


;;; Evaluator

;;; External references:
;;;      (APPLY proc args)
;;;      (ENV-LOOKUP env sym local? create?)
;;;      VCELL manipulators: -CONTENTS, -INFO
;;;      NONVALUE?, NONVALUE->VALUE, VALUE->NONVALUE
;;;      *SYNTAX-SYSTEM-EXISTS?*

(DEFINE (ZEVAL EXP ENV)
  (COND ((ATOM? EXP)
         (COND ((SYMBOL? EXP) (ZEVALUE EXP ENV))
               (ELSE EXP)))
	(ELSE
	 (LET ((HEAD (CAR EXP)))
	   (COND ((PAIR? HEAD)
		  (ZEVAL-CALL (ZEVAL HEAD ENV) (CDR EXP) ENV))
		 ((SYMBOL? HEAD)
		  (ZEVAL-SYMBOL-FORM HEAD EXP ENV))
		 ((AND *SYNTAX-SYSTEM-EXISTS?* (MACRO-EXPANDER? HEAD))
		  (ZEVAL-MACRO HEAD EXP ENV))
		 ((IDENTIFICATION HEAD)
		  => (LAMBDA (ID) (ZEVAL-SYMBOL-FORM ID EXP ENV)))
		 (ELSE
		  (ZEVAL-CALL (ZEVAL HEAD ENV) (CDR EXP) ENV)))))))

(DEFINE (ZEVAL-SYMBOL-FORM HEAD EXP ENV)
  (CASE HEAD
	((QUOTE) (CADR EXP))
	((IF)              
	 (IF (ZEVAL (CADR EXP) ENV)
	     (ZEVAL (CADDR EXP) ENV)
	     (ZEVAL (CADDDR EXP) ENV)))
	((BLOCK)
	 (ZEVAL-REST (CDR EXP) ENV))
	((LAMBDA)
	 (ZEVAL-LAMBDA (CDR EXP) ENV))
	((NAMED-LAMBDA)
	 (ZEVAL-LAMBDA (CDDR EXP) ENV))
	((SET-VAR SET-VARIABLE-VALUE)
	 (SET-ZEVALUE (CADR EXP) ENV (ZEVAL (CADDR EXP) ENV) NIL NIL))
	((DEFINE-VAR DEFINE-VARIABLE-VALUE)
	 (SET-ZEVALUE (CADR EXP) ENV (ZEVAL (CADDR EXP) ENV) T T))
	(ELSE
            (COND ((AND *SYNTAX-SYSTEM-EXISTS?*
                        (SYNTAX-TABLE-ENTRY (ZENV-SYNTAX-TABLE ENV) HEAD))
                   => (LAMBDA (DESC)
                        (ZEVAL-MACRO DESC EXP ENV)))
                  (ELSE
                   (ZEVAL-CALL (ZEVALUE HEAD ENV) (CDR EXP) ENV))))))

(DEFINE (ZEVAL-CALL PROC ARGS ENV)
  (COND ((NULL? ARGS) (PROC))
        (ELSE
         (LET ((ARGLIST (CONS (ZEVAL (CAR ARGS) ENV) '())))
           (DO ((Z ARGLIST (CDR Z))
                (ARGS (CDR ARGS) (CDR ARGS)))
               ((NULL? ARGS)
                (APPLY PROC ARGLIST))
             (SET (CDR Z) (CONS (ZEVAL (CAR ARGS) ENV) '())))))))

(DEFINE (ZEVAL-MACRO DESC EXP ENV)
  (LET ((NEW-EXP (EXPAND-MACRO-FORM DESC EXP *STANDARD-SYNTAX-TABLE*)))
    (COND ((ATOM? NEW-EXP)
           (SET (CAR EXP) 'BLOCK)
           (SET (CDR EXP) (CONS NEW-EXP '())))
          (ELSE
           (SET (CAR EXP) (CAR NEW-EXP))
           (SET (CDR EXP) (CDR NEW-EXP))))
    (ZEVAL NEW-EXP ENV)))

(DEFINE (ZEVAL-LAMBDA PARAMS+BODY ENV)
  (LET ((PARAMS (CAR PARAMS+BODY))
        (BODY (CDR PARAMS+BODY)))
    (COND ((NULL? PARAMS)               ; Trivial pessimization
           (LAMBDA ()
             (ZEVAL-REST BODY ENV)))
          (ELSE
           (LAMBDA ARGS
             (ZEVAL-REST BODY
                         (CONS ENV (CONS PARAMS ARGS))))))))

(DEFINE (ZEVAL-REST EXPS ENV)
  (COND ((ATOM? (CDR EXPS)) (ZEVAL (CAR EXPS) ENV))
        (ELSE (ZEVAL (CAR EXPS) ENV)
              (ZEVAL-REST (CDR EXPS) ENV))))

(DEFINE (ZEVALUE ID ENV)
  (COND ((NOT (PAIR? ENV))
         (LET ((PROBE (ENV-LOOKUP ENV ID NIL NIL)))
           (COND (PROBE (VCELL-CONTENTS PROBE))
                 (ELSE (VALUE->NONVALUE ID)))))
        (ELSE (ITERATE LOOP ((IDS (CADR ENV)) (VALS (CDDR ENV)))
                (COND ((ATOM? IDS)
                       (COND ((EQ? ID IDS) VALS)
                             (ELSE (ZEVALUE ID (CAR ENV)))))
                      ((EQ? ID (CAR IDS))
                       (CAR VALS))
                      (ELSE     ;; TC BUG!!
                       (LOOP (no-op (CDR IDS)) (CDR VALS))))))))

(DEFINE (SET-ZEVALUE ID ENV VAL LOCAL? DEF?)
  (COND ((NOT (PAIR? ENV))
         (LET ((LOC (ENV-LOOKUP ENV ID LOCAL? T)))
           (SET (VCELL-INFO LOC) (IF DEF? 'ZEVAL NIL))
           (SET (VCELL-CONTENTS LOC) VAL)))
        (ELSE
         (ITERATE LOOP ((IDS (CADR ENV)) (VALS (CDDR ENV)))
           (COND ((ATOM? IDS)
                  (COND ((EQ? ID IDS)
                         (ERROR "~S: can't set value of a rest-arg!" ID))
                        (ELSE (SET-ZEVALUE ID (CAR ENV) VAL LOCAL? DEF?))))
                 ((EQ? ID (CAR IDS))
                  (SET (CAR VALS) VAL)
                  VAL)
                 (ELSE
                  (LOOP (CDR IDS) (CDR VALS))))))))

(DEFINE (ZENV-SYNTAX-TABLE ENV)
  (COND ((PAIR? ENV)
	 (ZENV-SYNTAX-TABLE (CAR ENV)))
	(ELSE (ENV-SYNTAX-TABLE ENV))))
