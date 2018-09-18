(HERALD (TCOMP LAP T 10)
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Lap stream processing

;;; Transducers for LAP code.  The idea here is that when a form
;;; (LAP -items-) appears at top level in a file of Scheme code, the file
;;; transducer call PROCESS-LAP-STREAM, which in turn basically just
;;; calls EMIT on each item (it's a little hairier than this really).

(DEFINE (PROCESS-LAP-STREAM L)
  (BIND ((*PRINT-LEVEL* 2)
         (*PRINT-LENGTH* 3))
    (FORMAT *NOISE-OUTPUT* "~%(LAP ~S ---)" (CAR L)))
  (BIND ((*GENERATE-QUEUE* (MAKE-QUEUE))
         (*TP-LOC* NIL))
    (WALKCDR PROCESS-LAP-ITEM L)
    (EMPTY-GENERATE-QUEUE)
    (SHOVECOMMENT NIL)))

(DEFINE (PROCESS-LAP-ITEM L)
  (LET ((C (CADR L)))
    (IF (STRING? C) (EMITREMARK C))
    (PROCESS-LAP-ITEM-1 (CAR L))))

(DEFINE (PROCESS-LAP-ITEM-1 X)
  (COND ((STRING? X))                   ; Ignore.
        ((SYMBOL? X)
         ;;(COND ((EQ? *ASSEMBLER-TYPE* 'VAX-UNIX)
         ;;        (EMIT `(GLOBL ,X))))         ; Mostly for ADB
         ;;(EMIT `(GLOBL ,X))           ; For UNIT_1_... tags on the Apollo
         (EMITTAG X))
        ((NOT (PAIR? X))
         (WARN "~S illegal object in LAP stream"
               "will simply ignore it"
               X))
        (ELSE
         ;; Let's assume that LAP-MAGIC things are speudo-ops, and that random
         ;; pseudo-ops don't have losing OPERAND-MACRO's in them.  This might be
         ;; wrong, but then this whole module is.
         ;; Trouble is that the operands have to get expanded before we get to
         ;; EMIT-OPERAND.  This really ought to be done in REALLY-EMIT but I
         ;; didn't want to bother that routine, it has enough to do already.
         ;;
         ;; Bletch.
         (COND ((NOT (GET (CAR X) 'LAP-MAGIC))
                (DO ((Z (CDR X) (CDR Z)))
                    ((NULL? Z))
                  (SETF (CAR Z) (EXPAND-OPERAND (CAR Z))))))
         (EMIT X))))

(DEFINE (TRANSDUCE-LAP-FILE SPEC)
  (LET ((FILENAME (->FILENAME SPEC)))
    (WITH-OPEN-STREAMS ((ISTREAM (OPEN (FILENAME-WITH-TYPE FILENAME
							   *LAP-EXTENSION*)
				       '(IN)))
			(OSTREAM (OPEN (FILENAME-WITH-TYPE FILENAME
							  *ASSEMBLY-EXTENSION*)
				       '(OUT))))
      (BIND ((*LISP-ASSEMBLY-SYNTAX?* NIL)
	     (*ASSEMBLY-OUTPUT* OSTREAM)
	     (*GENERATE-QUEUE* (MAKE-QUEUE))
	     (*DELAYED-COMMENT-TEXT* NIL)
	     (*DELAYED-COMMENT-NODE* NIL))
	(DO ((X (READ ISTREAM) (READ ISTREAM)))
	    ((EOF? X)
	     (NEWLINE OSTREAM))
	  (PROCESS-LAP-ITEM-1 X))))))

;;; Hack for operand-macros.

(DEFINE (EXPAND-OPERAND X)
  (COND ((ATOM? X) X)
        ((EQ? (CAR X) '@)
         (INDIRECT (EXPAND-OPERAND (CADR X)) NIL))
        (ELSE
         (LET ((M (GET (CAR X) 'OPERAND-MACRO)))
           (COND (M (EXPAND-OPERAND (FUNCALL M X)))
                 (ELSE X))))))

;;; LAP-MAGIC's.

(DEFUN (LABEL LAP-MAGIC) (X)
  (EMITTAG X))

(COMPILATION-GLOBAL *ASSEMBLY-SECTION*)

(DEFUN (TEXT LAP-MAGIC) (X)
  (IGNORE X)
  (TEXT-SECTION))

(DEFUN (DATA LAP-MAGIC) (X)
  (IGNORE X)
  (DATA-SECTION))

(DEFUN (RANDOM-DATA LAP-MAGIC) (X)
  (DATA-SECTION)
  (PROCESS-LAP-STREAM (CDR X))
  (EMIT '(ALIGN 3))
  (SETQ *OFFSET* -10001)
  (TEXT-SECTION))

(DEFUN (DEFSYM LAP-MAGIC) (X)
  (PUT (CADR X) 'SYM (CADDR X))
  (PRINT `(PUT ',(CADR X) 'SYM ',(CADDR X)) *SUPPORT-OUTPUT*)
  (PUSH *THE-UNIT-DEFS* (CADR X)))

;;; This is a hack for KERNEL.

(DEFUN (MOVE LAP-MAGIC) (X)
  (BIND ((*STACKNUM* 0))                        ;Irrelevant
    (MOVE (EXPAND-OPERAND (CADR X))
          (EXPAND-OPERAND (CADDR X)))))

(DEFUN (PUSH LAP-MAGIC) (X)
  (DECLARE (SPECIAL *PUSH*))
  (BIND ((*STACKNUM* 0))
    (MOVE (EXPAND-OPERAND (CADR X)) *PUSH*)))

(DEFUN (ADJUST-TAG LAP-MAGIC) (X)
  (ADJUST-TAG (CADR X) (CADDR X) (CADDDR X)))

(DEFUN (POINTER LAP-MAGIC) (X)
  (EMIT-VIRTUAL-POINTER (CADR X)))

(DEFUN (EXTERNAL-ADDRESS LAP-MAGIC) (X)
  (EMIT `(EXTERNAL ,(CADR X)))
  (EMIT `(ADDRESS ,(CADR X))))

(DEFUN (CODE-ADDRESS LAP-MAGIC) (X)
  (EMIT-CODE-ADDRESS (CADR X)))

(DEFUN (VCELL-POINTER LAP-MAGIC) (X)
  (BIND ((*BUFFER-LINKAGE-AREA?* NIL))
    (UNIT-STATIC-TAG (CADR X) (CADDR X))
    (DATA-SECTION)))

(DEFUN (FLONUM LAP-MAGIC) (X)
  (OUTPUT-FLONUM (CADR X)))

;;; I'm still not really happy with the following.  Pretty random.

;;; There are basically SIX syntaxes for notating code chunks written in LAP:
;;; {tproc|template} X {defined|random Scheme|random lap}.
;;; Scheme syntaxes:
;;;  (DEFINE-LAP-PROCEDURE FOO (-options-) -body-)
;;;  (DEFINE-LAP-TEMPLATE  FOO (-options-) -body-) - this doesn't really exist!
;;;  (LAP-PROCEDURE P$FOO T$FOO C$FOO (-options-) -body-)
;;;  (LAP-TEMPLATE        T$FOO C$FOO (-options-) -body-)
;;; LAP syntaxes:
;;;  (PROCEDURE P$FOO T$FOO C$FOO (-options-))
;;;  (TEMPLATE        T$FOO C$FOO (-options-))

;;; This is called by the qlozures that LAP-TEMPLATEs and LAP-PROCEDUREs
;;; become.  PTAG is () in the template case.

(DEFINE (GENERATE-LAP-TEMPLATE PTAG CRUFT)
  (DESTRUCTURE (((TTAG CTAG OPTIONS . BODY) CRUFT))
    (LET ((TTAGS (CONS* CTAG TTAG PTAG)))
      (EMIT-TEMPLATE TTAGS)
      (ENQUEUE *GENERATE-QUEUE*
               (QLOZURE (TTAGS OPTIONS BODY)    ;TTAGS
                 (LAMBDA ()
                   (BIND ((*GENERATE-QUEUE* (MAKE-QUEUE)))      ; KLUDGE
                     ;; We don't do the following, because it has to happen
                     ;; INSIDE the binding of *TP-LOC*.
                     ;; (LAP-CHUNK-HANDLER TTAGS OPTIONS)
                     (PROCESS-LAP-STREAM (CONS `(%LAP-TEMPLATE ,TTAGS ,OPTIONS)
                                               BODY))
                     (EMPTY-GENERATE-QUEUE)))))
      (UNIT-ADDR-REF (OR PTAG TTAG)))))

;;; Auxiliary for above

(DEFUN (%LAP-TEMPLATE LAP-MAGIC) (X)
  (LAP-CHUNK-HANDLER (CADR X) (CADDR X)))

;;; (PROCEDURE ptag ttag ctag options)

(DEFUN (PROCEDURE LAP-MAGIC) (X)
  (LAP-TEMPLATE-HANDLER (CADR X) (CDDR X)))
  
;;; (TEMPLATE ttag ctag options)

(DEFUN (TEMPLATE LAP-MAGIC) (X)
  (LAP-TEMPLATE-HANDLER NIL (CDR X)))

;;; PTAG is possibly (), and CRUFT is (ttag ctag options)

(DEFINE (LAP-TEMPLATE-HANDLER PTAG CRUFT)
  (LET ((TTAGS (CONS* (CADR CRUFT) (CAR CRUFT) PTAG)))
    (EMIT-TEMPLATE TTAGS)
    (LAP-CHUNK-HANDLER TTAGS (CADDR CRUFT))))

;;;(TEMPLATE T$FOO C$FOO
;;;       ((SUPERIOR MAKE-FOO)
;;;        (SIZE 2 1)
;;;        (EXPR 2 3 1)                 ; 2 arguments.
;;;        (HANDLER HANDLE-FOO)         ; ?
;;;        (DEFINEE FOO)))              ; Static variable?

(DEFINE (LAP-CHUNK-HANDLER TTAGS OPTIONS)
  (LET ((SUPERIOR-LOC NIL)
        (SIZE-INFO '(0 . 0))            ; Assume TPROC
        (PROCEDURE-INFO NIL)
        (HANDLER-LOC NIL)
        (DEFINEE-LOC NIL))
    (SETQ *TP-LOC* (CADR TTAGS))        ; Bound by PROCESS-LAP-STREAM
    (WALK (LAMBDA (OPTION)
            (CASE (CAR OPTION)
              ((SUPERIOR) (SETQ SUPERIOR-LOC (CADR OPTION)))
              ((SIZE)
               (SETQ SIZE-INFO (CONS (CADR OPTION) (CADDR OPTION))))
              ((GC-METHOD) (SETQ SIZE-INFO (CADR OPTION)))
              ((EXPR LEXPR RETURN)
               (SETQ PROCEDURE-INFO OPTION))
              ((HANDLER) (SETQ HANDLER-LOC (CADR OPTION)))
              ((DEFINEE)
               (SETQ DEFINEE-LOC (UNIT-STATIC-TAG (CADR OPTION) 'DEF)))
              ))
          OPTIONS)
    (BEGIN-CHUNK TTAGS 
                 SUPERIOR-LOC   ; superior ttag or nil
                 SIZE-INFO      ; (ptr . scr) or gc-method slot
                 PROCEDURE-INFO ; ... see above, or nil
                 HANDLER-LOC    ; handler ptag or nil
                 DEFINEE-LOC)))

(DEFUN (INITIAL-VALUE LAP-MAGIC) (X)
  (DESTRUCTURE (((() SYMBOL EXPR) X))
    (OUTPUT-SYSBUILD-ITEM `(VALUE ,SYMBOL ,EXPR))))
    
(DEFUN (BEGIN-ALIGNED-IMPURE-DATUM LAP-MAGIC) (X)
  (IGNORE X)
  (BEGIN-ALIGNED-IMPURE-DATUM))

(DEFUN (BEGIN-ALIGNED-FOO-DATUM LAP-MAGIC) (X)
  (IGNORE X)
  (BEGIN-ALIGNED-FOO-DATUM))

(DEFUN (BEGIN-ALIGNED-PURE-DATUM LAP-MAGIC) (X)
  (IGNORE X)
  (BEGIN-ALIGNED-PURE-DATUM))

(DEFUN (DEFINE-SLINK LAP-MAGIC) (X)
  (LET ((TAG (CADR X)))
    (IF (NOT (GET TAG 'SLINK-OFFSET))
        (WARN "You lose! ~S isn't a SLINK slot!" "none" TAG))
    (EMIT `(GLOBL ,TAG))
    (EMITTAG TAG)))

(DEFUN (ASCIZ LAP-MAGIC) (X)
  (OUTPUT-ASCIZ (CADR X)))

(DEFUN (TP-IS LAP-MAGIC) (X)
  (SETQ *TP-LOC* (CADR X)))

;;; Operand macros.  SLINK operands are produced by the code generator, but the
;;; others shouldn't be.

(DEFUN (SLINK OPERAND-MACRO) (X)
  (LET ((OFF (GET (CADR X) 'SLINK-OFFSET)))
    (OR OFF
        (BUG "~S unknown SLINK offset" "will screw up badly" X))
    `(REG SLP ,OFF)))

(DEFUN (UREF OPERAND-MACRO) (X)
  `(REG TP (- ,(CADR X) ,(CADDR X))))

(DEFUN (QUOTE OPERAND-MACRO) (X)
  (QUOTE-LOC (CADR X)))

(DEFUN (STATIC OPERAND-MACRO) (X)
  (STATIC-LOC (CADR X) (CADDR X)))
