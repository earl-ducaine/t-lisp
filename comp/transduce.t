(HERALD (TCOMP TRANSDUCE T 316)
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Compiler Initialization and Top Level

(DEFINE *SOURCE-EXTENSION* 'T)
(DEFINE *LAP-EXTENSION* 'LAP)

(DEFINE (COMFILE FILESPEC)
  (COND (*BATCH-MODE?* (BATCH-COMPILE-FILE FILESPEC))
        ((MAYBE-COMPILE-FILE FILESPEC))
        (ELSE (FORMAT NIL "File ~S not found" FILESPEC))))  ; gross me out

(IMPORT *T-IMPLEMENTATION-ENV*
        *BACKTRACE
        ESCAPE-PROCEDURE-FRAME
        PRINT-SIGNAL
        CONDITION-HANDLER
        *UNSPECIFIC-ERROR-TYPE*
        *SYNTAX-ERROR-TYPE*
        *READ-ERROR-TYPE*)

(DEFINE (BATCH-COMPILE-FILE FILESPEC)
  (CATCH ABORT
    (LET ((H (LAMBDA (SIG)
               (PRINT-SIGNAL SIG (ERROR-OUTPUT))
               (CATCH FOO (*BACKTRACE (ESCAPE-PROCEDURE-FRAME FOO)))
               (ABORT SIG))))
      (BIND ((*BATCH-MODE?* T)  ;In case not called from COMFILE.  (See BUG.)
             ((CONDITION-HANDLER *UNSPECIFIC-ERROR-TYPE*) H)
             ((CONDITION-HANDLER *SYNTAX-ERROR-TYPE*)     H)
             ((CONDITION-HANDLER *READ-ERROR-TYPE*)
              (LAMBDA (SIG)
                ;; Usually this is just an extra right paren or something
                ;; equally trivial.
                (PRINT-SIGNAL SIG (ERROR-OUTPUT))
                NIL)))
        (COMPILE-FILE FILESPEC)))))

(DEFINE (COMPILE-FILE . REST)
  (OR (APPLY MAYBE-COMPILE-FILE REST)
      (COMPILE-FILE (ERROR "file not found~%  ~S"
			   `(COMPILE-FILE ,@REST)))))

(LSET *NOISE-MINUS-TERMINAL* (TERMINAL-OUTPUT))
(LSET *SUPPRESS-FLINK?* T)

;;; Three optional arguments: read table, syntax table, support env.

(DEFINE (MAYBE-COMPILE-FILE FILESPEC . REST)
  (LET* ((RT (RUNTIME))
         (GCT (GCTIME))
         (FILENAME (->FILENAME FILESPEC))
         (TFILE (PROBE-GENERATION
                 (COND ((FILENAME-TYPE FILENAME) FILENAME)
                       (ELSE (FILENAME-WITH-TYPE FILENAME
						 *SOURCE-EXTENSION*))))))
    (COND ((NOT TFILE) NIL)
          (ELSE
           (LET ((LAPX (IF *LISP-ASSEMBLY-SYNTAX?*
                           *LAP-EXTENSION*
                         *ASSEMBLY-EXTENSION*)))
             (WITH-OPEN-FILES
              ((ISTREAM TFILE                                          '(IN))
               (ASTREAM (FILENAME-WITH-TYPE TFILE LAPX)                '(OUT))
               (SSTREAM (FILENAME-WITH-TYPE TFILE *SUPPORT-EXTENSION*) '(OUT))
               (NSTREAM (FILENAME-WITH-TYPE TFILE *NOISE-EXTENSION*)   '(OUT)))
              (LET ((N+T (MAKE-BROADCAST-STREAM NSTREAM
                                                (TERMINAL-OUTPUT))))
                (BIND ((*ASSEMBLY-OUTPUT* ASTREAM)
                       (*SUPPORT-OUTPUT*  SSTREAM)
                       (*NOISE-OUTPUT* (IF *NOISY?* N+T NSTREAM))
                       ((ERROR-OUTPUT) N+T)
                       (*NOISE-MINUS-TERMINAL* NSTREAM)
                       (*NOISE-PLUS-TERMINAL* N+T)
                       (*SUPPRESS-FLINK?* NIL)
                       (*PRE-COOK?* NIL)
		       ;; (car '()) => ()
		       (*SYNTAX-TABLE* (OR (CADR REST)
					   *TC-SYNTAX-TABLE*))
		       (*NAMESPACE* (OR (CADDR REST) *STANDARD-SUPPORT-ENV*)))
		  (IF (CAR REST)
		      (SET (STREAM-READ-TABLE ISTREAM) (CAR REST)))
                  (COMP-BIND (LAMBDA ()
			       (PRE-TRANSDUCE TFILE)
			       (TRANSDUCE ISTREAM TFILE)
			       (POST-TRANSDUCE (FX- (RUNTIME) RT)
					       (FX- (GCTIME) GCT)
					       TFILE))))))
	     (ASSEMBLE TFILE))))))

(DEFINE (PRE-TRANSDUCE FILENAME)
  (LET ((BLURB *VERSION-BLURB*))

    ;; Introduce the noise file.
    (FORMAT *NOISE-OUTPUT* "~%~A----- Beginning T compilation on ~S~2%"
            #\SEMICOLON         ; uluz
            (FILENAME->STRING FILENAME))

    ;; Introduce the assembly file.
    (LET ((C (IF *LISP-ASSEMBLY-SYNTAX?*
                 #\SEMICOLON
                 *COMMENT-START-CHARACTER*))
	  (Z (WITH-OUTPUT-TO-STRING FOO (WRITE FOO FILENAME))))
      (FORMAT *ASSEMBLY-OUTPUT*
              '("~C ~A T compiler assembly file~%"
                "~C Compiled from ~A~%"
                "~C ~A~2%")
              C *TARGET-MACHINE*
              C Z
              C BLURB)

      ;; Introduce the support file.
      (FORMAT *SUPPORT-OUTPUT*
	      '(";;; ~A TC support file for ~A~%"
		";;; ~A~2%~S~%")
	      *TARGET-MACHINE* Z BLURB
	      `(COMMENT (SOURCE-FILENAME ,FILENAME)))
      )))

(DEFINE (POST-TRANSDUCE RT GCT FILENAME)
  (IGNORE RT GCT)
  (STATS)
  (FORMAT *NOISE-OUTPUT* "~2&;----- Finished compiling ~S~2%"
          (FILENAME->STRING FILENAME))

  ;; Make sure the other output files end with crlf's.
  (NEWLINE *ASSEMBLY-OUTPUT*)
  (NEWLINE *SUPPORT-OUTPUT*))

;;; File transducer.  This is where the interesting stuff happens.

;;; All real code emission etc. happens as a result of a call to this
;;; function.  It maps PROCESS-FORM over all forms read from the source
;;; file, gathering their ALPHATIZE'd skeletons together in one big
;;; LAMBDA-body, which at the end gets compiled.  A clever hack causes
;;; compilation of most function bodies along the way, so we don't need
;;; to have all code-trees for all functions in core at the same time.
;;; (This is mostly for Maclisp's benefit, as (theoretically at least)
;;; it shouldn't matter much on a large-address-space machine.)

;;; Things yet to do:
;;; - Block compilation (turn file into a big LABELS, maybe?)
;;; - Explicit reclamation of dead nodes (to reduce GC frequency)

(DEFINE (TRANSDUCE IFILE FILENAME)
  (LET ((FORM (READ IFILE)))
    (COND ((AND (PAIR? FORM) (EQ? (CAR FORM) 'HERALD))
	   ;; As a side-effect, PROCESS-HERALD sets *NAMESPACE*.
	   (PROCESS-HERALD FORM IFILE)  ;must precede the next READ
	   (REALLY-TRANSDUCE FORM (READ IFILE) IFILE FILENAME))
	  (ELSE
	   (LET ((H `(HERALD ,(LET ((NAME (FILENAME-NAME FILENAME)))
				(COND ((STRING? NAME) 'UNKNOWN-FILE-ID)
				      (ELSE NAME))))))
	     ;(WARN "file ~S doesn't begin with a HERALD form"
		    ;      "will use ~S for the file's HERALD"
		    ;      (FILENAME->STRING FILENAME)
		    ;      H)
	     (PROCESS-HERALD H IFILE)
	     (REALLY-TRANSDUCE H FORM IFILE FILENAME))))))

(LSET *FILE-TOPLEVEL-NODE-LIST* NIL)

(DEFINE (REALLY-TRANSDUCE H FORM IFILE FILENAME)
  ;; Compare these bindings with TEST-COMPILE's - they should be similar.
  (BIND ((*PROBE?* *TESTING?*)
         (*CTRACE* (IF *TESTING?* *CTRACEABLE-PHASES* '()))
         (*FILE-TOPLEVEL-NODE-LIST* '())
         (*COMPILE-TPROCS-EARLY?* T)
         (*SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *SYNTAX-TABLE* 'TRANSDUCE))
	 (*NAMESPACE* (MAKE-SUPPORT-ENV *NAMESPACE* 'TRANSDUCE)))
    (BEGIN-ASSEMBLY-FILE H FILENAME)
    (DO ((FORM FORM (READ IFILE)))
	((EOF? FORM)
	 (COND ((NULL? *FILE-TOPLEVEL-NODE-LIST*)
		(MENTION "source file is empty"
			 "fine with me, but it seems kind of pointless")
		(PROCESS-FORM 0)))
	 ;; Gather all the top-level forms together into one procedure.
	 (DO-TOPLEVEL-NODES T)
	 (FINISH-ASSEMBLY-FILE))
      (PROCESS-FORM FORM))))

;;; If CANONICALIZE is doing its job right, this should return a funny CONSTANT
;;;  node.

(DEFINE (DO-TOPLEVEL-NODES LAST-TIME?)
  ;; Gather all the top-level forms together into one procedure.
  (LET ((NODE (ALPHATIZE `(LAMBDA ()
                            ,@(MAP (LAMBDA (NODE) (INT NODE)) ;INT is macro
                                   (REVERSE! *FILE-TOPLEVEL-NODE-LIST*))))))
    (IF LAST-TIME? (SET *FILE-TOPLEVEL-LAMBDA-NODE* NODE))
    (SET *FILE-TOPLEVEL-NODE-LIST* '())        ; drop pointer
    (LET ((NEWNODE (BIND ((*COMPILE-TPROCS-EARLY?* T))
                     (BEGIN-COMPILATION-1 NODE))))
      (IF (NOT (CONSTANT-NODE? NEWNODE))
          (BUG "top level nodes didn't collapse to TPROC"
               "will act like it's all ok, but something strange is going on"))
      NEWNODE)))

;;; As value, this routine should return something to put in the ID field of
;;;  the unit.
;;; Maybe this should be sort of the other way around (which gets reset later,
;;;  somewhere in TRANSDUCE - see above).
;;; As a side-effect, *NAMESPACE* should be given a value.

(DEFINE (PROCESS-HERALD FORM IFILE)
  (LET* ((HERALDED-FILENAME (->FILENAME (CADR FORM)))
	 (PROBE (FILENAME-NAME HERALDED-FILENAME)))
    (SET *GENTAG-QUALIFIER*
	 (COND ((AND (SYMBOL? PROBE)
		     (FX< (STRING-LENGTH (SYMBOL-PNAME PROBE)) 12.))
		PROBE)
	       (ELSE NIL)))
    (FORMAT *SUPPORT-OUTPUT* "~S~%"
	    `(COMMENT (HERALDED-FILENAME ,HERALDED-FILENAME))))
  (LET ((ITEMS (CDDR FORM)))
    (LET ((Z (ASSQ 'READ-TABLE ITEMS)))
      (IF Z (SET (STREAM-READ-TABLE IFILE)
                 (EVAL (CADR Z) *SCRATCH-ENV*)))) ;!!!
    (WALK PROCESS-HERALD-ITEM ITEMS)
    (IF *PRE-COOK?* (FORMAT *NOISE-OUTPUT* "~&;  (precooked)~%"))))

(DEFINE (PROCESS-HERALD-ITEM ITEM)
  (COND ((ATOM? ITEM))
        (ELSE (LET ((PROC (GET (CAR ITEM) 'HERALD)))
                (COND (PROC (PROC ITEM))
                      (ELSE (WARN "unrecognized item in HERALD: ~S"
                                  "will ignore it"
                                  ITEM)))))))

(DEFUN (READ-TABLE HERALD) (ITEM)
  (IGNORE ITEM))

(DEFUN (SYNTAX-TABLE HERALD) (ITEM)
  (SET *SYNTAX-TABLE* (EVAL (CADR ITEM) *SCRATCH-ENV*)))    ;!!?

(DEFUN (SYSGEN HERALD) (ITEM)
  (IGNORE ITEM)
  (SET *DOING-SYSGEN?* T))

(DEFUN (MAKE-BASE HERALD) (ITEM)
  (IGNORE ITEM)
  (SET *DOING-SYSGEN?* T))

(DEFUN (MAKE-SYSTEM HERALD) (ITEM)
  (IGNORE ITEM)
  (SET *MAKE-SYSTEM?* T))

(DEFUN (PRE-COOK HERALD) (ITEM)
  (IGNORE ITEM)
  (SET *PRE-COOK?* T))

(DEFUN (LANGUAGE HERALD) (ITEM)
  (COND ((NOT (ALIKEV? (CDR ITEM) '(T)))
         (WARN "unknown LANGUAGE in HERALD: ~S"
               "will act as if it's T if you proceed from the breakpoint"
               ITEM)
         (BREAK LANGUAGE-LOSSAGE))))

(DEFUN (ENV HERALD) (ITEM)
  (LET ((LOWDOWN (CADR ITEM))
        (SUPPORT (CDDR ITEM)))
    (LET ((PROBE (ASSQ LOWDOWN *ENV-BASES*)))
      (SET *NAMESPACE*
	   (MAKE-SUPPORT-ENV
	    (COND ((NULL? PROBE)
		   (WARN "unknown context identifier in HERALD - ~S"
			 "will use standard context"
			 ITEM)
		   *NAMESPACE*)
		  (ELSE
		   (CDR PROBE)))
	    'ENV))
      (WALK (LAMBDA (FILESPEC) (LOAD-SUPPORT FILESPEC *NAMESPACE*))
            SUPPORT))))

(DEFUN (SUPPORT HERALD) (ITEM)
  (LET ((LOWDOWN (CADR ITEM))
        (SUPPORT (CDDR ITEM)))
    (LET ((PROBE (EVAL LOWDOWN *SCRATCH-ENV*)))     ;!!??
      (SET *NAMESPACE*
           (MAKE-SUPPORT-ENV PROBE 'SUPPORT))
      (WALK (LAMBDA (FILESPEC) (LOAD-SUPPORT FILESPEC *NAMESPACE*))
            SUPPORT))))

(DEFINE (*DEFINE-SUPPORT-ENV NAME SUPPORT)          ;Moribund
  (LET ((NS (MAKE-SUPPORT-ENV *STANDARD-SUPPORT-ENV* NAME)))
    (WALK (LAMBDA (FILESPEC) (LOAD-SUPPORT FILESPEC NS))
          SUPPORT)
    (COND ((ASSQ NAME *ENV-BASES*)
           => (LAMBDA (Z) (SET (CDR Z) NS)))
          (ELSE
           (PUSH *ENV-BASES* (CONS NAME NS))))
    NAME))

(DEFINE (LOAD-SUPPORT FILESPEC NS)
  (LET* ((F (FILENAME-WITH-TYPE (->FILENAME FILESPEC) *SUPPORT-EXTENSION*))
         (S (FILENAME->STRING F)))
    (COND ((FILE-EXISTS? F)
           ;; This ought to check version number against source for
           ;; consistency, but it doesn't.
           (FORMAT *NOISE-OUTPUT* "~&;Obtaining support file ~S~%" S)
           (WITH-OPEN-STREAMS ((STREAM (OPEN F '(IN))))
             (BIND ((*NAMESPACE* NS))                       ; Lose lose
               (DO ((FORM (READ STREAM) (READ STREAM)))
                   ((EOF? FORM))
                 (PROCESS-SUPPORT-FORM FORM)))))
          (ELSE
           (WARN "REQUIRE-SUPPORT: file ~S not found"
                 "will make no attempt to load it"
                 S)))))

(DEFINE (PROCESS-SUPPORT-FORM FORM)
  (CASE (CAR FORM)
    ((SYSBUILD-ITEM COMMENT) T)                 ; Ignore these
    ((CPUT)
     (LET ((SYM (CADR FORM))
           (PROP (CADDR FORM))
           (VAL (CADDDR FORM)))
       (COND ((OR (ATOM? SYM)
                  (NEQ? (CAR SYM) 'QUOTE)
                  (ATOM? PROP)
                  (NEQ? (CAR PROP) 'QUOTE))
              (LOSING-SUPPORT-FORM FORM))
             ((EQ? (CADR PROP) 'MAGIC)
              (SET (SYNTAX-TABLE-ENTRY *SYNTAX-TABLE* (CADR SYM))
                   (EVAL VAL *TC-MACRO-DEFINITION-ENV*)))
             ((MEMQ? (CADR PROP)
                     '(INTEGRABLE-FUNCTION CONSTANT DEFINED SETTER))
              (CPUT (LOCAL-LOOKUP *NAMESPACE* (CADR SYM) T)
                    (CADR PROP)
                    (EVAL VAL *TC-ENV*)))
             (ELSE
              (LOSING-SUPPORT-FORM FORM)))))
    ((PUT)
     (EVAL FORM *TC-ENV*))
    (ELSE
     (LOSING-SUPPORT-FORM FORM))))

(DEFINE (LOSING-SUPPORT-FORM FORM)
  (WARN "illegal form in support file - ~S"
        "will ignore it"
        FORM))

;;; COMP-BIND (this function needs a better name!)
;;; Calls argument, setting up user namespace and preparing for undoing
;;; compilation side effects.  (Thus if we ^G a compilation there should be no
;;; trace of the fact that we ever started one up in the first place.)

;;; Maybe this should flush cached VARIABLE-STRUCT props in the system
;;; namespace, too?  And CONSTANT-STRUCT props on symbols?...

(LSET *UNDO-LIST* '())
(LSET *FILE-TOPLEVEL-LAMBDA-NODE* NIL)
(LSET *INTEGRATED?* NIL)
(LSET *BIND-ANNOTATE-NODE* NIL)
(LSET *THE-SCRATCH-AREA* NIL)
(LSET *ABORT* FALSE)

(DEFINE (COMP-BIND PROC)
  (CATCH ABORT
  (BIND ((*ABORT* ABORT)
	 (*DOING-SYSGEN?* NIL)
         (*ERROR-COUNT* 0)
         (*UNDO-LIST* '())
         (*ENV* '())
         (*FILE-TOPLEVEL-LAMBDA-NODE* NIL)
         (*INTEGRATED?* NIL)
         ((GEN-ID-COUNT) 0)             ; for system macros
         (*GENTAGNUM* 16)               ; for assembly tags
         (*TN-SERIAL-NUMBER* 0)
         (*BIND-ANNOTATE-NODE* NIL)
         (*GENTAG-QUALIFIER* NIL)
         (*DELAYED-COMMENT-TEXT* NIL)
         (*DELAYED-COMMENT-NODE* NIL)
         (*THE-LINKAGE-AREA* '())       ; Unit stuff
         (*THE-STRUCTURE-AREA* '())
         (*THE-SCRATCH-AREA* '())
         (*THE-STRINGS* '()))
    (PROGV (MAP CAR *STAT-COUNTERS*) ;Grossly inefficient. Worry?
           (LET ((Z (LIST 0.))) (RPLACD Z Z))
           (PROGV *COMPILATION-GLOBALS*
                  (LET ((Z (LIST NIL))) (RPLACD Z Z))
                  (UNWIND-PROTECT
                   (BLOCK0 (PROC)
                           (IF (NOT (ZERO? *ERROR-COUNT*))
                               (FORMAT *NOISE-OUTPUT* "~&;;; ~D error~P~%"
                                       *ERROR-COUNT* *ERROR-COUNT*)))
                   (WALK (LAMBDA (X) (X)) *UNDO-LIST*)))))))

(DEFINE (PROCESS-FORM SEXPR)
  (COND ((AND (PAIR? SEXPR) (EQ? (CAR SEXPR) 'LAP))     ; Context screws?
         (PHASE!GENERATE-CODE-1 (QLOZURE (SEXPR)        ; ?!
                                  (LAMBDA ()
                                    (PROCESS-LAP-STREAM (CDR SEXPR))))))
        (ELSE
         (IF (TIME-TO-DO-TOPLEVEL-NODES?)
             (SET *FILE-TOPLEVEL-NODE-LIST*
                  (LIST (ALPHATIZE `(,(INT (DO-TOPLEVEL-NODES NIL)))))))
         (PUSH *FILE-TOPLEVEL-NODE-LIST* (BEGIN-COMPILATION SEXPR)))))
           
(DEFINE *DO-TOPLEVEL-NODES-FREQUENCY* 50) ;?

;;; This really ought to be based on some complexity measure.

(DEFINE (TIME-TO-DO-TOPLEVEL-NODES?)
  (FX>= (LENGTH *FILE-TOPLEVEL-NODE-LIST*) *DO-TOPLEVEL-NODES-FREQUENCY*))

;;; Debugging interface for compiler

(DEFINE-SYNTAX (CL FOO)
  `(TEST-COMPILE ,(IF (ATOM? FOO) FOO `',FOO)))
(DEFINE-SYNTAX (TCL)
  `(TEST-COMPILE ',(SEXPRFY *TEST*)))

(DEFINE (TEST-COMPILE PROC)
  (COMP-BIND
     (LAMBDA ()
       (SET *NAMESPACE*
	    (MAKE-SUPPORT-ENV *TEST-COMPILE-NAMESPACE* 'TEST-COMPILE))
       ;; Compare this list of bindings with TRANSDUCE's.  They should
       ;;  correspond.
       (BIND ((*TESTING?* T)
              (*PROBE?* T)
              (*CTRACE* *CTRACE*)
              (*COMPILE-TPROCS-EARLY?* NIL)
              (*GENTAG-QUALIFIER* NIL)
              (*ENABLE-LAP-COMMENTARY?* T)
	      (*SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *TC-SYNTAX-TABLE*
						 'TEST-COMPILE)))
         (LET ((SEXPR (COND ((PAIR? PROC) PROC)
                            ((PROCEDURE? PROC) (DISCLOSE PROC))
                            (ELSE (ERROR "gross function in TEST-COMPILE")))))
           (COND ((AND (PAIR? SEXPR)
                       (OR (EQ? (CAR SEXPR) 'LAMBDA)
                           (EQ? (CAR SEXPR) 'NAMED-LAMBDA)))
                  (BEGIN-ASSEMBLY-FILE '(HERALD TEST) NIL)
                  (SET *TEST* (BEGIN-COMPILATION SEXPR))
                  (FINISH-COMPILATION *TEST*)
                  (FINISH-ASSEMBLY-FILE)
                  (FORMAT NIL "~A error~P"
                          (IF (ZERO? *ERROR-COUNT*) "No" *ERROR-COUNT*)
                          *ERROR-COUNT*))
                 (ELSE "?")))))))

(DEFINE-SYNTAX (DCL FOO) `(DRIBBLE-TEST-COMPILE ',FOO))

(DEFINE (DRIBBLE-TEST-COMPILE PROC)
  (LET ((F (MAKE-FILENAME NIL NIL (IF (SYMBOL? PROC) PROC 'TEST) 'DRIB)))
    (WITH-OPEN-FILES ((DRIBFILE F '(OUT)))
      (FORMAT *NOISE-OUTPUT* "~%;Dribbling onto ~A~%"
              (FILENAME->STRING F))
      (FORMAT DRIBFILE ";;; TC dribble file~%")
      (BLOCK0 (BIND ((*NOISE-OUTPUT*
                      (MAKE-BROADCAST-STREAM DRIBFILE *NOISE-OUTPUT*))
                     (*ASSEMBLY-OUTPUT*
                      (MAKE-BROADCAST-STREAM DRIBFILE *ASSEMBLY-OUTPUT*)))
                (TEST-COMPILE PROC))
              (FORMAT DRIBFILE "~2%;;; End of dribble file~%")))))
