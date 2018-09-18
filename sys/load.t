(HERALD (TSYS LOAD T 112)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; LOAD and friends.

;;; The main confusion to surmount before understanding this code is that
;;; when you ask to load a file, there are about five different filenames
;;; potentially involved, and the trick is to keep them all straight and
;;; to check consistency at the right places.
;;; These names are:
;;;   (a) The name appearing in the source file's HERALD.
;;;   (b) The name by which the compiler found the source file.
;;;   (c) The name of the file the compiler wrote.
;;;   (d) The name by which the file was found at load time.
;;;   (e) The name given in the REQUIRE form (this is obsolescent).
;;; (Also note that in cases (b), (c), and (d), there are really two file
;;; names involved, the "given name" and the "true name"; but the current
;;; filename facility doesn't make that distinction yet.)

;;; Operations on loaded files.
;;; Loaded-file is a subtype of compiled-code, at least for now.

(DEFINE-OPERATION (LOADED-FILE-SOURCE LOADED-FILE)) ;returns a filename or #f
(DEFINE-OPERATION (LOADED-FILE-HERALD LOADED-FILE)) ;always returns a herald

(DEFINE (LOADED-FILE-ID LOADED-FILE)
  (LET ((FILENAME-NAME-SYMBOL
	 (LAMBDA (FN)
	   (LET ((NAME (FILENAME-NAME FN))) (IF (SYMBOL? NAME) NAME NIL)))))
    (COND ((HERALD-FILENAME (LOADED-FILE-HERALD LOADED-FILE))
	   => FILENAME-NAME-SYMBOL)
	  ((LOADED-FILE-SOURCE LOADED-FILE)
	   => FILENAME-NAME-SYMBOL)
	  (ELSE NIL))))

;;; Entry points for LOAD

(LSET *LOAD-NOISILY?* T)
(LSET +LOAD-NOISILY?+ T)
(DEFINE LOAD-NOISILY?
  (OBJECT (NAMED-LAMBDA LOAD-NOISILY? () *LOAD-NOISILY?*)
          ((SETTER SELF)
           (LAMBDA (VALUE) (SET *LOAD-NOISILY?* (TRUE? VALUE))))))

(LSET *PRINT-LOAD-MESSAGE?* T)
(DEFINE PRINT-LOAD-MESSAGE?
  (OBJECT (NAMED-LAMBDA PRINT-LOAD-MESSAGE? () *PRINT-LOAD-MESSAGE?*)
          ((SETTER SELF)
           (LAMBDA (VALUE) (SET *PRINT-LOAD-MESSAGE?* (TRUE? VALUE))))))

;;; Someday make the ENV-OPTION be not optional.

(DEFINE (LOAD SPEC . ENV-OPTION)
  (LOAD-AUX LOAD            SPEC ENV-OPTION T   *LOAD-NOISILY?*))

(DEFINE (LOAD-IF-PRESENT SPEC . ENV-OPTION)
  (LOAD-AUX LOAD-IF-PRESENT SPEC ENV-OPTION NIL *LOAD-NOISILY?*))

(DEFINE (LOAD-NOISILY SPEC . ENV-OPTION)
  (LOAD-AUX LOAD-NOISILY    SPEC ENV-OPTION T   T))

(DEFINE (LOAD-QUIETLY SPEC . ENV-OPTION)
  (LOAD-AUX LOAD-QUIETLY    SPEC ENV-OPTION NIL NIL))

;;; Deal with defaulting of second arg.  T has no &optionals.

(DEFINE (LOAD-AUX LOAD SPEC ENV-OPTION COMPLAIN-IF-MISSING? NOISILY?)
  (BIND ((+LOAD-NOISILY?+ NOISILY?))
    (LOAD-FILE SPEC
               (COND ((NULL? ENV-OPTION) NIL)
                     (ELSE
                      (COND ((NOT (NULL? (CDR ENV-OPTION)))
                             ;; There ought to be some clean way to do
                             ;; optional arguments in T.
                             (ERROR "too many arguments~%  ~S"
                                    `(,(IDENTIFICATION LOAD) ,SPEC
                                                             ,@ENV-OPTION))))
                      (CAR ENV-OPTION)))
               COMPLAIN-IF-MISSING?)))

;;; Fills in an extension if one wasn't supplied, and loads the file.

(DEFINE (LOAD-FILE SPEC ENV COMPLAIN-IF-MISSING?)
  (LET ((FILENAME (->FILENAME SPEC)))
    (COND ((AND (NULL? (FILENAME-TYPE FILENAME))
                (SYMBOL? (FILENAME-NAME FILENAME))) ;Backwards compatibility.
           (LET ((BINNAME (FILENAME-WITH-TYPE FILENAME 'BIN)))
             (WITH-OPEN-STREAMS ((STREAM (MAYBE-OPEN BINNAME '(IN))))
               (COND (STREAM
                      ;; Should compare file date with source, and
                      ;;  use the newer version.
                      ;; (COND ((FILE-NEWER? SRCNAME BINNAME)
                      ;;           (OFFER-TO-RECOMPILE ...)))
                      (LOAD-STREAM STREAM ENV))
                     (ELSE
                      (LOAD-FILE (FILENAME-WITH-TYPE FILENAME 'T)
                                 ENV COMPLAIN-IF-MISSING?))))))
          (ELSE
           (WITH-OPEN-STREAMS ((STREAM (MAYBE-OPEN FILENAME '(IN))))
             (COND (STREAM (LOAD-STREAM STREAM ENV))
                   (COMPLAIN-IF-MISSING?
                    (LOAD-FILE (ERROR "file not found~%  (~S ~S ~S)"
                                      'LOAD FILENAME ENV)
                               ENV COMPLAIN-IF-MISSING?))
                   (ELSE NIL)))))))

(LSET *LOAD-LEVEL* 0)

;;; Magically load whatever lurks inside the stream.

(DEFINE (LOAD-STREAM STREAM ENV)
  (LET* ((FILENAME (STREAM-FILENAME STREAM))
	 (CODE (IF (OBJECT-FILE-STREAM? STREAM)
                   (READ-COMPILED-CODE STREAM)
                   (READ-INTERPRETED-CODE STREAM ENV)))
         (TARGET-ENV (GET-TARGET-ENV CODE ENV))
         (HERALDED-FILENAME (HERALD-FILENAME (LOADED-FILE-HERALD CODE)))
	 (ID (LOADED-FILE-ID CODE)))
    (PRINT-LOAD-MESSAGE (IF FILENAME (FILENAME->STRING FILENAME) STREAM)
			ID
                        TARGET-ENV)
    (COND ((NOT (FILENAMES-COMPATIBLE? FILENAME HERALDED-FILENAME))
	   (FORMAT (ERROR-OUTPUT)
		   "~aWarning: (~s ~s ...)~%~a  doesn't match (~s ~s)~%"
		   #\SEMICOLON        ;uluz
		   'HERALD HERALDED-FILENAME
		   #\SEMICOLON        ;uluz
		   'LOAD   FILENAME)))
    (BIND ((*LOAD-LEVEL* (FX+ *LOAD-LEVEL* 1)))
      (BLOCK0 (RUN-COMPILED-CODE CODE TARGET-ENV)
	      (IF ID (SET (TABLE-ENTRY (LOADED-FILES TARGET-ENV) ID) CODE))))))

;;; Random utilities for above.

;;; Given a compiled code object, return the environment into which it
;;; believes it would like to be loaded, by evaluating the ENV clause from
;;; the object's HERALD structure.

(DEFINE (GET-TARGET-ENV CODE ENV)       ; Compatible with T 2.7
  (OR ENV (REPL-ENV)))

(comment
(DEFINE (GET-TARGET-ENV CODE ENV)
  (LET ((DEFAULT-ENV (OR ENV (REPL-ENV))))      ; ??!??!!?!
    (COND ((HERALD-ENVIRONMENT (LOADED-FILE-HERALD CODE))
           => (LAMBDA (EXP)
                (LET ((TARGET-ENV (EVAL EXP DEFAULT-ENV)))
                  (COND ((AND ENV (NEQ? TARGET-ENV ENV))
                         ;; Zowie!  What a hack!
                         (FORMAT (ERROR-OUTPUT)
                                 '("~aWarning: (~s ... (~s ~s) ...)~%"
                                   "~a  doesn't match (~s ... ~s)~%")
                                 #\SEMICOLON        ; uluz
                                 'HERALD 'ENV TARGET-ENV
                                 #\SEMICOLON        ; uluz
                                 'LOAD ENV)))
                  TARGET-ENV)))
          (ELSE DEFAULT-ENV)))))

(DEFINE (PRINT-LOAD-MESSAGE WHERE ID ENV)
  (COND (*PRINT-LOAD-MESSAGE?*
         (LET ((MSG-STREAM (TERMINAL-OUTPUT)))  ; foo
           (COMMENT-INDENT MSG-STREAM (FX* *LOAD-LEVEL* 2))
           (WRITES MSG-STREAM
                   (IF (AND ID (TABLE-ENTRY (LOADED-FILES ENV) ID))
                       "Reloading "
                       "Loading "))
           (FORMAT MSG-STREAM "~S into ~S~%"
                   WHERE
                   ;; Hack to make message more concise
                   (OR (IDENTIFICATION ENV) ENV))
           (FORCE-OUTPUT MSG-STREAM)))))

(DEFINE (COMMENT-INDENT MSG-STREAM N)       ;used also by PP?
  (FRESH-LINE MSG-STREAM)
  (WRITEC MSG-STREAM #\SEMICOLON)
  (SET-HPOS MSG-STREAM (FX+ N 1)))

;;; The screw case for ancient REQUIRE's is F1 = FOO and F2 = "~bar/foo.t"

(DEFINE (FILENAMES-COMPATIBLE? F1 F2)
  (OR (NULL? F1)
      (NULL? F2)
      (AND (ALIKEV? (FILENAME-NAME F1) (FILENAME-NAME F2))
           (LET ((D1 (FILENAME-DIR  F1))
                 (D2 (FILENAME-DIR  F2)))
             (OR (NULL? D1)
                 (NULL? D2)
                 (ALIKEV? D1 D2))))
      (AND (NULL? (FILENAME-DIR F1))
           (STRING? (FILENAME-NAME F1)))
      (AND (NULL? (FILENAME-DIR F2))
           (STRING? (FILENAME-NAME F2)))))

;;; Operations on compiled code objects.

(DEFINE-OPERATION (RUN-COMPILED-CODE CODE ENV))
(DEFINE-PREDICATE COMPILED-CODE?)

;;; Load a file of interpreted code, returning a compiled-code object.
;;; ENV is used only for evaluating things in the HERALD form.

(DEFINE (READ-INTERPRETED-CODE STREAM ENV)
  ((LAMBDA (C)
     (LET ((FIRST-FORM (READ STREAM)))
       (COND ((AND (PAIR? FIRST-FORM)
                   (EQ? (CAR FIRST-FORM) 'HERALD))
              (C (CADR FIRST-FORM) (CDDR FIRST-FORM) STREAM))
             (ELSE
              ;; File has no HERALD.  Warn only if it's really a file (?).
              ;(COND ((STREAM-FILENAME STREAM)
               ;      => (LAMBDA (FILENAME)
               ;           (FORMAT (ERROR-OUTPUT)
               ;                   "~&~aWarning: file ~S has no HERALD form.~%"
               ;                   #\SEMICOLON    ;uluz
               ;                   (FILENAME->STRING FILENAME)))))
              (C NIL '() (CONS-STREAM FIRST-FORM STREAM))))))
   (LAMBDA (SPEC CLAUSES STREAM)
     (LET* ((H (PARSE-HERALD SPEC CLAUSES))
	    (ENV (OR ENV (REPL-ENV)))
	    (SYNTAX (COND ((HERALD-SYNTAX-TABLE H)
			   => (LAMBDA (ST) (EVAL ST ENV)))
			  (ELSE
			   (ENV-SYNTAX-TABLE ENV)))))
       (COND ((HERALD-READ-TABLE H)
              => (LAMBDA (RT)
                   (SET (STREAM-READ-TABLE STREAM) (EVAL RT ENV)))))
       ;; Compare this to the code in LOAD-COMPILED-FILE.
       (STANDARD-COMPILE-STREAM STREAM SYNTAX H)))))

;;; Print value loaded.  Called by STANDARD-COMPILE-STREAM.

(DEFINE (LOAD-PRINT VAL)
  (COND ((AND +LOAD-NOISILY?+ (NOT (REPL-WONT-PRINT? VAL)))
	 (LET ((OUT (TERMINAL-OUTPUT)))
	   (PRINT (OR (IDENTIFICATION VAL) VAL) OUT)
	   (SPACE OUT)
	   (FORCE-OUTPUT OUT)))))

;;; Read a compiled-code object from a stream.

(DEFINE (READ-COMPILED-CODE STREAM)
  (LET ((UNIT (LOAD-RAW-UNIT STREAM)))
    (RELOCATE-UNIT-1 UNIT NIL)
    (MAYBE-ADJUST-SOURCE-FILENAME UNIT)
    (ADD-TO-POPULATION *UNIT-POPULATION* UNIT)
    UNIT))

(DEFINE *UNIT-POPULATION* (MAKE-POPULATION '*UNIT-POPULATION*)) ;for REFS

;;; Methods for units.

(DEFINE HANDLE-UNIT
  (%HANDLER UNIT
	   ((RUN-COMPILED-CODE SELF ENV)
	    (COND ((UNIT-ENV UNIT)
		   (IF (NEQ? (UNIT-ENV UNIT) ENV)
		       (ERROR '("can't re-run compiled code~%  ~S"
				"  (temporary restriction in ~S)")
			      `(RUN-COMPILED-CODE ,SELF ,ENV)
			      'READ-COMPILED-CODE)))
		  (ELSE
		   (RELOCATE-UNIT-2 UNIT ENV NIL)))
	    ((UNIT-THING UNIT)))
	   ((COMPILED-CODE? SELF) T)
	   ((GET-LOADED-FILE SELF) SELF)
	   ((LOADED-FILE-HERALD SELF)
	    (MAYBE-ADJUST-SOURCE-FILENAME UNIT)
	    (UNIT-HERALD UNIT))
	   ((LOADED-FILE-SOURCE SELF)
	    (MAYBE-ADJUST-SOURCE-FILENAME UNIT)
	    (COND ((UNIT-SOURCE-FILENAME UNIT) => ->FILENAME)
		  (ELSE NIL)))
	   ((PRINT SELF STREAM)
	    (FORMAT STREAM "#{Unit~_~S~_~S}"
		    (OBJECT-HASH UNIT)
		    (LOADED-FILE-ID SELF)))))

;;; Bootstrap conspiracy - see TSYSTEM and EMIT.
  
(DEFINE (MAYBE-ADJUST-SOURCE-FILENAME UNIT)
  (LET ((F (UNIT-SOURCE-FILENAME UNIT)))
    (COND ((AND (PAIR? F) (EQ? (CAR F) '**FILENAME**))
           (SET (UNIT-SOURCE-FILE-NAME UNIT)
                (APPLY MAKE-FILENAME (CDR F))))))
  (LET ((H (UNIT-HERALD UNIT)))
    (COND ((LIST? H)
           (SET (UNIT-HERALD UNIT)
                (PARSE-HERALD (UNIT-ID UNIT) H))))))

;;; (*REQUIRE id spec env) - calls to this result from expansions
;;;  of REQUIRE forms.
;;; Make this smarter some day.

(DEFINE (*REQUIRE ID SPEC ENV)
  (LET* ((FILENAME (->FILENAME SPEC))
	 (NAME (FILENAME-NAME FILENAME))
         (ID (OR ID (AND (SYMBOL? NAME) NAME)))
         (EXISTING-LOADED-FILE (AND ID (TABLE-ENTRY (LOADED-FILES ENV) ID))))
    (COND ((AND EXISTING-LOADED-FILE
                (NOT (RELOAD? FILENAME EXISTING-LOADED-FILE)))
           (COND (*PRINT-LOAD-MESSAGE?*
                  (LET ((MSG-STREAM (TERMINAL-OUTPUT)))
                    (COMMENT-INDENT MSG-STREAM (FX* *LOAD-LEVEL* 2))
                    (FORMAT MSG-STREAM "Already loaded ~S~%" ID)
                    (FORCE-OUTPUT MSG-STREAM))))
           '**FILE-ALREADY-LOADED**)
          (ELSE
           (LOAD-FILE FILENAME ENV T)))))

;;; Eventually, check file dates, compiledness, etc.

(DEFINE (RELOAD? FILENAME LOADED-FILE)
  (LET ((Z (LOADED-FILE-SOURCE LOADED-FILE)))
    (COND ((NOT (FILENAMES-COMPATIBLE? FILENAME Z))
           (ERROR '("~S has already been loaded from ~S~%"
                    "  Do (RET T) to reload from ~S, or (RET) not to")
                  (FILENAME-NAME FILENAME)
                  (FILENAME->STRING Z)
                  (FILENAME->STRING FILENAME)))
          (ELSE NIL))))

;;; HERALD parsing cruft!

;;; The CADR of a HERALD is just a filespec.  E.g.
;;;            (HERALD (TSYS LOAD T 54) ...)
;;;   or even  (HERALD #[Filename YALE-RING TSYS LOAD T 54] ...)
;;; This is the filespec for "what the file thinks it is."  It may be
;;; different from the place where the file is actually living.

;;; The parsed-HERALD structure.

(DEFINE-STRUCTURE-TYPE HERALD
    FILENAME        ;Filename, or whatever.
    READ-TABLE      ;An expression specifying a read table.
    SYNTAX-TABLE    ;An expression specifying a syntax table.
    SUPPORT         ;Expression specifying early binding environment.
    ENVIRONMENT     ;An expression specifying environment into which to load.
                    ; (Actually, not yet; this is still compatible with 2.7.)
    LANGUAGE        ;Expression evaluating to a "language," whatever that is.
    MODULE	    ;Ask John Lamping.
    PRE-COOK?       ;Boolean.
    BASE?           ;Boolean.
    SYSTEM?         ;Boolean.
    )

(LET ((H (STYPE-MASTER HERALD-STYPE)))
  (SET (HERALD-READ-TABLE   H) NIL)
  (SET (HERALD-SYNTAX-TABLE H) NIL)
  (SET (HERALD-SUPPORT      H) NIL)
  (SET (HERALD-ENVIRONMENT  H) NIL)
  (SET (HERALD-LANGUAGE     H) NIL)
  (SET (HERALD-MODULE       H) NIL)
  (SET (HERALD-PRE-COOK?    H) NIL)
  (SET (HERALD-BASE?        H) NIL)
  (SET (HERALD-SYSTEM?      H) NIL))

(LSET *HERALD-ITEMS*
      `((READ-TABLE   ,HERALD-READ-TABLE   0 CONTEXT)
        (SYNTAX-TABLE ,HERALD-SYNTAX-TABLE 1 CONTEXT)
        (SUPPORT      ,HERALD-SUPPORT      2 CONTEXT)
        (ENV          ,HERALD-ENVIRONMENT  3 CONTEXT)
        (LANGUAGE     ,HERALD-LANGUAGE     4 CONTEXT)
	(MODULE	      ,HERALD-MODULE       5 CONTEXT)
        (PRE-COOK     ,HERALD-PRE-COOK?    6 BOOLEAN)
        (MAKE-BASE    ,HERALD-BASE?        7 BOOLEAN)
        (MAKE-SYSTEM  ,HERALD-SYSTEM?      8 BOOLEAN)))

;;; Parse a HERALD.  Returns a HERALD structure.

(DEFINE (PARSE-HERALD FILESPEC CLAUSES)
  (LET ((H (MAKE-HERALD))
        (CLAUSE-ORDINAL (LAMBDA (C)
                          (COND ((AND (PAIR? C)
                                      (ASSQ (CAR C) *HERALD-ITEMS*))
                                 => CADDR)
                                (ELSE 1000)))))
    (SET (HERALD-FILENAME H)
	 (COND ((NULL? FILESPEC) NIL)
	       ((FILESPEC? FILESPEC) (->FILENAME FILESPEC))
	       (ELSE
		(SYNTAX-ERROR "bad filespec in ~S form~%  ~S"
			      'HERALD
			      `(HERALD ,FILESPEC ,@CLAUSES))
		NIL)))
    (ITERATE LOOP ((L (SORT CLAUSES
                            (LAMBDA (C1 C2)
                              (FX< (CLAUSE-ORDINAL C1) (CLAUSE-ORDINAL C2)))))
                   (ITEMS *HERALD-ITEMS*)
                   (PREV NIL))
      (COND ((OR (NULL? ITEMS)
                 (NULL? L)
                 (AND (NOT (NULL? L))
                      (NOT (PAIR? (CAR L)))))
             (IF (NOT (NULL? L))
                 (SYNTAX-ERROR "illegal ~s clause(s)~%  ~s"
                               'HERALD
                               `(HERALD ,FILESPEC . ,CLAUSES)))
             H)
            ((EQ? (CAAR L) PREV)
             (SYNTAX-ERROR "duplicate ~s clause~%  ~s"
                           'HERALD (CAR L))
             (LOOP (CDR L) ITEMS PREV))
            ((NEQ? (CAAR L) (CAAR ITEMS))
             ;; Use default value.
             (LOOP L (CDR ITEMS) NIL))
            (ELSE
             (LET ((Z (CAR ITEMS)) (C (CAR L)))
               (SET ((CADR Z) H)
                    (XCASE (CADDDR Z)
                      ((CONTEXT)
                       (LET ((FOO (IF (AND (EQ? (CAR C) 'ENV)
                                           (EQ? (CADR C) 'T))
                                      '(THE-ENVIRONMENT)   ;??!?!??!!
                                      (CADR C))))
                         (COND ((NULL? (CDR C))
                                (SYNTAX-ERROR "illegal ~s clause~%  ~s"
                                              'HERALD C))
                               ((NULL? (CDDR C)) FOO)
                               (ELSE
                                `(',AUGMENT-CONTEXT ,FOO
                                                    ,@(MAP (LAMBDA (F) `',F)
                                                           (CDDR C)))))))
                      ((BOOLEAN) T)))
               (LOOP (CDR L) (CDR ITEMS) (CAR C))))))))

(DEFINE-OPERATION (AUGMENT-CONTEXT CONTEXT . SPECS)
  (COND ((ENVIRONMENT? CONTEXT)
         (WALK (LAMBDA (SPEC)
                 (*REQUIRE NIL SPEC CONTEXT))
               SPECS)
         CONTEXT)
        (ELSE
         (ERROR "unimplemented HERALD operation~%  ~S"
                `(AUGMENT-CONTEXT ,CONTEXT . ,SPECS))
         CONTEXT)))
