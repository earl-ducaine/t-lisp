(HERALD (TSYS COMPILE T 8)
        (ENV TCOMP))

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;;; Compilation of a single form

;;; Here we alphatize, optimize, bind-annotate, and canonicalize a top-level
;;; S-expression.

(DEFINE (BEGIN-COMPILATION SEXPR)
  (SET *TEST* NIL)                     ; release former garbage
  (BIND ((*INITIAL-VERSION* (PHASE!ALPHATIZE SEXPR)))
    (BEGIN-COMPILATION-1 *INITIAL-VERSION*)))

(DEFINE (BEGIN-COMPILATION-1 NODE)
  (PHASE!PROPAGATE NODE)
  (BIND ((*META-VERSION* (PHASE!META-EVALUATE NODE)))
    (PHASE!BIND-ANNOTATE *META-VERSION*)
    (PHASE!CANONICALIZE *META-VERSION*)))

;;; Do the other compilation passes: finish the annotation, and generate code.

(DEFINE (FINISH-COMPILATION NODE)
  (PHASE!FLOW-ANNOTATE NODE)
  (PHASE!REP-ANNOTATE NODE)
  (PHASE!CLOSE-ANNOTATE NODE)
  (PHASE!TARGET-ANNOTATE NODE)
  (PHASE!GENERATE-CODE NODE))

;;; Compilation phase overseers, split off into separate functions to
;;; try to make the control structure a little clearer.  I don't think
;;; it's much of a help.  Most of these simply keep track of runtime and
;;; debug tracing, dispatching to the function which really does the
;;; job.

(DEFINE-LOCAL-SYNTAX (WITH-RUNTIMER TIMER . BODY)
  `(LET ((%%RUNTIME%% (RUNTIME)))
     (BLOCK0 (BLOCK ,@BODY)
             (SET ,TIMER
                  (FX+ ,TIMER (FX/ (FX- (RUNTIME) %%RUNTIME%%) 1000.))))))

(STAT-COUNTER *ALPHA-CONVERSION-RUNTIME* "ms alpha-conversion")

(LSET *PHASE* NIL)

(DEFINE (PHASE!ALPHATIZE SEXPR)
  (COND ((CTRACE? 'INPUT "See user-supplied code")
         (FORMAT *NOISE-OUTPUT* "~&~%;;; User-supplied code:~%")
         (PRETTY-PRINT SEXPR *NOISE-OUTPUT*)
         (NEWLINE *NOISE-OUTPUT*)))
  (BIND ((*INITIAL-VERSION*
          (WITH-RUNTIMER *ALPHA-CONVERSION-RUNTIME*
            (BIND ((*PHASE* 'ALPHATIZE)) 
              (ALPHATIZE SEXPR)))))
    (COND ((CTRACE? 'ALPHA "See initial alpha-conversion (*INITIAL-VERSION*)")
           (FORMAT *NOISE-OUTPUT* "~&~%;;; Initial alpha-conversion:~%")
           (SXNOISE *INITIAL-VERSION*)))
    *INITIAL-VERSION*))

(STAT-COUNTER *PROPAGATION-RUNTIME* "ms level-propagation")
(DEFINE (PHASE!PROPAGATE NODE)
  (SET (NODE-LEVEL NODE) NODE)
  (WITH-RUNTIMER *PROPAGATION-RUNTIME*
    (PROPAGATE NODE)))

(STAT-COUNTER *META-EVALUATION-RUNTIME* "ms optimization")
(DEFINE (PHASE!META-EVALUATE NODE)
  (BIND ((*TRACE-OPTIMIZERS?* (CTRACE? 'OPT "Trace optimizer")))
    (COND (*TRACE-OPTIMIZERS?*
           (FORMAT *NOISE-OUTPUT* "~&~%;;; Optimizer trace:~%")))
    (BIND ((*META-VERSION*
            (BIND ((*PHASE* 'META-EVALUATE))
              (WITH-RUNTIMER *META-EVALUATION-RUNTIME*
                (DO ((NODE NODE NEWNODE)
                     (NEWNODE (META-EVALUATE NODE) (META-EVALUATE NEWNODE)))
                    ((EQ? NEWNODE NODE) NEWNODE))))))
      (IF (OR (NODE-REFS *META-VERSION*)
              (NODE-SETQS *META-VERSION*))
          (BUG "ENV-ANALYZE lost big - failed to filter all lexical variables"
               "will pretend it hadn't lost"))
      (COND ((CTRACE? 'META "See result of optimization (*META-VERSION*)")
             (FORMAT *NOISE-OUTPUT* "~&~%;;; Optimized result:~%")
             (SXNOISE *META-VERSION*)))
      *META-VERSION*)))

(STAT-COUNTER *BIND-ANNOTATION-RUNTIME* "ms binding annotation")
(DEFINE (PHASE!BIND-ANNOTATE NODE)
  (BIND ((*PHASE* 'BIND-ANNOTATE))
    (WITH-RUNTIMER *BIND-ANNOTATION-RUNTIME*
      (BIND-ANNOTATE NODE))))


(STAT-COUNTER *CANONICALIZE-RUNTIME* "ms canonicalization")
(DEFINE (PHASE!CANONICALIZE NODE)
  (BIND ((*PHASE* 'CANONICALIZE))
    (WITH-RUNTIMER *CANONICALIZE-RUNTIME*
      (CANONICALIZE NODE))))


(STAT-COUNTER *FLOW-ANNOTATION-RUNTIME* "ms flow annotation")
(DEFINE (PHASE!FLOW-ANNOTATE NODE)
  (BIND ((*PHASE* 'FLOW-ANNOTATE))
    (WITH-RUNTIMER *FLOW-ANNOTATION-RUNTIME*
      (FLOW-ANNOTATE NODE))))

(STAT-COUNTER *REP-ANNOTATION-RUNTIME* "ms representation annotation")
(DEFINE (PHASE!REP-ANNOTATE NODE)
  (BIND ((*PHASE* 'REP-ANNOTATE))
    (WITH-RUNTIMER *REP-ANNOTATION-RUNTIME*
      (REP-ANNOTATE NODE 'POINTER))))

(STAT-COUNTER *CLOSE-ANNOTATION-RUNTIME* "ms closure annotation")
(DEFINE (PHASE!CLOSE-ANNOTATE NODE)
  (BIND ((*PHASE* 'CLOSE-ANNOTATE))
    (WITH-RUNTIMER *CLOSE-ANNOTATION-RUNTIME*
      (CLOSE-ANNOTATE NODE '()))))

(STAT-COUNTER *TARGET-ANNOTATION-RUNTIME* "ms target annotation")
(DEFINE (PHASE!TARGET-ANNOTATE NODE)
  (BIND ((*PHASE* 'TARGET-ANNOTATE))
    (WITH-RUNTIMER *TARGET-ANNOTATION-RUNTIME*
      (TARGET-ANNOTATE NODE))))

(STAT-COUNTER *CODE-GENERATION-RUNTIME* "ms code generation")
(DEFINE (PHASE!GENERATE-CODE NODE)
  (PHASE!GENERATE-CODE-1 (QLOZURE (NODE) (LAMBDA () (GENERATE-CODE NODE)))))

;;; Used also by the magic LAP construct
(DEFINE (PHASE!GENERATE-CODE-1 F)
  (BIND ((*ASSEMBLY-OUTPUT*             ; This sux
          (COND ((AND (NEQ? *ASSEMBLY-OUTPUT* (TERMINAL-OUTPUT))
                      (CTRACE? 'CG "See code"))
                 (MAKE-BROADCAST-STREAM *ASSEMBLY-OUTPUT*
                                        *NOISE-OUTPUT*))
                (ELSE *ASSEMBLY-OUTPUT*)))
         (*PHASE* 'GENERATE))
    (WITH-RUNTIMER *CODE-GENERATION-RUNTIME*
      (F))))

;;; (CTRACE phase) enables tracing of a phase of the compiler
;;; (CTRACE 'ALL) traces all phases
;;; (CTRACE) shows what phases are being traced
;;; (CTRACE-OFF phase) untraces a phase
;;; (CTRACE-OFF) untraces all phases

(DEFINE *CTRACEABLE-PHASES* '(INPUT ALPHA OPT META PACK CG))
(LSET *CTRACE* *CTRACEABLE-PHASES*)

(DEFINE (CTRACE . MAYBE-PHASE)
  (COND ((NULL? MAYBE-PHASE) *CTRACE*)
        (ELSE (LET ((PHASE (CAR MAYBE-PHASE)))
                (COND ((MEMQ PHASE *CTRACEABLE-PHASES*)
                       (SET *CTRACE* (ADJOIN PHASE *CTRACE*)))
                      ((EQ? PHASE 'ALL)
                       (SET *CTRACE* *CTRACEABLE-PHASES*))
                      (ELSE `("Choices are:" ,@*CTRACEABLE-PHASES* ALL)))))))
    
(DEFINE (CTRACE-OFF . MAYBE-PHASE)
  (COND (MAYBE-PHASE (SET *CTRACE* (SETREMQ (CAR MAYBE-PHASE) *CTRACE*)))
        (ELSE (SET *CTRACE* NIL))))

;;; Hack...

(DEFINE-LOCAL-SYNTAX (CHOOSE-LOOP QUERY . CLAUSES)
  `(CATCH RETURN
     (DO () (NIL)
       (FORMAT (TERMINAL-OUTPUT) "~&--~A?--" ,QUERY)
       (CASE (DO ((C (READC (TERMINAL-INPUT)) (READC (TERMINAL-INPUT))))
		 ((NOT (CHAR= C #\NEWLINE))
		  C))
	     ((#\?)
	      (SHOW-CHOICES ',(MAP CADR CLAUSES)))
	     ,@(MAP (LAMBDA (CLAUSE) (CONS (CAR CLAUSE) (CDDR CLAUSE)))
		    CLAUSES)))))

(DEFINE (SHOW-CHOICES CHOICES)
  (WALK (LAMBDA (CHOICE)
          (FORMAT (TERMINAL-OUTPUT) "  ~A~%" CHOICE))
        CHOICES))

(DEFINE (CTRACE? PHASE QUERY)
  (AND (MEMQ PHASE *CTRACE*)
       (OR (NOT *PROBE?*)
           (CHOOSE-LOOP QUERY
             ((#\Y #\y #\SPACE) "Space or Y: yes" (RETURN T))
             ((#\N #\n #\RUBOUT) "Rubout or N: no" (RETURN NIL))
             ((#\B #\b) "B: breakpoint" (BREAK QUERY))
             ((#\!)     "!: stop asking" (SET *PROBE?* NIL) (RETURN T))
             ((#\G #\g #\ALT) ": stop tracing everything"
                              (CTRACE-OFF) (RETURN NIL))
             ((#\-)       "-: stop tracing this phase only"
                          (CTRACE-OFF PHASE) (RETURN NIL))
             ((#\Q #\q) "Q: throw to top level" (*ABORT* NIL))
             ))))
