(HERALD (TSYS DEBUG T 115)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Random collection of debugging stuff

;;; Backtrace

(DEFINE (*BACKTRACE FRAME)
  (FORMAT *DEBUG-OUTPUT* "~& Continue into~25T Module~40T Code~%")
  (DO ((FRAME FRAME (FRAME-PREVIOUS FRAME)))
      ((NULL? FRAME) *REPL-WONT-PRINT*)
    (FRAME-PRINT-SYNOPSIS FRAME *DEBUG-OUTPUT*)))

(DEFINE-OPERATION (FRAME-PRINT-SYNOPSIS FRAME STREAM)
  (LET ((THING (COND ((INTERPRETER-FRAME? FRAME)
                      (INTERPRETER-FRAME-CODE FRAME))
                     (ELSE
                      (EXTEND-TEMPLATE FRAME)))))
    (LET ((PROC-NAME (GET-PROC-NAME THING))
          (LOADED-FILE (GET-LOADED-FILE THING))
          (DISCLOSED (DISCLOSE THING)))
      (COND (PROC-NAME (FORMAT STREAM " ~S" PROC-NAME))
            (ELSE      (FORMAT STREAM " (anonymous)")))
      (COND (LOADED-FILE (FORMAT STREAM "~25T ~S" (LOADED-FILE-ID LOADED-FILE)))
            (ELSE        (FORMAT STREAM "~25T (none)")))
      (COND (DISCLOSED
             (SET (HPOS STREAM) 40)
             (WRITEC STREAM #\SPACE)
             (PRINT-ONE-LINE DISCLOSED STREAM)))
      (NEWLINE STREAM))))

(DEFINE (FRAME-DISCLOSE FRAME)      ; Used by CRAWL.
  (COND ((AND (FRAME? FRAME)
              (INTERPRETER-FRAME? FRAME))
         (DISCLOSE (INTERPRETER-FRAME-CODE FRAME)))))

(DEFINE-OPERATION (GET-PROC-NAME OBJ)
  (COND ((TEMPLATE? OBJ)
         (ITERATE LOOP ((TEM OBJ))
           (COND ((TEMPLATE-DEFINEE TEM) => VCELL-ID)
                 ((TEMPLATE-SUPERIOR-TEMPLATE TEM)
                  => LOOP)
                 (ELSE NIL))))
        (ELSE NIL)))

(DEFINE-OPERATION (GET-LOADED-FILE OBJ)
  (COND ((BOGUS-ENTITY? OBJ)
         (GET-LOADED-FILE (BOGUS-ENTITY-HANDLER OBJ)))
        ((EXTEND? OBJ)
         (GET-LOADED-FILE (EXTEND-TEMPLATE OBJ)))
        ((TEMPLATE? OBJ) (TEMPLATE-UNIT OBJ))
        (ELSE NIL)))

(DEFINE-OPERATION (GET-ENVIRONMENT OBJ)
  (COND ((AND (FRAME? OBJ)
              (INTERPRETER-FRAME? OBJ))
         (INTERPRETER-FRAME-ENV OBJ))
        ((BOGUS-ENTITY? OBJ)
         (GET-ENVIRONMENT (BOGUS-ENTITY-HANDLER OBJ)))
        ((EXTEND? OBJ)
         (LET ((PROBE (UNIT-ENV (TEMPLATE-UNIT (EXTEND-TEMPLATE OBJ)))))
           (COND ((ENVIRONMENT? PROBE) PROBE)
                 (ELSE NIL))))
        (ELSE NIL)))

(DEFINE-OPERATION (DISCLOSE OBJ) NIL)

(DEFINE-OPERATION (WHERE-DEFINED PROC)
  (COND ((GET-LOADED-FILE PROC) => LOADED-FILE-SOURCE)
        (ELSE NIL)))

;;; TRACE

(lset *trace-level* 0)

(define (make-traced-object proc id origin)
  (let ((id (or id (identification proc) proc))
	(active? t))
   (labels
    ((traced-proc
      (join
       (object (lambda arglist
		 (cond (active?
			(bind ((active? nil))
			  (let ((stream (debug-output)))
			    (comment-indent stream *trace-level*)
			    (format stream
				    "~D Calling ~S with arguments~_~S~%"
				    *trace-level* id arglist)))
			(receive vals
				 (bind ((*trace-level*
					 (fx+ *trace-level* 1)))
				   (if (operation? proc)
				       (apply operate
					      (car arglist)
					      traced-proc
					      *the-buck-stops-here*
					      arglist)
				       (apply proc arglist)))
			  (bind ((active? nil))
			    (let ((stream (debug-output)))
			      (comment-indent stream *trace-level*)
			      (format stream
				      "~D Returned from ~S with values~_~S~%"
				      *trace-level* id vals)))
			  (apply values vals)))
		       (else
			(apply proc arglist))))
	       ((get-loaded-file self) (get-loaded-file proc))  ;not a no-op!
	       ((traced-location self) origin)
	       ((traced-id self) id)
	       ((*untrace self) proc)
	       ((traced? self) t)
	       ((print self stream)
		(format stream "#{Traced~_~S~_~S}" (object-hash self) proc)))
       proc)))
    traced-proc)))

(define-operation (traced-location obj))
(define-operation (traced-id       obj))
(define-predicate traced?)

(define-operation (*trace proc id origin)       ; operations handle
  (make-traced-object proc id origin))

(define-operation (*untrace obj))

(define *traced-objects* (make-population '*traced-objects*))

(define (set-traced loc id)
  (let ((proc (contents loc)))
    (cond ((traced? proc)
           (format *debug-output* "~&~S already traced.~%" id))
          ;((operation? proc)
           ;(format *debug-output* "~&Operation tracing not yet supported.~%"))
          (else
           (let ((traced (*trace (contents loc) id loc)))
             (add-to-population *traced-objects* traced)
             (set (contents loc) traced)
             (format *debug-output* "~&~S traced.~%" id))))
    *repl-wont-print*))

(define (set-untraced loc)
  (let ((proc (contents loc)))
    (cond ((traced? proc)
           (remove-from-population *traced-objects* proc)
           (let ((probe (contents (traced-location proc))))
             (cond ((eq? probe proc)
                    (format *debug-output* "~&~S untraced.~%"
                            (set (contents loc) (*untrace proc))))
                   (else
                    (format *debug-output* "~&~S not untraceable.~%" probe)))))
          (else
           (format *debug-output* "~&~S not traced.~%" proc)))
    *repl-wont-print*))

(define (display-traced-objects)
  (format *debug-output* "~&Traced:~%")
  (walk-population (lambda (obj)
                     (cond ((eq? obj (contents (traced-location obj)))
                            (format *debug-output* "  ~S~%" (traced-id obj)))))
                   *traced-objects*)
  *repl-wont-print*)

(define (untrace-traced-objects)
  (walk-population (lambda (obj)
                     (set-untraced (traced-location obj))
                     (remove-from-population *traced-objects* obj))
                   *traced-objects*)
  *repl-wont-print*)

;;; "User interface"

(define-syntax (trace . places)
  (cond ((null? places)
         '(display-traced-objects))
        (else
         (blockify (map (lambda (place)
                          `(set-traced (,(t-syntax 'locative) ,place)
                                       ',(if (symbol? place) place nil)))
                        places)))))

(define-syntax (untrace . places)
  (cond ((null? places)
         '(untrace-traced-objects))
        (else
         (blockify (map (lambda (place)
                          `(set-untraced (,(t-syntax 'locative) ,place)))
                        places)))))  

;;; Measure consing performed in evaluating an expression.

(DEFINE-SYNTAX (PIG X)
  `(*PIG (LAMBDA () ,X)))

(DEFINE (*PIG X)
  (LET ((BEFORE (POINTER-ADDRESS (HEAP-POINTER))))
    (LET ((VAL (X))
          (AMOUNT (FX- (POINTER-ADDRESS (HEAP-POINTER)) BEFORE)))
      (FORMAT *DEBUG-OUTPUT* "~&;Consed ~S quadwords, ~S (#x~X) bytes~%"
              AMOUNT (POINTER->FIXNUM AMOUNT) (POINTER->FIXNUM AMOUNT))
      VAL)))
