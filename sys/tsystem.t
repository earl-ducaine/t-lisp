(HERALD (TSYS TSYSTEM T 142)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; T system startup file

(DEFINE (COMMAND-LINE) *COMMAND-LINE*)  ; Where does this belong?

(DEFINE PRINT-ENV-WARNINGS?
  (OBJECT (LAMBDA () *PRINT-ENV-WARNINGS?*)     ; See ENV.T.
          ((SETTER SELF)
           (LAMBDA (VAL)
             (SET *PRINT-ENV-WARNINGS?* VAL)))))

(DEFINE GC-NOISILY?
  (OBJECT (LAMBDA () *GC-NOISILY?*)
          ((SETTER SELF)
           (LAMBDA (VAL)
             (SET *GC-NOISILY?* VAL)))))

;;; System intialization stuff

(DEFINE (MAJOR-VERSION VERSION)
  (FX/ VERSION 1000))

(DEFINE (MINOR-VERSION VERSION)
  (FIXNUM-REMAINDER VERSION 1000))

(DEFINE (PRINT-STANDARD-HERALD WHAT VERSION GEN . MAYBE-NOTE)
  (FORMAT T "~&~A ~S.~S (~S) ~A/~A  ~A~%"
          WHAT
          (MAJOR-VERSION VERSION)
          (MINOR-VERSION VERSION)
          GEN                           ; "edit number"
          (PROCESSOR-TYPE *LOCAL-PROCESSOR*)
          (OS-TYPE *LOCAL-OS*)
          (IF (NULL? MAYBE-NOTE) "" (CAR MAYBE-NOTE)))
  *REPL-WONT-PRINT*)

;;; Fix file name is "<system-name>FIX<edit-number>.T" in the system directory.

(DEFINE (FIX-FILE-NAME DIR PREFIX GEN)
  (MAKE-FILENAME NIL DIR (CONCATENATE-SYMBOL PREFIX 'FIX GEN) NIL))

(DEFINE *THE-T-SYSTEM*
  (LET ((GEN 0))
    (OBJECT NIL
            ((SYSTEM-GENERATION-NUMBER SELF) GEN)
            ((SET-SYSTEM-GENERATION-NUMBER SELF N) (SET GEN N))
            ((INITIALIZE-SYSTEM SELF)
             (BACKPATCH-INITIAL-UNITS)
             (LOAD-QUIETLY (FIX-FILE-NAME (THE-T-SYSTEM-DIRECTORY) 'T GEN)
                           *T-IMPLEMENTATION-ENV*)
             (LOAD-QUIETLY (MAKE-FILENAME NIL (THE-INIT-FILE-DIRECTORY)
                                          'INIT NIL)
                           *SCRATCH-ENV*))
            ((REINITIALIZE-SYSTEM SELF)
             (REINITIALIZE-STREAMS)     ; set up standard input and output
             (INITIALIZE-OS-STUFF)
             (CREATE-LOCAL-FS)
             (LOAD-QUIETLY (FIX-FILE-NAME (THE-T-SYSTEM-DIRECTORY) 'T GEN)
                           *T-IMPLEMENTATION-ENV*)
             (LOAD-QUIETLY (MAKE-FILENAME NIL (THE-INIT-FILE-DIRECTORY)
                                          'INIT NIL)
                           *SCRATCH-ENV*))
            ((PRINT-SYSTEM-HERALD SELF)
             (PRINT-STANDARD-HERALD 'T *T-VERSION-NUMBER* GEN
                                    "Copyright (C) 1984 Yale University"))
            ((SYSTEM-NAME SELF) 'T)
            ((IDENTIFICATION SELF) '*THE-T-SYSTEM*))))

(PUSH *EMBEDDED-SYSTEMS* *THE-T-SYSTEM*)

(DEFINE *T-VERSION-NUMBER* 2009)

(SET *TOP-LEVEL*
     (LAMBDA ()
       (SET *TOP-LEVEL* *STANDARD-TOP-LEVEL*)
       (INITIALIZE-SYSTEMS)
       (BREAKPOINT)))

(DEFINE (BACKPATCH-INITIAL-UNITS)       ; Gross hack
  (WALK-VECTOR (LAMBDA (UNIT)
                 (COND ((OR (NULL? (UNIT-ENV UNIT))
                            (EQ? (UNIT-ENV UNIT) *SYSTEM-BOOT-ENV*))
                        (SET (UNIT-ENV UNIT) *T-IMPLEMENTATION-ENV*)))
                 (MAYBE-ADJUST-SOURCE-FILENAME UNIT))
               *THE-INITIAL-UNITS*))

(DEFINE (SYSTEM-LOADED? NAME)
  (ANY? (LAMBDA (SYSTEM)
          (EQ? (SYSTEM-NAME SYSTEM) NAME))
        *EMBEDDED-SYSTEMS*))

;;; Environment initialization.

;;; Make a "base environment," i.e., a fresh environment which has copies of
;;; all the "released" system variables in it.

(DEFINE (MAKE-STANDARD-ENV)
  (LET ((STANDARD-ENV (MAKE-EMPTY-LOCALE '*STANDARD-ENV*)))
    (EXPORT-TSYS STANDARD-ENV)
    (*DEFINE STANDARD-ENV '*STANDARD-ENV* STANDARD-ENV)
    (*DEFINE STANDARD-ENV '*STANDARD-SYNTAX-TABLE*
       (ENV-SYNTAX-TABLE STANDARD-ENV))
    ;; Obsolete name, but documented in manual 3rd ed.
    (*DEFINE STANDARD-ENV '*THE-BASE-ENV* STANDARD-ENV)
    ;; Obsolete name, but used a lot all over the place.
    (*DEFINE STANDARD-ENV '*SYSTEM-ENV* STANDARD-ENV)
    ;; Gross hack to prevent CRAWL from blowing out.  What to do?
    (*LSET STANDARD-ENV '*OBJ* NIL)
    STANDARD-ENV))


;;; Create a user environment inferior to given environment.
;;; The variable *SCRATCH-ENV* will be defined in the base environment to
;;; be the new environment.

(DEFINE (MAKE-SCRATCH-ENV STANDARD-ENV)
  (LET ((SCRATCH-ENV (MAKE-LOCALE STANDARD-ENV '*SCRATCH-ENV*)))
    (*DEFINE STANDARD-ENV '*SCRATCH-ENV* SCRATCH-ENV)
    SCRATCH-ENV))

;;; ---------- Initialize everything.

(INITIALIZE-OS-STUFF)                   ; !

(CREATE-LOCAL-FS)                       ; Needed for booting TC, for example

(SET *Z?* NIL)                          ; Become legitimate.

;;; The following call is expensive.

(DEFINE *STANDARD-ENV* (MAKE-STANDARD-ENV))
(DEFINE *STANDARD-SYNTAX-TABLE* (ENV-SYNTAX-TABLE *STANDARD-ENV*))
(DEFINE *SYSTEM-ENV* *STANDARD-ENV*)        ; Obsolete name

(DEFINE *SCRATCH-ENV* (MAKE-SCRATCH-ENV *STANDARD-ENV*))

(INITIALIZE-REPL *SCRATCH-ENV*)

(SET *THE-BOOT-ENV* *SCRATCH-ENV*)      ; For further embedded systems.

;;; End of basic system initialization sequence.

;;; Control falls from here either into other embedded systems or into
;;; (*TOP-LEVEL*).  The latter will call (INITIALIZE-SYSTEMS).
