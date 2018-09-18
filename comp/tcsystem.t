(HERALD (TCOMP TCSYSTEM T 76))

;;; Copyright (c) 1983, 1984 Yale University

(LSET *ENABLE-LAP-COMMENTARY?* NIL)      ; !!

;; Debugging switches
(LSET *REANALYZE* 'ONCE)
(LSET *TN-BREAK* NIL)
(LSET *PREFTN-TRACE* NIL)
(LSET *REMTN-TRACE* NIL)
(LSET *SORT-TN-TRACE* NIL)
(LSET *ENTITY-DEBUG?* NIL)
(LSET *RECORD-TN-CONFLICTS?* NIL)        ;?!
(LSET *DEFINE-BREAK-IN* '())

(LSET *TRACE-OPTIMIZERS?* T)

(LSET *PROBE?* T)
(LSET *NOISY?* T)
(LSET *TESTING?* NIL)
(LSET *COMPILER-VERSION* 0)
(LSET *COMPILE-TPROCS-EARLY?* NIL)

(LSET *NODE-POOL* (MAKE-POOL '*NODE-POOL* CREATE-NODE))
(LSET *POOL?* T)

(LSET *NULL-OBJECT* (CREATE-CONSTANT '()))
(LSET *NON-NULL-OBJECT* (CREATE-CONSTANT '*NON-NULL-OBJECT*))

(DEFINE (HACK-GLOBALS)
  (SET *STAT-COUNTERS* (REVERSE! *STAT-COUNTERS*))
  (WALK (LAMBDA (K) (SET (CONTENTS K) '*UNDEFINED*)) *COMPILATION-GLOBALS*))

;;; Obtain needed T system internals

(WALK (LAMBDA (X)
        (*DEFINE *TC-ENV* X (*VALUE *T-IMPLEMENTATION-ENV* X)))
      '(SYSTEM-GENERATION-NUMBER
        SET-SYSTEM-GENERATION-NUMBER
        INITIALIZE-SYSTEM
	REINITIALIZE-SYSTEM
        PRINT-SYSTEM-HERALD
        LOAD-QUIETLY
        FIX-FILE-NAME
        *THE-T-SYSTEM*
        THE-T-SYSTEM-DIRECTORY
        THE-INIT-FILE-DIRECTORY
        MAJOR-VERSION
        MINOR-VERSION
        PROCESSOR-TYPE                  ; ?
        OS-TYPE
        LOCAL-OS
        FLONUM-GUTS
        *TAB-STOP-INTERVAL*
        ARGLIST->ARGSPECTRUM
        FILENAME-WITH-TYPE
        FILENAME-GENERATION
        PROBE-GENERATION
        ENVIRONMENT?
        CEILING
        CHAR+
        make-aegis-fs                   ; for sysgen
        make-unix-fs
        make-vms-fs
        gen-id-count                    ; for TRANSDUCE
        expand-destructure              ; for ALPHATIZE
	assemble
	atom-expander
	*true-object*
        ))  

(DEFINE (EXPORT-TC ENV)
  (WALK (LAMBDA (SYM)
          (*DEFINE ENV SYM (*VALUE *TC-ENV* SYM)))
        '(TEST-COMPILE
          MAKE-MACRO-EXPANDER    ; Foo
          COMFILE
          COMPILE-FILE
          MAYBE-COMPILE-FILE
          TC-SYNTAX-TABLE
          TC-MACRO-DEFINITION-ENV
          *STANDARD-SUPPORT-ENV*
          MAKE-EMPTY-SUPPORT-ENV
          MAKE-SUPPORT-ENV
	  SUPPORT-ENV-ENTRY
          LOAD-SUPPORT
          *DEFINE-SUPPORT-ENV)))

(LSET *VERSION-BLURB* "")

(DEFINE *THE-TC-SYSTEM*
  (LET ((GEN 0))
    (OBJECT NIL
            ((SYSTEM-GENERATION-NUMBER SELF) GEN)
            ((SET-SYSTEM-GENERATION-NUMBER SELF N) (SET GEN N))
            ((INITIALIZE-SYSTEM SELF)
             (HACK-GLOBALS)
             (SET *VERSION-BLURB*
                  (FORMAT NIL "TC version ~S.~S (~S)"
                          (MAJOR-VERSION *TC-VERSION-NUMBER*)
                          (MINOR-VERSION *TC-VERSION-NUMBER*)
                          GEN))
             (LOAD-QUIETLY (FIX-FILE-NAME (THE-T-SYSTEM-DIRECTORY) 'TC GEN)
                           *TC-ENV*)
             (LOAD-QUIETLY (MAKE-FILENAME NIL (THE-INIT-FILE-DIRECTORY)
                                          'TCINIT NIL)
                           *SCRATCH-ENV*))
            ((REINITIALIZE-SYSTEM SELF)
             (LOAD-QUIETLY (FIX-FILE-NAME (THE-T-SYSTEM-DIRECTORY) 'TC GEN)
                           *TC-ENV*)
             (LOAD-QUIETLY (MAKE-FILENAME NIL (THE-INIT-FILE-DIRECTORY)
                                          'TCINIT NIL)
                           *SCRATCH-ENV*))
            ((PRINT-SYSTEM-HERALD SELF)
             (FORMAT T '("~&TC ~S.~S (~S) in T ~S.~S (~S) ~A/~A  "
                         "Copyright (C) 1984  Yale University~%")
                     (MAJOR-VERSION *TC-VERSION-NUMBER*)
                     (MINOR-VERSION *TC-VERSION-NUMBER*)
                     GEN                ; "edit number"
                     (MAJOR-VERSION *T-VERSION-NUMBER*)
                     (MINOR-VERSION *T-VERSION-NUMBER*)
                     (SYSTEM-GENERATION-NUMBER *THE-T-SYSTEM*)
                     (PROCESSOR-TYPE (LOCAL-PROCESSOR))
                     (OS-TYPE (LOCAL-OS)))
             *REPL-WONT-PRINT*)
            (((*VALUE *T-IMPLEMENTATION-ENV* 'SYSTEM-NAME) SELF) 'TC)
            ((IDENTIFICATION SELF) '*THE-TC-SYSTEM*))))

(DEFINE *TC-VERSION-NUMBER* 1004)

(PUSH (*VALUE *T-IMPLEMENTATION-ENV* '*EMBEDDED-SYSTEMS*) *THE-TC-SYSTEM*)

;;; Default syntax table used for compilation.

(LSET *TC-SYNTAX-TABLE* (ENV-SYNTAX-TABLE *SCRATCH-ENV*))
(LSET *SYNTAX-TABLE* *TC-SYNTAX-TABLE*)

(DEFINE TC-SYNTAX-TABLE
  (OBJECT (LAMBDA () *TC-SYNTAX-TABLE*)
          ((SETTER SELF)
           (LAMBDA (VAL)
             (SET *TC-SYNTAX-TABLE* VAL)))))

;;; Environment in which to evaluate DEFINE-LOCAL-SYNTAX bodies.

(LSET *TC-MACRO-DEFINITION-ENV* *SCRATCH-ENV*)

(DEFINE TC-MACRO-DEFINITION-ENV
  (OBJECT (LAMBDA () *TC-MACRO-DEFINITION-ENV*)
          ((SETTER SELF)
           (LAMBDA (VAL)
             (SET *TC-MACRO-DEFINITION-ENV*
                  (CHECK-ARG ENVIRONMENT? VAL
                             TC-MACRO-DEFINITION-ENV))))))

;;; Export.  This should come last.

(EXPORT-TC *SYSTEM-ENV*)
