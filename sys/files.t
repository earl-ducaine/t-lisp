(herald files)

;;; Copyright (c) 1983, 1984 Yale University

(define-local-syntax (define-system path built-on . bletch)
  `(*define-system ',path ',built-on (append . ,bletch)))

(define (*define-system path built-on files)
  (let ((id (cadr path))
        (system path))
    (put id 'system system)
    (put id 'built-on built-on)
    (put id 'system-files files)
    id))

(define (get-system sysname)
  (or (get sysname 'system)
      (error "no such system")))

(define (system-name system)     (cadr system))
(define (system-path system)     system)
(define (system-files system)    (get (system-name system) 'system-files))
(define (system-built-on system) (get (system-name system) 'built-on))

;;; $ means that the marked file is compiled pre-cooked.

;;; 68000/Aegis system base.
(DEFINE *AEGIS-BASE-FILES*
  '((TSYS BOOT)                         ; $ Must be first

    (TSYS M68SLINK)                     ; $
    (TSYS M68KERNEL)                    ; $
    (TSYS AEXENO)                       ; $

    (TSYS EARLY)                        ; $
    (TSYS RELOC)                        ; $ Init needs EARLY
    (TSYS CHUNK)                        ; $
    (TSYS POPULATE)                     ;
    (TSYS OPERATION)                    ; Init needs EARLY and CHUNK
    (TSYS OPEN)
    (TSYS PCOMMON)                      ; Init needs OPEN
    (TSYS PRIMOPS)                      ; Init needs PCOMMON

    (TSYS M68PRIMOPS)                   ; Init needs PRIMOPS
    (TSYS M68OPEN)
    (TSYS AEGIS)                        ; Should occur early
    (TSYS AEFILE)                       ; Should occur early
    (TSYS AEFAULT)
    (TSYS AELOAD)
    (TSYS AEGC)                         ; $
    ))

;;; VAX/Un*x system base.
(DEFINE *UNIX-BASE-FILES*
  '((TSYS BOOT)                         ; $ Must be first

    (TSYS VAXSLINK)                     ; $
    (TSYS VAXKERNEL)                    ; $
    (TSYS UNXENO)                       ; $

    (TSYS EARLY)                        ; $
    (TSYS RELOC)                        ; $ Init needs EARLY
    (TSYS CHUNK)                        ; $
    (TSYS POPULATE)                     ; 
    (TSYS OPERATION)                    ; Init needs EARLY and CHUNK
    (TSYS OPEN)
    (TSYS PCOMMON)                      ; Init needs OPEN
    (TSYS PRIMOPS)                      ; Init needs PCOMMON

    (TSYS VAXPRIM)                      ; Init needs PRIMOPS
    (TSYS VAXOPEN)                      ; Init need VAXPRIM
    (TSYS UNIX)                         ; Should occur early
    (TSYS STDIO)
    (TSYS UNFAULT)                      ; Init need PRIMOPS
    (TSYS UNLOAD)
    (TSYS VAXGC)                        ; $
    (TSYS UNGC)                         ; $
    (TSYS UNFILE)
    (TSYS UNTIME)
    ))

;;; VAX/VMS system base.
(DEFINE *VMS-BASE-FILES*
  '((TSYS BOOT)                         ; $ Must be first

    (TSYS VAXSLINK)                     ; $
    (TSYS VAXKERNEL)                    ; $
    (TSYS VMSXENO)                      ; $

    (TSYS EARLY)                        ; $
    (TSYS RELOC)                        ; $ Init needs EARLY
    (TSYS CHUNK)                        ; $
    (TSYS POPULATE)                     ; 
    (TSYS OPERATION)                    ; Init needs EARLY and CHUNK
    (TSYS OPEN)
    (TSYS PCOMMON)                      ; Init needs OPEN
    (TSYS PRIMOPS)                      ; Init needs PCOMMON

    (TSYS VAXPRIM)                      ; Init needs PRIMOPS
    (TSYS VAXOPEN)
    (TSYS VMS)                          ; Should occur early
    (TSYS VMSIO)
    (TSYS VMSFAULT)                     ; !!
    (TSYS VMSLOAD)
    (TSYS VAXGC)                        ; $
    (TSYS VMSGC)                        ; $
    ))

;;; T virtual machine.

(DEFINE *VM-SYSTEM-FILES*
  '((TSYS VMWRITE)                      ; $
    (TSYS VMREPL)                       ; $
    (TSYS SIGNAL)                       ; $ ASAP following VM system.
    (TSYS LIST1)                        ; $ INITIALIZE-OS-STUFF needs
    (TSYS THROW)                        ; defines BIND-HANDLER
    (TSYS FAULT)
    (TSYS FRAME)                        ; GC and THROW need
    (TSYS GC)                           ; $
    (TSYS GCTOP)
    (TSYS VMSYSTEM)
    ))

;;; The various VM systems.

(DEFINE-SYSTEM (TSYS AVM) ()
  *AEGIS-BASE-FILES* *VM-SYSTEM-FILES*)

(DEFINE-SYSTEM (TSYS UVM) ()
  *UNIX-BASE-FILES*  *VM-SYSTEM-FILES*)

(DEFINE-SYSTEM (TSYS VVM) ()
  *VMS-BASE-FILES*   *VM-SYSTEM-FILES*)

;;; *T-SYSTEM-FILES* forms the bulk of the "high-level" T system.

(DEFINE *T-SYSTEM-FILES*
  '((TSYS VECTOR)
    (TSYS CHARACTER)
    (TSYS STRING)
    (TSYS HASH)                         ; win win
    (TSYS MISC)                         ; defines WALK
    (TSYS STRUCT)                       ; MAKE-STYPE calls WALK
    (TSYS POOL)                         ; Init needs POPULATE, STRUCT, MISC
    (TSYS CARCDR)
    (TSYS LIST2)
    (TSYS TREE)
    ;; ALIASES should follow definition of any procedure that has an alias.
    (TSYS ALIASES)
    (TSYS PROPERTY)
    (TSYS TABLE)                        ; Precedes FS
    (TSYS RATIO)                        ; Precede DISPATCH
    (TSYS ARITH)                        ; Precede BIGNUM & DISPATCH
    (TSYS BIGNUM)                       ; Follows ARITH, precedes DISPATCH
    (TSYS DISPATCH)
    (TSYS SYNTAX)
    (TSYS ENV)
    (TSYS LOAD)
    (TSYS EVAL)                         ; Init needs SYNTAX and ERROR
    (TSYS SORT)
    (TSYS DEBUG)
    (TSYS CRAWL)
    (TSYS LAPCONST)                     ; No one needs
;;; I/O
    (TSYS RECOGNIZE)
    (TSYS READTABLE)                    ; Must follow RECOGNIZE.
    (TSYS READ)                         ; Must follow READTABLE.
    (TSYS PRINT)                        ; Must follow READ.
    (TSYS PFLOAT)
    (TSYS FORMAT)
    (TSYS PP)
    (TSYS FS)                           ; Must follow TABLE.
    (TSYS STROPS)
    (TSYS STREAMS)
    (TSYS CHANNEL)                      ; Must follow POPULATE.
    (TSYS STANDARD)                     ; Must follow READ.
    (TSYS REPL)                         ; Init needs POPULATION, ENV, etc.
;;; Macros - all need module SYNTAX
    (TSYS OBJECT)
    (TSYS MACROS)
    (TSYS LET)
    (TSYS COND)
    (TSYS BACKQUOTE)
    (TSYS LOCATION)
    (TSYS DEFSTRUCT)
;;; Initialization
    (TSYS EXPORT)
    (TSYS OBSOLETE)
    (TSYS EXPORTS)                      ; Must follow EXPORT.
    (TSYS TSYSTEM)                      ; Must come last!
    ))

;;; The various full-fledged T systems.

(DEFINE-SYSTEM (TSYS AT) (TSYS AVM) *T-SYSTEM-FILES*)

(DEFINE-SYSTEM (TSYS UT) (TSYS UVM) *T-SYSTEM-FILES*)

(DEFINE-SYSTEM (TSYS VT) (TSYS VVM) *T-SYSTEM-FILES*)

;;; Compiler files.

(DEFINE *TC-FILES*
  '((TCOMP TCENV)
    (TCOMP TCTOP)
    (TCOMP MACHAX)
    (TCOMP NODE)
    (TCOMP DEFS)
    (TCOMP RANDOM)
    (TCOMP SETS)
    (TCOMP ENVINIT)
    (TCOMP TRANSDUCE)
    (TCOMP COMPILE)
    (TCOMP ALPHATIZE)
    (TCOMP CMACROS)
    (TCOMP ANALYZE)
    (TCOMP OPTIMIZE)
    (TCOMP SUBST)
    (TCOMP COPY)
    (TCOMP STRATEGY)
    (TCOMP ANNOTATE)
    (TCOMP CLOSURE)
    (TCOMP TARGETIZE)
    (TCOMP PACK)
    (TCOMP GENERATE)
    (TCOMP GMAX)
    (TCOMP COMMON)
    (TCOMP EMIT)
    (TCOMP OUTPUT)
    (TCOMP LAP)
    (TCOMP TCSYSTEM)))

(DEFINE *M68-TC-FILES*
  '((TCOMP M68GEN)
    (TCOMP M68FOO)
    (TCOMP M68EMIT)
    (TCOMP M68SUP)))

(DEFINE *VAX-TC-FILES*
  '((TCOMP VAXGEN)
    (TCOMP VAXEMIT)
    (TCOMP VAXSUP)))

(DEFINE *PYRAMID-TC-FILES*
  '((TCOMP PYRGEN)
    (TCOMP PYREMIT)
    (TCOMP PYRSUP)))

(DEFINE-SYSTEM (TCOMP ATC) (TSYS AT) *TC-FILES* *M68-TC-FILES*)

(DEFINE-SYSTEM (TCOMP UTC) (TSYS UT)
  *TC-FILES* *VAX-TC-FILES* '((TCOMP UNEMIT)))

(DEFINE-SYSTEM (TCOMP VTC) (TSYS VT)
  *TC-FILES* *VAX-TC-FILES* '((TCOMP VMSEMIT)))

;;; Random auxiliaries.

;;; Returns list of systems in the order in which they should be initialized...

(define (embedded-systems system)
  (do ((s system (system-built-on s))
       (l '() (cons s l)))
      ((null? s) l)))

(define (all-system-files system)
  (apply append
         (map system-files (embedded-systems system))))

;;; Tell which files need to be recompiled.

(define (check sysname)
  (walk check1 (embedded-systems (get-system sysname)))
  t)

(define (check1 system)
  (let ((system (if (symbol? system) (get-system system) system)))
    (walk (lambda (path)
            (maybe-recompile (->filename path) *support-extension* nil))
          (system-files system))))

;;; Compile all files needing compilation.

(define (comall sysname)
  (walk comall1 (embedded-systems (get-system sysname)))
  t)

(define (comall1 system)
  (let ((system (if (symbol? system) (get-system system) system)))
    (walk (lambda (path)
            (maybe-recompile (->filename path) *support-extension* t))
          (system-files system))))


;;; MAYBE-RECOMPILE returns target filename.

;;; WHICH should be one of, e.g., *LAP-EXTENSION* or *SUPPORT-EXTENSION*.
;;; This returns either NIL, if the desired file doesn't exist at all,
;;; or else a filename for the desired file, possibly freshly compiled.

(DEFINE (MAYBE-RECOMPILE FNAME WHICH STUFF)
  (LET ((SOURCE (FILENAME-WITH-TYPE FNAME *SOURCE-EXTENSION*))
        (TARGET (FILENAME-WITH-TYPE FNAME WHICH)))
    (LET ((SOURCE? (PROBE-GENERATION SOURCE))
          (TARGET? (PROBE-GENERATION TARGET)))
      (COND ((AND (NOT SOURCE?) (NOT TARGET?))
             (FORMAT (TERMINAL-OUTPUT)
                     "~%** File ~A doesn't exist."
                     (FILENAME->STRING SOURCE)))
            ((AND SOURCE? (NOT TARGET?))
             (FORMAT (TERMINAL-OUTPUT)
                     "~%** There is no ~A file for ~A.~%"
                     WHICH
                     (FILENAME->STRING SOURCE?))
             (ASK-ABOUT-RECOMPILING FNAME WHICH "** Compile it?" STUFF))
            ((AND SOURCE? TARGET?
                  (FX> (FILENAME-GENERATION SOURCE?)
                       (FILENAME-GENERATION TARGET?)))
             (FORMAT (TERMINAL-OUTPUT)
                     "~%** ~A file for ~A is not up to date.~%"
                     WHICH
                     (FILENAME->STRING SOURCE?))
             (ASK-ABOUT-RECOMPILING FNAME WHICH "** Recompile?" STUFF))))
    TARGET))

;;; STUFF should be either empty, true or false.

(DEFINE (ASK-ABOUT-RECOMPILING FNAME WHICH PROMPT STUFF)
  (COND ((IF (EQ? *EMPTY* STUFF) (Y-OR-N-P PROMPT) STUFF)
         ((IF *BATCH-MODE?* BATCH-COMPILE-FILE COMPILE-FILE) FNAME))))

(LSET *BATCH-MODE?* NIL)

;;; A possible replacement for the above?

(comment
(define (compilation-state path)
  (let ((source (make-filename nil (car path) (cadr path)
                                 *source-extension*))   ; ?
        (target (make-filename nil (car path) (cadr path)
                                 *support-extension*))) ; ??
    (let ((source? (file-exists? source))
          (target? (file-exists? target)))
      (cond (source?
             (cond (target?
                    (cond ((file-newer? source target)
                           'needs-recompiling)
                          (else
                           'compiled)))
                   (else
                    'not-compiled)))
            (else
             (cond (target?
                    'no-source)
                   (else
                    'no-files)))))))
)

;;; Write a file which will load a system into the VM.

(define (zloader sysname)
  (let* ((system (get-system sysname))
         (syspath (system-path system))
         (ns (make-filename nil (car syspath) (cadr syspath) 'zload)))
    (with-open-files ((stream ns '(out)))
      (format stream ";;; Load system ~S~2%;;; (zload ~S *the-boot-env*)~2%"
              sysname (filename->string ns))
      (walk (lambda (path)
              (format stream "(zload-bin ~S *the-boot-env*)~%"
                      (filename->string (make-filename nil
                                                       (car path)
                                                       (cadr path)
                                                       'bin))))
            (system-files system))
      ns)))
