(herald (tcomp sysgen t)
        (env tcomp))

;;; Copyright (c) 1983, 1984 Yale University

;;; System generation utility functions.

;;; The procedures *MAKE-BASE and friends scan support files and
;;; construct, as an assembler file, any symbols and value cells which
;;; are required by "pre-cooked" modules.

;;; Things of interest to this program appear in support files as lists
;;; of the form (SYSBUILD-ITEM . item), where "item" is one of the
;;; following:
;;;   (STATIC symbol)
;;;         "_v$symbol" is to be the tag for the "symbol"'s static value
;;;         cell.
;;;   (VALUE symbol expression)
;;;         The symbol gets as initial value the thing denoted by the
;;;         assembly expression.
;;;   (UNIT tag)
;;;         "tag" is a tag for a "unit," the in-core equivalent of an object
;;;         file.  A slot in the unit points to a "setup procedure" which
;;;         when run does the file's initializations, i.e. top level forms
;;;         (DEFINEs, etc.).  At system startup time, this code gets run.
;;;   ... there are some others ...

;;; Utilities

(define (system-base-path system)
  (do ((b system (system-built-on b))
       (z nil b))
      ((null? b)
       (let ((path (system-path z)))
         `(,(car path) ,(symbolconc (cadr path) 'base))))))

(define (filestring fs dir name type)
  (filename->string (make-filename fs dir name type)))

;;; MAKE-SYSTEM

(define (make-system system-name)       ; E.g. (MAKE-SYSTEM 'AT)
  (let ((system (get-system system-name)))
    (make-system-aux system
                     (system-path system)
                     't
                     '*make-system
                     '((make-system)))))

(define (make-base system-name)
  (let ((system (get-system system-name)))
    (cond ((system-built-on system)
           (format (terminal-output)
                   "~&Can't MAKE-BASE embedded system ~S~%"
                   system-name))
          (else
           (let ((b (system-base-path system)))
             (make-system-aux system
                              b
                              (cadr b)
                              '*make-base
                              '((make-base))))))))

(define (make-system-aux system path unit-id procname herald-stuff)
  (let ((fname (make-filename nil (car path) (cadr path) 't)))
    (with-open-files ((stream fname '(out)))
      (format stream
              "~S~%~S~%~S~%"
              `(herald ,unit-id (pre-cook) ,@herald-stuff)
	      '(define-local-syntax (ceval x) (eval x *tc-env*))
              `(ceval (,procname ',(system-name system)))))
    (comfile fname)))

;;; Write the initial units vector, and a bind script for the system.

(define (*make-system system-name)
  (let* ((system (get-system system-name))
         (sys-path (system-path system))
         (paths `(,@(all-system-files system)
                  ,(system-base-path system)
                  ,sys-path)))
    ;; This vector has things to run to initialize the system.
    (output-vector 'the-initial-units
                   (+map (lambda (path)
                           (let ((tag
                                  (cond ((eq? path sys-path)
                                         *the-unit-0-tag*)
                                        (t
                                         (symbolconc 'd- (cadr path))))))
                             (if (neq? path sys-path) (emit `(external ,tag)))
                             `(+ ,tag %%extend-tag)))
                         paths)
                   "Object files comprising system")
    (funcall (get *target-system* 'write-bind-script)
             system
             paths)
    ;; Be sure to coordinate this with PROCESS-SPECIAL-ITEMS!
    `(block (define *generation-numbers*
              ',(let ((n (* #x+f00bad 2))) (list n n n n n n n n)))
            (define *system-build-time*
              "!============================================================!")
            )))

(lset *the-symbols* nil)
(lset *the-source-file* nil)
(lset *sysgen-end-of-file* nil)

;;; Scan support files for pre-cooked files (the "base"), and emit symbols,
;;; pnames, and value cells.  Also write a vector of initial symbols.

(define (*make-base system-name)
  (let ((system (get-system system-name)))
    (begin-aligned-impure-datum)
    (walk (lambda (u) (emit `(external ,u)))
          '(symbol-template vcell-template))
    (bind ((*the-symbols* '())
           (*the-source-file* nil))     ; Bind here for PROCESS-SPECIAL-ITEMS
      (unwind-protect
       (block
        (process-special-items (cadr (system-path system))
                               (cadr (system-base-path system)))
        (walk (lambda (path)
                (set *the-source-file* path)
                (let* ((fname (make-filename nil
                                             (car path)
                                             (cadr path)
                                             *support-extension*))
                       (gen (probe-generation fname)))
                  (cond (gen
                         (format *noise-output* ";File ~A~%"
                                 (filename->string gen))
                         (with-open-files ((stream fname '(in)))
                           (catch eof
                             (bind ((*sysgen-end-of-file* eof))
                               (do ((item (get-next-item stream)
                                          (get-next-item stream)))
                                   ((null? item))
                                 (process-item item))))))
                        (else
                         (format *noise-output* ";File ~A (no support file)~%"
                                 (filename->string fname))))))
              (system-files system))
        (newline *noise-output*)
        (set *the-symbols* (reverse! *the-symbols*))
        ;; Spit out symbols, pnames, value cells, and vector of all symbols.
        (output-vector 'the-initial-symbols
                       (map output-symbol *the-symbols*)
                       "vector of initial symbols")
        )                               ;Close PROGN
       ;; Unwind-forms:
       (walk clean-symbol *the-symbols*)
       ))
    (text-section)
    0))

(define (get-next-item stream)
  (let ((form (+read stream)))
    (cond ((eof? form) nil)
          ((and (pair? form) (eq? (car form) 'sysbuild-item))
           (cdr form))
          (else (get-next-item stream)))))

(lset *sysgen-verbose?* nil)  ;; '(ITEMS SYMBOLS)

;;; Use LIST instead of QUOTE to avoid TC/RELOC bignum bug.

(put 'nonvalue-hack 'sym
     (list '+ '(- %%misc-tag %%extend-tag) #x+80000000))

(define (process-special-items . syms)
  (process-item '(value *nonvalue-hack* nonvalue-hack))
  (process-item '(static *generation-numbers*))
  (process-item '(static *system-build-time*))
  (process-item '(symbol defined-initially))
  (process-item '(symbol sysgen))
  (process-item '(value *the-initial-units*   the-initial-units))
  (process-item '(value *the-initial-symbols* the-initial-symbols))
  (walk (lambda (sym)
          (process-item `(symbol ,sym)))
        syms)
  (emit '(external the-initial-units)))

(define (clean-symbol symbol)
  (cond ((remprop symbol 'required)
         (remprop symbol 'cell-needed)
         (remprop symbol 'initial-value)
         (remprop symbol 'defined)
         (remprop symbol 'referenced)
         (remprop symbol 'set))))

(define *sample-aegis-fs* (make-aegis-fs '(sample)))

(defun (apollo write-bind-script) (system paths)
  (let* ((sys-path (system-path system))
         (sys-ns (filestring *sample-aegis-fs*
                             (car sys-path)
                             (cadr sys-path)
                             nil))
         (script-fname (make-filename nil
                                     (car sys-path)
                                     (cadr sys-path)
                                     'sh)))
    (with-open-files ((make script-fname '(out)))
      (format *noise-output* ";Writing ~A~%" (filename->string script-fname))
      (format make
              "# Bind script for system ~A~%~~tsys/adm/newgen "
              sys-path)
      (walk (lambda (s)
              (format make "~A "
                      (let ((p (system-path s)))
                        (filestring *sample-aegis-fs*
                                    (car p) (cadr p) nil))))
            (embedded-systems system))
      (format make
              "~%~~tsys/adm/safe_asm ~A~%"
              sys-ns)
      (let ((x (symbolconc 'x (cadr sys-path))))
        (format make "von~%bind -b ~A -map >~A - <<!~%"
                (filestring *sample-aegis-fs* (car sys-path) x nil)
                (filestring *sample-aegis-fs* (car sys-path) x 'map)))
      (walk (lambda (path)
              (writes make (filestring *sample-aegis-fs*
                                       (car path) (cadr path) 'bin))
              (newline make))
            paths)
      (format make
              '("~~tsys/assist.bin~%"
                "~~tsys/ttop.bin~%"
                "~~tsys/ftoa.bin~%"
                "~~tsys/ndb~%"
                "-und~%-end~%!~%")))))

(define *sample-unix-fs* (make-unix-fs '(sample)))

;;; Write the makefile for the Unix "make" utility.  What a loss.

(defun (unix write-bind-script) (system paths)
  (let* ((sys-path (system-path system))
         (script-fname (make-filename nil
                                      (car sys-path)
                                      (cadr sys-path)
                                      'makefile)))
    (with-open-files ((make script-fname '(out)))
      (format *noise-output* "~&;Writing Makefile ~A~%"
              (filename->string script-fname))
      (format make '("# Makefile for ~s~2%"
                     "as = /t/bin/tas               # Assembler~%"
                     "NEWGEN = /t/adm/newgen~%")
              sys-path)
      (format make '("TSYS = /t/t2.9/sys~%"
                     "TCOMP = .~%"))    ; Losing MAKE blows out if expanded
      (format make "OBJFILES = ")
      (walk (lambda (path)
              (if (fx> (hpos make) 60.)
                  (format make "\\~%~A" #\tab))
              (format make "~A.o "
                      (grossly-losing-unix-filestring path)))
            `(,@paths
              (tsys unassist)))
      (format make "/lib/crt0.o")
      (newline make)
      (let ((sysname (filestring *sample-unix-fs* nil (cadr sys-path) nil))
            (gross (grossly-losing-unix-filestring sys-path)))
        (format make
                '("SYSTEM = x~A~2%"
                  "$(SYSTEM): Makefile~%"
                  "~A$(NEWGEN) ")
                sysname #\tab)
        (walk (lambda (s)
                (format make "~A "
                        (grossly-losing-unix-filestring (system-path s))))
              (embedded-systems system))
        (newline make)
        (format make
                '("~A$(as) -o ~A.o ~A.s~%"
		  ;; -ljobs should precede -lm on 4.1.
                  "~Ald -x $(OBJFILES) -ltermcap -lm -lc -e start~%"
                  "~Amv a.out $(SYSTEM)~%")
                #\tab gross gross  #\tab  #\tab
                )))))

;;; Create a filename in MAKE's special syntax.

(define (grossly-losing-unix-filestring path)
  (format nil "$(~A)/~A"
          (car path)
          (string-downcase (symbol-pname (cadr path)))))

;;; For VMS, write a ".OPT" file for LINK, and hack other stuff manually with
;;; some external command file.

(defun (vms write-bind-script) (system paths)
  (let* ((sys-path (system-path system))
         (script-fname (make-filename nil
                                      (car sys-path)
                                      (cadr sys-path)
                                      'opt)))
    (with-open-files ((make script-fname '(out)))
      (format *noise-output*
              "~2&;Writing ~A~%"
              (filename->string script-fname))
      (walk (lambda (path)
              (format make "~A:~A,-~%" (car path) (cadr path)))
            paths)
      (format make "TSYS:VSTART,-~%")
      (format make "TSYS:VASSISTIO,-~%")
      (format make "TSYS:VASSIST~%"))))

;;; Main dispatch for processing an "item".

(define (process-item item)
  (if (memq 'items *sysgen-verbose?*)
      (format *noise-output* ";  Item: ~:S~%" item))
  (case (car item)
    ((end)
     (*sysgen-end-of-file* t))
    ((static)                           ;(STATIC symbol reftype)
     (require-symbol (cadr item) (caddr item))
     (put (cadr item) 'cell-needed t))
    ((symbol)                           ;(SYMBOL symbol)
     (require-symbol (cadr item) 'symbol))
    ((value)                            ;(VALUE symbol value)
     (require-symbol (cadr item) 'setq)
     (put (cadr item) 'cell-needed t)
     (let ((oldval (get (cadr item) 'initial-value))
           (newval (caddr item)))
       (cond ((and oldval (not (alikev? oldval newval)))
              (warn "initial heap value of ~S is being changed from ~S to ~S"
                    "will use the new value"
                    (cadr item) oldval newval))
             (oldval
              (mention "duplicate INITIAL-VALUE for ~S (value = ~S)"
                       "none"
                       (cadr item) oldval)))
       (if (and *the-source-file*       ; This is horrible
                (symbol? (meval-lap-offset newval)))
           (emit `(external ,newval)))
       (put (cadr item) 'initial-value newval)))
    ((unit source-file) nil)                 ; Obsolete & ignorable.
    (else (warn "illegal SYSGEN item: ~S" "will ignore it" item))
    ))

;;; REFTYPE is one of REF, FNREF, SETQ, DEF, SYMBOL, or NIL.

(define (require-symbol symbol reftype)
  (cond ((not (get symbol 'required))
         (push *the-symbols* symbol)
         (put symbol 'required t)))
  (case reftype
    ((symbol #f) nil)
    ((ref fnref) (put symbol 'referenced *the-source-file*))
    ((def)
     (if (get symbol 'set)
         (format *noise-output*
                 (case (get symbol 'set)
                   ((setq) ";  ~S is SET and later DEFINEd~%")
                   ((def)  ";  ~S is DEFINEd twice~%"))
                 symbol))
     (put symbol 'set 'def))
    ((setq lset)
     (if (eq? (get symbol 'set) 'def)
         (format *noise-output* ";  ~S is DEFINEd and later SET~%" symbol)
       (put symbol 'set 'setq)))))

(define *defined-special-tag* (make-special-tag 's 'defined-initially))

(define (output-symbol symbol)
  (let ((tag (make-special-tag 's symbol))
        (pntag (make-special-tag 'pn symbol))
        (celltag (cond ((get symbol 'cell-needed) (make-special-tag 'v symbol))
                       (t nil))))
    ;; Emit the symbol
    (if (memq 'symbols *sysgen-verbose?*)
        (format *noise-output* ";  Symbol: ~:S~%" symbol))
    (begin-aligned-impure-datum)
    (emit-to-unit '(address symbol-template))   ; word 0
    (emit `(globl ,tag))
    (emittag tag)
    ;; The following must match the slot offset definitions in PRODUCE.
    (emit-to-unit `(address ,(or celltag 'null)))       ; word 1
    (emit-to-unit `(address ,pntag))                    ; word 2
    (emit-to-unit `(address null))                      ; word 3 - plist
    ;; Emit print name
    (output-string pntag (symbol-pname symbol))         ;Pname
    ;; Emit value cell, if any
    (cond (celltag
           (let ((val (get symbol 'initial-value))
                 (setp (get symbol 'set)))
             (cond (val
                    (if (memq 'symbols *sysgen-verbose?*)
                        (format *noise-output* "  Value: ~:S" val))))
;            (EMITREMARK "Value cell")
             (begin-aligned-impure-datum)
             (emit-to-unit '(address vcell-template))   ; word 0
             (emit `(globl ,celltag))
             (emittag celltag)
             (emit-to-unit `(address (rel ,(or val
                                               `(+ ,celltag nonvalue-hack)))))
             (emit-to-unit `(address ,tag))             ; word 2
             (emit-to-unit `(address ,(if (eq? setp 'def)
                                          *defined-special-tag*
                                        'null)))
             )))
    tag))

;;; Construct a (pure) vector from a list of things.

(define (output-vector tag things remark)
  (let ((len (length things)))
    (format *noise-output* ";Vector: ~S (~D element~P)~%"
            tag len len)
    (emitremark remark)
    (begin-aligned-impure-datum)        ; [68000 wants impure.]
    (emit-to-unit '(peso (* 4 8)))
    (emit-to-unit `(peso ,(lsh len *shift*)))
    (emit-vector-template)
    (emit `(globl ,tag))
    (emittag tag)
    (walk (lambda (thing) (emit-to-unit `(address ,thing))) things)
    (text-section)))
