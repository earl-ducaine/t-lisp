(herald (tsys fs t 75)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; File systems and filename translation

(define-integrable (local-fs) *local-fs*)

;;; ---------- File system types:

;;; Convert symbol pname to case preferred by file system types.

(define-operation (->preferred-case fstype string)
  (string-downcase string))             ; lower

;;; Called if logical name spec was a string.

(define-operation (massage-dir-string fstype dir)
  dir)

;;; Called if ln spec was a symbol not in logical name table.

(define-operation (massage-ln-string fstype ln)
  (massage-dir-string fstype ln))

(define-operation (massage-gen-part fstype gen)
  (ignore gen)
  "")

;;; Maybe resolve file type.

(define-operation (translate-file-type fstype type)
  nil)                                  ; none

(define-operation (query-fs-names fstype fs))

;;; Parse a filespec string.

(define-operation (parse-filespec fstype fs string)
  (make-filename fs nil string nil))

;;; FS type table (?)

(define *fs-type-table* (make-table '*fs-type-table*))

;;; Aegis

(define aegis-fs?
  (object (lambda (fs) (eq? (fs-type fs) aegis-fs?))
          ((massage-dir-string self ln) ; Called from dir-part
           (string-append ln "/"))
          ((massage-ln-string self ln)
           (string-append "~" (string-downcase ln) "/"))
          ((query-fs-names self fs)
           (or (read-fs-names-from (make-filename fs "~sys/t" 'localfs 't))
               (read-fs-names-from (make-filename fs 'tsystem 'localfs 't))))
          ((print self stream)
           (format stream "#{File-system-type~_AEGIS}"))))

(define (make-aegis-fs names)
  (make-fs aegis-fs? names))

;;; Utility for QUERY-FS-NAMES

(define (read-fs-names-from filename)
  (with-open-streams ((stream (maybe-open filename '(in))))
    (cond ((and stream (graphic? (peekc stream)))
           (let ((probe (read stream)))
             (cond ((pair? probe) probe)
                   (else (list probe)))))
          (else nil))))     ; Assume () = false

;;; UNIX

(define unix-fs?
  (object (lambda (fs) (eq? (fs-type fs) unix-fs?))
          ((massage-dir-string self dir)        ; Called from dir-part
           (string-append dir "/"))
          ((massage-ln-string self ln)
           (string-append "$" ln "/"))
          ((translate-file-type self type)
           (case type
             ((bin) ".o")               ; this may change
             (else nil)))
          ((query-fs-names self fs)
           (or (read-fs-names-from (make-filename fs nil "/etc/sysname" nil))
               (read-fs-names-from (make-filename fs 'tsystem 'localfs 't))))
	  ((parse-filespec self fs string)
	   (ignore fs)
	   (parse-unix-filespec string))
          ((print self stream)
           (format stream "#{File-system-type~_UNIX}"))))

(define (make-unix-fs names)
  (make-fs unix-fs? names))

;;; VMS

(define vms-fs?
  (object (lambda (fs) (eq? (fs-type fs) vms-fs?))
          ((massage-ln-string self ln)
           (string-append ln ":"))
          ((->preferred-case self string) string)
          ((massage-gen-part self gen)
           (cond ((fixnum? gen) (format nil ".~S" gen))
                 ((eq? gen 'newest) ".0")
                 (else nil)))
          ((query-fs-names self fs)
           (read-fs-names-from
            (make-filename fs
                           (the-T-system-directory)
                           'localfs 't)))
          ((print self stream)
           (format stream "#{File-system-type~_VMS}"))))

(define (make-vms-fs names)
  (make-fs vms-fs? names))

;;; Tops-20

(define tops-20-fs?
  (object (lambda (fs) (eq? (fs-type fs) tops-20-fs?))
          ((massage-ln-string self ln)
           (string-append ln ":"))
          ((->preferred-case self string) string)
          ((translate-file-type self type)
           (case type
             ((bin) ".FASL")
             (else nil)))
          ((massage-gen-part self gen)
           (cond ((fixnum? gen) (format nil ".~S" gen))
                 ((eq? gen 'newest) ".0")
                 (else nil)))
          ((print self stream)
           (format stream "#{File-system-type~_TOPS-20}"))))

(define (make-tops-20-fs names)
  (make-fs tops-20-fs? names))

;;; ---------- File systems:

;;; File systems are instantiations of file system types.

(define-operation (fs-type fs))         ; Return an fs-type object for the fs
(define-operation (fs-name fs))         ; Return a symbol naming the fs

(define-settable-operation (logical-name fs ln))
(define set-logical-name (setter logical-name))

(define-settable-operation (fs-maybe-open-proc fs))
(define set-fs-maybe-open-proc (setter fs-maybe-open-proc))

(define-operation (set-fs-name fs newname))

(define-operation (maybe-open-filename fs filename modes))

(define-operation (fs-parse-filespec fs string)
  (make-filename fs nil string nil))

(define-predicate file-system?)

(define (make-fs fstype names)          ; Note internet domain(s) also?
  (let ((name (if (null? names) nil (car names)))
        (access (lambda (fs filename modes)
                  (error "no access to ~S~%  ~S"
                         fs
                         `(open ,filename ,modes)))))
       ;; (fprobe default-fs-probe-file) ...?
    (let ((ln-table (make-table `(logical-names ,name))))
      (let ((fs (object nil
                        ((fs-type self) fstype)
                        ((logical-name self ln)
                         (table-entry ln-table ln))
                        ((set-logical-name self ln def)
                         (set (table-entry ln-table ln) def))
                        ((maybe-open-filename self filename modes)
                         (access self filename modes))
                        ((fs-maybe-open-proc self) access)
                        ((set-fs-maybe-open-proc self val)
                         (set access val))
                        ((fs-name self) name)
                        ((set-fs-name self newname)
                         (cond ((not (memq? newname names))
                                (push names newname)
                                (set (table-entry *fs-table* newname) self)
                                (if (not name) (set name newname)))))
			((fs-parse-filespec self string)
			 (parse-filespec fstype self string))
                        ((file-system? self) t)
                        ((print self stream)
                         (format stream "#{File-system~_~s}" name)))))
        (walk (lambda (name)
                (set (table-entry *fs-table* name) fs))
              names)
        fs))))

(define (probe-generation fname)
  (let ((fname (->filename fname)))
    (let ((gen (local-probe-generation fname)))
      (cond (gen
             (make-filename (local-fs)
                            (filename-dir fname)
                            (filename-name fname)
                            (filename-type fname)
                            gen))
            (else nil)))))

(define (file-exists? fname)
  (local-file-exists? (->filename fname)))

(lset *local-fs* nil)

(define (create-local-fs)
  (let ((fs ((local-fs-creator) '())))
    (set *local-fs* fs)
    (set (fs-maybe-open-proc fs) maybe-open-local)
    (walk (lambda (name)
            (set-fs-name fs name))
          (query-fs-names (fs-type fs) fs))
    fs))

;;; Map file-system names to corresponding file-system-objects.

(define *fs-table* (make-table '*fs-table*))

;;; ---------- MAKE-FILENAME

(define (make-filename fs ln name . optionals)
  (let ((type (car optionals))
        (gen (cadr optionals))
        (cached-string nil))
    (object nil
            ((maybe-open self modes)
             (maybe-open-filename (->fs fs)
                                  self
                                  modes))
            ((filename->string self)
             (or cached-string
                 (set cached-string
                      (let ((z (resolve-logical-name (->fs fs)
                                                     ln
                                                     '())))
                        (fs-filename->string (fs-type (car z))
                                             (car z)
                                             (cdr z)
                                             name
                                             type
                                             gen)))))
            ((filename-fs self) fs)
            ((filename-dir self) ln)
	    ((filename-leaf self)
             (let ((z (resolve-logical-name (->fs fs) ln '())))
               (fs-filename-leaf (fs-type (car z))
		                 (car z)
                                 name
                                 type
                                 gen)))
            ((filename-name self) name)
            ((filename-type self) type)
            ((filename-generation self) gen)
            ((filename? self) t)
            ((print self stream)
             (format stream "#[Filename~_~S~_~S~_~S"
                     (if (file-system? fs) (fs-name fs) fs) ln name)
             (if (or type gen)
                 (format stream "~_~S" type))
             (if gen
                 (format stream "~_~S" gen))
             (writec stream #\]))
	    ((display self stream)
	     (writes stream (filename->string self))))))

(define (->fs thing)
  (cond ((null? thing) (local-fs))
        ((and (symbol? thing) (table-entry *fs-table* thing)))
        ((file-system? thing) thing)
        (else (->fs (error "unknown file system - ~S" thing)))))

(define-predicate filename?)
(define-operation (filename->string filename))
(define-operation (filename-fs   filename))
(define-operation (filename-dir  filename))
(define-operation (filename-leaf filename))
(define-operation (filename-name filename))
(define-operation (filename-type filename))
(define-operation (filename-generation filename))

(define (make-filename-for-read l stream)
  (let* ((l (cdr l))
         (n (length l)))
    (cond ((or (fx< n 3) (fx> n 5))
           (read-error stream "illegal filename syntax - ~S" l))
          (else
           (apply make-filename l)))))

(define (->filename obj)
  (cond ((filename? obj) obj)
        ((string? obj)
	 (fs-parse-filespec (local-fs) obj))
	((symbol? obj)
         (make-filename nil nil obj nil))
        ((and (proper-list? obj)
              (not (null? obj))
	      (not (null? (cdr obj))))
         (apply make-filename nil obj))
        (else
         (->filename (error "can't coerce to filename~%  (~S ~S)"
                            '->filename obj)))))

(define (filespec? obj)
  (or (filename? obj)
      (string? obj)	;should check syntax
      (symbol? obj)
      (and (proper-list? obj)
	   (not (null? (cdr obj)))	;(cdr '()) => ()
	   (destructure (((dir name type gen . z) obj))	;(cdr '()) => ()
	     (and (null? z)
		  (or (null? dir) (symbol? dir) (string? dir))
		  (or (symbol? name) (string? name))
		  (or (null? type) (symbol? type) (string? type))
		  (or (integer? gen) (null? gen)))))))

(define (filename-equal? n1 n2)
  (let ((foo (lambda (x y)
               (or (eq? x y)
                   (and (string? x) (string? y) (string-equal? x y))))))
    (and (eq? (filename-fs   n1) (filename-fs   n2))
         (or (and (foo (filename-dir  n1) (filename-dir  n2))
                  (foo (filename-name n1) (filename-name n2))
                  (foo (filename-type n1) (filename-type n2)))
             (string-equal? (filename->string n1)
                            (filename->string n2))))))

;;; Handy utility.

(define (filename-with-type filename type)
  (make-filename (filename-fs filename)
                 (filename-dir filename)
                 (filename-name filename)
                 type
                 (filename-generation filename)))

;;; ---------- FS-FILENAME->STRING

;;; Synthesize a string naming a given file in file-system-native syntax.

(define-operation (fs-filename->string self fs ln name type gen)
  (string-append (namestring-dir-part fs ln)
                 (namestring-name-part fs name)
                 (namestring-type-part fs type)
                 (namestring-gen-part fs gen)))

(define-operation (fs-filename-leaf self fs name type gen)
  (string-append (namestring-name-part fs name)
                 (namestring-type-part fs type)
                 (namestring-gen-part fs gen)))

;;; A directory component may be one of:
;;; - null, for the current working directory;
;;; - a symbol, for a logical name; or
;;; - a string, for the actual name of a directory (e.g. "<F.T.X.SYS>").

(define (namestring-dir-part fs ln)
  (cond ((null? ln) "")
        ((string? ln)
         (massage-dir-string (fs-type fs) ln))
        ((symbol? ln)
         (massage-ln-string  (fs-type fs) (symbol-pname ln)))
        (else
         (namestring-dir-part
          fs
          (error "ill-formed directory spec~%  (~S~_~S~_~S~_...)"
                 'filename->string fs ln)))))

;;; The name part may be either a string or a symbol.
;;; [Note: for VMS, we should probably truncate to 9 characters.]

(define (namestring-name-part fs name)
  (cond ((symbol? name)
         (->preferred-case (fs-type fs) (symbol-pname name)))
        ((string? name)
         name)
        (else
         (namestring-name-part
          fs
          (error "ill-formed filename spec~%  (~S~_~S~_...~_~S~_...)"
                 'filename->string fs name)))))

(define (namestring-type-part fs type)
  (cond ((null? type) "")
        ((symbol? type)
         (let ((fstype (fs-type fs)))
           (or (translate-file-type fstype type)
               (string-append "."
                              (->preferred-case fstype (symbol-pname type))))))
        ((string? type)
         (string-append "." type))
        (else
         (namestring-type-part
          fs
          (error "ill-formed filename type~%  (~S~_~S~_...~_~S)"
                 'filename->string fs type)))))

(define (namestring-gen-part fs gen)
  (cond ((null? gen) "")
        ((and (or (symbol? gen)
                  (fixnum? gen))
              (massage-gen-part (fs-type fs) gen)))
        (else
         (namestring-gen-part
          fs
          (error "ill-formed filename generation~%  (~S~_~S~_...~_~S)"
                 'filename->string fs gen)))))

;;; ---------- Logical names

;;; Logical names internal to T.  The value of a logical name must be a
;;; pair (file-system . logical-name).

(define (resolve-logical-name fs ln circle)
  (let ((z (cons fs ln)))
    (cond ((not (symbol? ln)) z)
          (else
           (cond ((mem? (lambda (x y)
                          (and (eq? (car x) (car y)) (eq? (cdr x) (cdr y))))
                        z
                        circle)
                  ;; Lose!  We've tried to get this one before.
                  (error "circular logical name definitions: ~S" circle))
                 (else
                  (let ((probe (logical-name fs ln)))
                    (cond ((null? probe)
                           ;; Let someone else resolve the logical name.
                           z)
                          ((pair? probe)
                           (resolve-logical-name (car probe)
                                                 (cdr probe)
                                                 (cons z circle)))
                          (else
                           (resolve-logical-name (car z)
                                                 probe
                                                 (cons z circle)))))))))))

;;; ---------- Operating system & processor types

;;; Hack.

(lset *local-processor*
      (object nil
              ((processor-type self) *kernel-processor*)
              ((print-type-string self) "Processor")))

(define-operation (processor-type processor))

(define (local-processor) *local-processor*)

(define (mc68000-processor? processor)
  (eq? (processor-type processor) 'mc68000))

(define (vax11-processor? processor)
  (eq? (processor-type processor) 'vax11))

(define (pyramid-processor? processor)
  (eq? (processor-type processor) 'pyramid))

;;; Glack.

(lset *local-os*
      (object nil
              ((os-type self) *xeno-operating-system*)
              ((print-type-string self) "Operating-system")))

(define-operation (os-type os))

(define (local-os) *local-os*)

(define (aegis-os? os)
  (eq? (os-type os) 'aegis))

(define (unix-os? os)
  (eq? (os-type os) 'unix))

(define (vms-os? os)
  (eq? (os-type os) 'vms))
