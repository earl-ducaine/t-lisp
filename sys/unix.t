(herald (tsys unix t 159)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; UNIX interface

;;; This is called from INITIALIZE-T.  Do interesting stuff.
;;; This happens as the initial read-eval-print is entered, but before the
;;; patch file and init file are loaded.

(define (initialize-os-stuff)
  (initialize-interrupt-handlers)
  (initialize-stack-guard)
  (set gc-output   zterminal-output)
  (do ((i (fx- *argc* 1) (fx- i 1))
       (l '() (cons (asciz->string (xref *argv* i)) l)))        ; yuck!!
      ((fx< i 0)
       (set *command-line* l))))

;;; Horrible hacks for terminal i/o.  Assume for now same as standard i/o.
;;; Fix later.

(define *ttyin-channel*  *stdin-channel*)
(define *ttyout-channel* *stdout-channel*)

(define (experimental?)
  (string-posq #\x (car *command-line*)))

(define (the-t-system-directory)        ; fix file and localfs file
  (if (experimental?) 'xtsystem 'tsystem))

(define (the-init-file-directory)       ; user init file
  'home)


;;; Bug: people might move ^Z to some other key.

(define (luser-typed-eof-at-top-level)
  (format (error-output)
          "** Use ^Z or (STOP) to suspend, or (EXIT) to exit.~%"))

(define (stop)
  (call-xenoid *kill-xenoid* (fixnum->pointer 17) 0)
  *repl-wont-print*)

(lset *exit-agenda* '())

(define (exit . foo)
  (do ((l (swap *exit-agenda* '()) (cdr l)))
      ((null? l)
       (call-xenoid *exit-xenoid*
		    (fixnum->pointer (if (null? foo) 0 (car foo)))))
    ((car l))))

(define (unix-shell-command com)
  (let ((com (check-arg string? com unix-shell-command)))
    (call-xenoid *system-xenoid* (string->asciz com))
    0))

;;; Someday, the following ought to be settable.

(define (unix-getenv var)
  (let ((var (check-arg string? var unix-getenv)))
    (let ((probe (call-xenoid *getenv-xenoid*
                              (string->asciz var))))
      (cond ((pointer-equal? probe 0)
             nil)
            (else (asciz->string probe))))))

;;; Returns the login name as a string.
(define (unix-user-name)
  (asciz->string (xcall *getlogin-xenoid*)))


;;; Filename and file system stuff

(define (local-fs-creator) make-unix-fs)

;;; Parse a losing unix filespec.
;;;   This procedure handles $SYM and HOST:, but we punt ~USER for now.
;+++  This is consing far to much.

(define (parse-unix-filespec str)
  (check-arg string? str parse-unix-filespec)
  (let* ((len (string-length str))
         (fs-len   (let ((pos  (string-posq #\: str))
                         (slsh (string-posq #\/ str)))
                     (if (or (not pos)
                               (and slsh
                                    (fx< slsh pos)))
                           -1
                           pos)))
         (fs       (if (fx= -1 fs-len)
                       nil
                       (string->file-part (string-slice str 0 fs-len))))
         (leaf-pos (fx+ 1 (or (string-back-posq #\/ str)
                              (string-back-posq #\: str)
                              -1)))
         (leaf-len (fx- len leaf-pos))
         (leaf     (string-slice str
                                 leaf-pos
                                 leaf-len))
         (type-pos (let ((pos (string-back-posq #\. leaf)))
                     (if pos (fx+ pos 1) -1)))
         (type     (cond ((fxN= -1 type-pos)
                          (block0
                             (string->file-part 
                                 (string-slice leaf
                                               type-pos
                                               (fx- leaf-len type-pos)))
                             (set (string-length leaf) (fx- type-pos 1))))
                          (else nil)))
         (dir (cond ((fx= (fx+ fs-len 1) leaf-pos) nil)
                    ((char= (nthchar str (fx+ fs-len 1)) #\$)
                     (string->symbol
                         (string-upcase
                            (string-slice str
                                          (fx+ fs-len 2)
                                          (fx- (fx- leaf-pos fs-len) 3)))))
                    (else
                     (string->file-part
                         (string-slice str
                                       (fx+ fs-len 1)
                                       (fx- (fx- leaf-pos fs-len) 2)))))))
     (make-filename fs dir (string->file-part leaf) type nil)))

      
(define-integrable (string-back-posq ch str)
  (iterate loop ((i (fx- (string-length str) 1)))
    (cond ((fx= i -1) nil)
          ((char= ch (nthchar str i)) i)
          (else (loop (fx- i 1))))))

;;; Make a file part a symbol if possible, otherwise it remains a string.
 
(define (string->file-part str)
  (cond ((string-empty? str) str)
        ((convertible? str (string-length str) 0)
          (string->symbol (string-upcase str)))
        (else str)))

;*** CONVERTIBLE?
;*** ==============================================================
;*** Returns T if the string can unambiguously be converted to 
;*** a symbol and back again.
;***
(define-integrable (convertible? str len idx)
  (iterate looop ((i idx))
    (cond ((fx= len i) T)
          (else
           (let ((ch (nthchar str i)))
             (cond ((or (uppercase? ch)
                        (char= ch #\/)
                        (char= ch #\~)
                        (char= ch #\.))
                    nil)
                   (else
                    (looop (fx+ i 1)))))))))

;;; Assemble an object file.

(define (assemble f)
  (let* ((f (->filename f))
         (f1 (filename-with-type f 's))
         (f2 (filename-with-type f 'bin))
         (f3 (filename-with-type f 'sd)))
    (cond ((file-exists? f1)
           (unix-shell-command
            (format nil "/t/bin/tas -o ~A ~A"
                    (filename->string f2)
                    (filename->string f1)))
           (unix-shell-command
            (format nil "mv ~A ~A"
                    (filename->string f1)
                    (filename->string f3)))
           f2)
          (else
           (error "assembly file not found~%  ~S" `(assemble ,f1))))))
