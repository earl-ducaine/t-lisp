(herald util
        (syntax-table *orbit-syntax-table*))

;;; Imports
;;;===========================================================================

(import *t-implementation-env*
        print-type-string
        self-evaluating?
        blockify
        arglist->argspectrum
        make-syntax-descriptor
        walk-table
        check-special-form-syntax
        filename-with-type
        parse-herald
        herald-support
        augment-context)

(load '(hax stringtable))
(load '(dump loaddump))

;;; Messages
;;;==========================================================================

(lset *noise-stream* t)  ;So NOISE works at top level.
(lset *noise-flag* t)
(lset *debug-flag* t)

(define (bug f . rest)
  (apply error (list f "~%  (compiler error)") rest))

(define (warning f . rest)
  (apply format *noise-stream* (list "~%;;; Warning: " f) rest))

(define (noise f . rest)
  (if *noise-flag*
      (apply format *noise-stream* (list "~&;;; " f) rest)))

(define (orbit-debug . args)
  (if *debug-flag*
      (apply format *noise-stream* args)))


;;; (PP-CPS node . stream)
;;;===========================================================================
;;; Print CPS node tree in linear form.  Stream defaults to terminal output.

(define (pp-cps node . stream)
  (pp-cps-1 node 2 (if stream
                       (car stream)
                       (terminal-output))))

(define (pp-cps-1 node indent-to stream)
  (let ((z (pp-cps-2 node)))
    (cond ((lambda-node? node)
           (let ((vars (lambda-all-variables node)))
             (set (hpos stream) indent-to)
             (writec stream #\()
             (write stream (map variable-unique-name vars))
             (set (hpos stream) (fx+ indent-to 18))     ;format sux
             (pretty-print (pp-cps-2 (lambda-body node)) stream)    ;for '
             (format stream ")~%")
             (pp-cps-body (lambda-body node) indent-to stream))))
    z))

(define (pp-cps-body node indent-to stream)
  (walk (lambda (node) (pp-cps-1 node (fx+ indent-to 1) stream))
        (call-proc+args node)))

(define (pp-cps-2 node)
  (cond ((not (node? node))
         `(not-a-node ,node))
        (else
         (xselect (node-variant node)
           ((lambda-node?)
            (lambda-name node))
           ((leaf-node?)
            (case (leaf-variant node)
                  ((literal)
                   `',(literal-value node))
                  ((primop)
                   (cond ((primop.constructed? (primop-value node))
                          `(,(identification (primop-value node))
                            . ,(primop.arglist (primop-value node))))
                         (else
                          (identification (primop-value node)))))
                  (else
                   (variable-unique-name (reference-variable node)))))
           ((call-node?)
            (map pp-cps-2 (call-proc+args node)))))))

(define (lambda-name node)
  (variable-unique-name (lambda-self-var node)))

;;; Returns a lexically unique name for the variable.

(define (variable-unique-name var)
  (cond ((variable? var)
         (let ((name (variable-name var)))
           (cond ((variable-binder var)
                  (concatenate-symbol name "_" (variable-id var)))
                 (else
                  name))))
        ((primop? var)
         (identification var))
        (else
         var)))

;;; (COPY-NODE-TREE node rename)
;;;===========================================================================
;;;   NODE is the root to start from, RENAME is an a-list of variables and
;;; their replacements in the copy.

(define (copy-node-tree node rename)
  (xselect (node-variant node)
    ((leaf-node?)
     (copy-leaf node rename))
    ((lambda-node?)
     (copy-lambda node rename))
    ((call-node?)
     (copy-call node rename))))

(define (copy-leaf node rename)
  (xcase (leaf-variant node)
    ((literal)
     (create-literal-node (literal-value node)))
    ((primop)
     (create-primop-node (primop-value node)))
    ((reference)
     (let* ((var (reference-variable node))
            (new (cond ((assq var rename)
                        => (lambda (z)
                             (create-reference-node (cdr z))))
                       (else
                        (create-reference-node var)))))
       new))))

(define (copy-lambda node rename)
  (let* ((vars (lambda-rest+variables node))
         (new-vars (map (lambda (var)
                          (if var
                              (create-variable (variable-name var))
                              nil))
                        vars))
         (new-node (create-lambda-node (variable-name (lambda-self-var node))
                                       new-vars)))
    (relate lambda-body
            new-node
            (copy-node-tree (lambda-body node)
                            (append! (map cons vars new-vars) rename)))
    new-node))

(define (copy-call node rename)
  (let* ((args (call-proc+args node))
         (new-node (create-call-node (length args) (call-exits node))))
    (walk (lambda (arg)
            (relate (node-role arg)
                    new-node
                    (copy-node-tree arg rename)))
          args)
    new-node))

;;; (ERASE node)
;;; (ERASE-ALL node)
;;;============================================================================
;;;     Erase node structure.  Mostly to update the REFS slot of variables free
;;; to this node.

(define (erase-all node)
  (cond ((empty? node)
         nil)
        (else
         (walk erase-all (node-children node))
         (erase node))))

(define (erase node)
  (cond ((empty? node)
         nil)
        (else
         (cond ((reference-node? node)
                (modify (variable-refs (reference-variable node))
                        (lambda (refs)
                          (delq! node refs)))))
         (set *loose* (fx- *loose* 1))
         (set (node-role node) '<erased>))))






















;;; Little utilities.
;;;========================================================================

(define (find pred l)
  (iterate loop ((l l))
    (cond ((null? l) nil)
          ((pred (car l)) (car l))
          (else (loop (cdr l))))))

(define (filter pred l)
  (iterate loop ((l l) (r '()))
    (cond ((null? l) (reverse! r))
          ((pred (car l)) (loop (cdr l) (cons (car l) r)))
          (else (loop (cdr l) r)))))

(define (select-from-table pred table)
  (let ((res '()))
    (walk-table table
                (lambda (key entry)
                  (if (pred key entry)
                      (push res `(,key . ,entry)))))
    res))

(define (table->list table)
  (select-from-table true table))
