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

;;; Print CPS code in linear form.
;;;===========================================================================

(define (pp-cps node . stream)
  (pp-cps-1 node 2 (if stream
                       (car stream)
                       (terminal-output))))

(define (pp-cps-1 node indent-to stream)
  (let ((z (pp-cps-2 node)))
    (cond ((lambda-node? node)
           (let ((vars (lambda-variables node)))
             (set (hpos stream) indent-to)
             (writec stream #\()
             (write stream (map variable-unique-name vars))
             (set (hpos stream) (fx+ indent-to 18))     ;format sux
             (pretty-print (pp-cps-2 (lambda-body node)) stream)    ;for '
             (format stream ")~%")
             (pp-cps-body (lambda-body node) indent-to stream))))
    z))

(define (pp-cps-body node indent-to stream)
  (cond ((and (primop-node? (call-proc node))
              (primop.pp-cps (primop-value (call-proc node))
                             (call-proc node)
                             indent-to
                             stream))
         nil)
        (else
         (walk (lambda (node) (pp-cps-1 node (fx+ indent-to 1) stream))
               (call-proc+args node)))))

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

;;; Called by (primop.pp-cps primop/*block ...)
(define (pp-cps-block node indent stream)
  (let ((indent (fx+ indent 1)))
    (iterate loop ((call (node-parent node)))
      (let ((arg1 ((call-arg 1) call)))
        (pp-cps-1 ((call-arg 2) call) indent stream)
        (cond ((not (primop-ref? (call-proc (lambda-body arg1))
                                 primop/*block))
               (pp-cps-1 arg1 indent stream))
              (else
               (loop (lambda-body arg1))))))))

(define (lambda-name node)
  (variable-unique-name (car (lambda-variables node))))

;;; Returns a lexically unique name for the variable.

(define (variable-unique-name var)
  (cond ((variable? var)
         (and (used? var)
              (let ((name (variable-name var)))
                (cond ((variable-binder var)
                       (concatenate-symbol name "_" (variable-id var)))
                      (else
                       name)))))
        ((primop? var)
         (identification var))
        (else var)))


;;; Copy node structure.
;;;===========================================================================

(define (copy-node-tree node rename source)
  (xselect (node-variant node)
    ((leaf-node?)
     (copy-leaf node rename source))
    ((lambda-node?)
     (copy-lambda node rename source))
    ((call-node?)
     (copy-call node rename source))))

(define (copy-leaf node rename source)
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
       (if source
           (set (reference-copy-sources new)
                (cons source (reference-copy-sources node))))
       new))))

(define (copy-lambda node rename source)
  (let* ((vars (lambda-variables node))
         (new-vars (map (lambda (var)
                          (if (used? var)
                              (let ((new-var (create-variable
                                               (variable-name var))))
                                (set (variable-rep new-var)
                                     (variable-rep var))
                                new-var)
                              nil))
                        vars)))
    (let ((new-node (create-lambda-node new-vars)))
      (relate lambda-body
              new-node
              (copy-node-tree (lambda-body node)
                              (append! (map cons vars new-vars) rename)
                              source))
      new-node)))

(define (copy-call node rename source)
  (let* ((args (call-proc+args node))
         (new-node (create-call-node (length args) (call-exits node))))
    (walk (lambda (arg)
            (relate (node-role arg)
                    new-node
                    (copy-node-tree arg rename source)))
          args)
    new-node))

;;; Erase
;;;============================================================================
;;; Erase node structure.  Mostly to update the REFS slot of
;;; variables free to this node.

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
               (let ((var (reference-variable node)))
                 (modify (variable-refs var)
                         (lambda (refs) (delq! node refs)))
                 (if (and (null? (variable-refs var))
                          (variable-binder var))
                     (set (nth (lambda-variables (variable-binder var))
                               (variable-number var))
                          nil)))))
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
