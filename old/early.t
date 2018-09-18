(herald early
        (syntax-table *orbit-syntax-table*))

;;; Early binding (i.e. integrable procedures) database for ORBIT.

;;; Support environments.
;;;============================================================================

(define (make-support-env id super primop-env)
  (let ((table (make-table id)))
    (object (lambda (name)
              (or (table-entry table name)
                  (super name)))
            ((setter self)
             (lambda (name value)
               (set (table-entry table name) value)))
            ((local-support self)
             (select-from-table (lambda (key entry)
                                  (ignore key)
                                  (support.local? entry))
                                table))
            ((primop-support self name)
             (primop-env name))
            ((support-env-id self name)
             (if (table-entry table name)
                 id
                 (support-env-id super name)))
            ((augment-context self . rest)
             (get-support-environments self rest))
            ((print self stream)
             (format stream "#{Support-env ~D ~S}" (object-hash self) id)))))

(define-operation (local-support env proc))
(define-operation (primop-support env proc))
(define-operation (support-env-id env name) nil)

(define (make-primop-env id)
  (let ((table (make-table id)))
    (object (lambda (name)
              (table-entry table name))
            ((setter self)
             (lambda (name value)
               (set (table-entry table name) value)))
            ((print self stream)
             (format stream "#{Primop-env ~D ~S}" (object-hash self) id)))))

(define *primitive-primop-env*
  (make-primop-env '*primitive-primop-env*))

(define (make-empty-support-env id)
  (make-support-env id false (make-primop-env id)))

(define *primitive-support-env*
  (make-support-env '*primitive-support-env*
                    (make-empty-support-env 'nil)
                    *primitive-primop-env*))

(lset *standard-support-env* *primitive-support-env*)

(define-structure-type support
  local?
  variant
  value
  data)

(define support.local? support-local?)
(define support.all-data support-data)
(define-settable-operation (support.data self))
(define support.variant support-variant)
(define support.value
  (object (lambda (support)
            ((support-value support)))
    ((setter self) (setter support-value))))


(define-methods handle-support
  ((print self stream)
   (format stream "#{Support~_~S}" (object-hash self)))
  ((support.data self key)
   (cdr (assq key data)))
  (((setter support.data) self key new)
     (cond ((assq key data)
            => (lambda (entry)
                 (set (cdr entry) new)))
           (else
            (push data `(,key . ,new))))))


(define (make-support-entry local? data variant value)
  (let ((s (make-support)))
    (set (support-local? s)   local?)
    (set (support-variant s) variant)
    (set (support-value s)    value)
    (set (support-data s)     data)
    s))

(define (*define-support env name variant)
  (cond ((env name)
         (bug "support for ~S mutiply defined" name))
        (else
         (set (env name)
              (make-support-entry t '() variant false)))))

(define-predicate support-variant?)
(define-predicate permanent-support-variant?)

(define-local-syntax (define-support-variant name constant?)
  (let ((name (concatenate-symbol 'support/ name)))
    `(define ,name
       (object nil
         ((support-variant? self) t)
         ((identification self) ',name)
         ,@(if constant?
               '(((permanent-support-variant? self) t))
               '())))))

(define-support-variant lset ())
(define-support-variant define ())
(define-support-variant constant t)
(define-support-variant integrable t)
(define-support-variant wired t)
(define-support-variant multiple t)

(define (supported? var)
  (if (variable-support-env var) t nil))

(define (variable-support var)
  (cond ((variable-support-env var)
         => (lambda (env)
              (env (variable-name var))))
        (else
         (bug "there is no support for ~S" var))))

(define (add-support-value var variant value)
  (let ((name (variable-name var))
        (env  (variable-support-env var))
        (permanent? (and value
                         (or (primop-node? value)
                             (literal-node? value)))))
    (cond ((env name)
           => (lambda (support)
                (set (support.value support)
                     (if permanent?
                         (lambda () value)
                         (lambda () (get-support-definition var variant))))))
          (else
           (bug "adding support value of ~S but it has no support" name)))
    name))

(define (change-support-variant var variant)
  (let ((name (variable-name var))
        (env  (variable-support-env var)))
    (cond ((env name)
           => (lambda (support)
                (set (support.variant support) variant)))
          (else
           (bug "changing support variant of ~S but it has no support" name)))
    name))

(define (early-bind free-variables support)
  (let ((new-support (make-empty-support-env nil)))
    (iterate loop ((to-do free-variables) (free '()) (defined '()))
      (cond ((null? to-do)
             (values (reverse! free) (reverse! defined) new-support))
            (else
             (xcase (early-bind-var (car to-do) new-support support)
               ((defined)
                (loop (cdr to-do) free (cons (car to-do) defined)))
               ((early-bound)
                (loop (cdr to-do) free defined))
               ((free)
                (loop (cdr to-do) (cons (car to-do) free) defined))))))))

(define (early-bind-var var new-support support-env)
  (let* ((definitions (find-definitions var))
         (name (variable-name var))
         (support (support-env name)))
    (if (and definitions support)
        (warning "shadowing ~S" name))
    (cond ((< 1 (length definitions))
           (fix-multiple-definitions var)
           'defined)
          (definitions
           (noise "Defining '~S' support for ~S~%"
                  (identification (caar definitions))
                  (variable-name var))
           (*define-support new-support name (caar definitions))
           (set (variable-support-env var) new-support)
           (if (permanent-support-variant? (caar definitions))
               (add-permanent-support var (cdar definitions) new-support))
           'defined)
          (support
           (noise "Early binding (~S): ~S~%"
                  (object-hash var)
                  (variable-name var))
           (set (variable-support-env var) support-env)
           'early-bound)
          (else
           'free))))

(define (find-definitions var)
  (iterate loop ((to-do (variable-refs var)) (defs '()))
    (let ((ref (car to-do)))
      (cond ((null? to-do)
             defs)
            ((support-from-ref ref)
             => (lambda (support.variant)
                  (loop (cdr to-do) `((,support.variant . ,ref) . ,defs))))
            (else
             (loop (cdr to-do) defs))))))

(define (add-permanent-support var ref support-env)
  (let* ((parent (node-parent ref))
         (variant (support-from-ref ref))
         (value ((call-arg 3) parent)))
    (cond ((support-is-primop? value)
           (add-support-value var variant value))
          (else
           (if (and (lambda-node? value)
                    (not (eq? variant support/constant)))
               (mark-reference-sources value var))
           (add-support-value var variant value)))))

(define (support-from-ref ref)
  (let ((proc (call-proc (node-parent ref))))
    (if (and (eq? (call-arg 2) (node-role ref))
             (primop-node? proc)
             (primop.defines-support? (primop-value proc)))
        (primop.support-variant (primop-value proc))
        nil)))

(define (supports-definition? ref)
  (let ((proc (call-proc (node-parent ref))))
    (and (eq? (call-arg 3) (node-role ref))
         (primop-node? proc)
         (primop.defines-support? (primop-value proc)))))

(define (support-is-primop? value)
  (cond ((and (reference-node? value)
              (variable-binder (reference-variable value)))
         (let* ((binder (variable-binder (reference-variable value)))
                (call (node-parent binder)))
           (if (and (primop-ref? (call-proc call) primop/*primop)
                    (eq? (call-arg 1) (node-role binder))
                    (null? (cddr (call-args call))))
               (literal-value ((call-arg 2) call))
               nil)))
        ((and (lambda-node? value)
              (primop-ref? (call-proc (lambda-body value)) primop/*primop))
         (literal-value ((call-arg 2) (lambda-body value))))
        (else
         nil)))

(define (mark-reference-sources node variable)
  (iterate tree-walk ((node node))
    (xselect (node-variant node)
      ((leaf-node?)
       (if (reference-node? node)
           (push (reference-copy-sources node) variable)))
      ((call-node?)
       (walk tree-walk (call-proc+args node)))
      ((lambda-node?)
       (tree-walk (lambda-body node))))))

(define (get-support-definition var variant)
  (cond ((not (permanent-support-variant? variant))
         nil)
        ((eq? variant (support-from-ref (car (variable-refs var))))
         (get-support-from-ref variant (car (variable-refs var))))
        (else
         (iterate loop ((refs (cdr (variable-refs var))))
           (let ((ref (car refs)))
             (cond ((null? refs)
                    nil)
                   ((neq? variant (support-from-ref ref))
                    (loop (cdr refs)))
                   (else
                    (set (variable-refs var)
                         (cons ref (delq! ref (variable-refs var))))
                    (get-support-from-ref variant ref))))))))

(define (get-support-from-ref variant ref)
  (let ((value ((call-arg 3) (node-parent ref))))
    (cond ((support-is-primop? value)
           => (lambda (primop)
                (create-primop-node primop)))
          ((and (eq? variant support/constant)
                (not (suitable-constant-support? value)))
           nil)
          (else
           value))))

(define (suitable-constant-support? node)
  (and (leaf-node? node)
       (not (and (reference-node? node)
                 (variable-binder (reference-variable node))))))


(define (fix-multiple-definitions var)
  (warning "~S is multiply defined" (variable-name var))
  (walk (lambda (ref)
          (let ((variant (support-from-ref ref)))
            (if (permanent-support-variant? variant)
                (set (primop-value (call-proc (node-parent ref)))
                     primop/*define))))
        (variable-refs var)))
