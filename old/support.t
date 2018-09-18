(herald support
        (syntax-table *orbit-syntax-table*))

;;; Reading and writing support files

;;; Convert a support environment to an s-expression
;;;===========================================================================

(define (support-env->expression name env)
  (let ((support (local-support env)))
    (receive (envs primops)
             (needed-support-envs support)
    `(,name
      ,envs
      ,(map (lambda (pair)
              `(,(car pair)
                ,(primop.formals (cdr pair))
                . ,(primop.source (cdr pair))))
            primops)
      ,(map (lambda (pair)
              `(,(car pair) . ,(express-support (cdr pair))))
            support)))))

(define (needed-support-envs support)
  (let ((primops (make-table 'primops)))
    (iterate loop ((support support) (envs '()))
      (cond ((null? support)
             (values envs (table->list primops)))
            ((support.value (cdar support))
             => (lambda (value)
                  (loop (cdr support)
                        (find-support-envs value primops envs))))
            (else
             (loop (cdr support) envs))))))

(define (find-support-envs node primops envs)
  (cond ((empty? node)
         envs)
        ((not (node? node))
         (bug "bogus node in find-support-envs"))
        ((primop-node? node)
         (let ((primop (primop-value node)))
           (cond ((primop.environment primop)
                  =>(lambda (env)
                      (if (memq? env envs)
                          envs
                          (cons env envs))))
                 ((table-entry primops (primop.id primop))
                  envs)
                 (else
                  (set (table-entry primops (primop.id primop)) primop)
                  envs))))
        ((lambda-node? node)
         (find-support-envs (lambda-body node) primops envs))
        ((call-node? node)
         (iterate loop ((nodes (call-proc+args node)) (res envs))
           (cond ((null? nodes)
                  res)
                 (else
                  (loop (cdr nodes)
                        (find-support-envs (car nodes) primops res))))))
        ((and (reference-node? node)
              (not (variable-binder (reference-variable node)))
              (variable-support-env (reference-variable node)))
         =>(lambda (env)
             (let ((name (support-env-id
                           env
                           (variable-name (reference-variable node)))))
               (if (or (null? name)
                       (memq? name envs))
                   envs
                   (cons name envs)))))
        (else
         envs)))

;;; Convert support to an s-expression
;;;===========================================================================

(define (express-support support)
  `(,(support.all-data support)
    ,(identification (support.variant support))
    ,(cond ((support.value support)
            => (lambda (value)
                 (express-node value)))
           (else
            nil))))

;;; Convert a node to an s-expression
;;;===========================================================================

(define (express-node node)
  (cond ((or (empty? node)
             (not (node? node)))
         (bug "trying to express an empty or illegitimate node ~S" node))
        (else
         (xselect (node-variant node)
           ((leaf-node?)
            (express-leaf node))
           ((lambda-node?)
            (express-lambda node))
           ((call-node?)
            (express-call node))))))

(define (express-leaf node)
  (xcase (leaf-variant node)
    ((literal)
     `(literal ,(literal-value node)))
    ((primop)
     (let ((primop (primop-value node)))
       `(primop
         ,(primop.environment primop)
         ,(primop.id primop)
         . ,(primop.arglist primop))))
    ((reference)
     (let ((variable (reference-variable node)))
       `(reference
         ,(variable-unique-name variable)
         ,@(cond ((variable-binder variable)
                  '())
                 ((variable-support-env variable)
                  => (lambda (env)
                       `(,(support-env-id env (variable-name variable)))))
                 (else
                  '(()))))))))

(define (express-lambda node)
  `(lambda ,(map variable-unique-name
                 (lambda-variables node))
     ,@(express-call (lambda-body node))))

(define (express-call node)
  `(,(call-exits node)
    . ,(map express-node (call-proc+args node))))

;;; Getting all the support necessary for a file
;;;========================================================================

(define (get-support-environments init file-specs)
  (let ((support-envs (make-table 'support-envs)))
    (iterate loop ((env init) (files (map ->filename file-specs)))
      (cond ((null? files)
             env)
            (else
             (loop (read-support-from-file (car files) env support-envs)
                   (cdr files)))))))

(define (read-support-from-file filename super support-envs)
  (with-open-streams ((input
                       (open-retrieve (filename-with-type filename 'sup))))
    (let* ((exp (read input))
           (envs (map (lambda (name)
                        `(,name . ,(get-support-env name support-envs)))
                      (cadr exp))))
      (set (table-entry support-envs (car exp))
           (expression->support-env exp super envs)))))

(define (get-support-env name support-envs)
  (cond ((table-entry support-envs name)
         => identity)
        (else
         (set (table-entry support-envs name)
              (read-support-from-file (->filename name) false support-envs)))))

;;; Convert an s-expression back into a support environment
;;;===========================================================================

(define (expression->support-env expression super support-envs)
  (destructure (((name env-names primops support-defs) expression))
    (let* ((primop-env (primop-defs->env primops name))
           (shape (make-handle))
           (env (make-support-env name super primop-env)))
      (walk (lambda (def)
              (set (env (car def))
                   (resurrect-support (cdr def)
                                      shape
                                      `((#f . ,env) . ,support-envs))))
            support-defs)
      env)))

(define (primop-defs->env defs name)
  (let ((env (make-primop-env name)))
    (walk (lambda (def)
            (set (env (car def))
                 (construct-primop (car def) name (cadr def) (cddr def))))
          defs)
    env))

;;; Convert an s-expression back to support
;;;===========================================================================

(define (resurrect-support exp shape support-envs)
  (destructure (((data variant value) exp))
    (let ((value (if value (resurrect-node value shape support-envs) nil)))
      (make-support-entry nil
                          data
                          (*value *orbit-env* variant)
                          (if value
                              (lambda () value)
                              false)))))

;;; Convert an s-expression back into a node
;;;===========================================================================

(define (resurrect-node exp shape support-envs)
  (case (car exp)
    ((lambda)
     (resurrect-lambda exp shape support-envs))
    ((literal)
     (create-literal-node (cadr exp)))
    ((primop)
     (resurrect-primop exp support-envs))
    ((reference)
     (resurrect-reference exp shape support-envs))
    (else
     (bug "resurrect-node got an unknown form ~S" exp))))

(define (resurrect-lambda exp shape support-envs)
  (destructure (((() var-names . body-exp) exp))
    (let* ((vars (map create-variable var-names))
           (node (create-lambda-node vars)))
      (relate lambda-body
              node
              (resurrect-call body-exp
                              (bind-variables vars node shape)
                              support-envs))
      node)))

(define (resurrect-call exp shape support-envs)
  (destructure (((exits . exps) exp))
    (let ((node (create-call-node (length exps) exits)))
      (relate-call-proc+args node
                             (map (lambda (exp)
                                    (resurrect-node exp shape support-envs))
                                  exps))
      node)))

(define (resurrect-primop exp support-envs)
  (destructure (((() env-name name . arglist) exp))
    (cond ((assq env-name support-envs)
           => (lambda (env)
                (get-primop-from-env name arglist (cdr env))))
          (else
           (bug "primop lookup error, can't find environment for ~S" exp)))))

(define (get-primop-from-env name arglist env)
  (cond ((primop-support env name)
         =>(lambda (primop)
             (create-primop-node (if arglist
                               (join (object nil
                                       ((primop.constructed? self) t)
                                       ((primop.arglist self) arglist))
                                     primop)
                               primop))))
        (else
         (bug "primop lookup failed, can't find ~S" exp))))

(define (resurrect-reference exp shape support-envs)
  (destructure (((() name env-name) exp))
    (cond ((null? (cddr exp))
           (create-reference-node (obtain-variable shape name)))
          ((cdr (assq env-name support-envs))
           => (lambda (env)
                (let ((var (create-variable name)))
                  (set (variable-support-env var) env)
                  (create-reference-node var))))
          (else
           (bug "variable lookup error, can't find environment for ~S" exp)))))
