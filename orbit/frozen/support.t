(herald support
        (syntax-table *orbit-syntax-table*))

(lset *support-tables* (make-table '*support-tables*))

;;; Reading and writing support files
;;;  Support tables are converted to s-expressions and then either written
;;; or dumped to files.


;;;========================================================================
;;; TABLES => FILES
;;;========================================================================

;;; (SUPPORT-TABLE->EXPRESSION name table)
;;;===========================================================================
;;;    Convert support table TABLE to an s-expression.  NAME is the name to
;;; be associated with the support.  Currently the name is a symbol.
;;;    The format is
;;; (<name>
;;;  <list of names of support tables that this one references>
;;;  <list of (<primop id> . <defining clauses>) pairs>
;;;  <list of (<name> <expressed-support>) pairs>
;;; )

(define (support-table->expression name table)
  (let ((support (local-support table)))
    (receive (tables primops)
             (needed-support-tables support)
    `(,name
      ,tables
      ,(map (lambda (pair)
              `(,(car pair)
                ,(primop.formals (cdr pair))
                . ,(primop.source (cdr pair))))
            primops)
      ,(map (lambda (pair)
              `(,(car pair) . ,(express-support (cdr pair))))
            support)))))

;;; (NEEDED-SUPPORT-TABLES support)
;;;===========================================================================
;;;  Returns a list of all the names of environments that contain primops or
;;; variables referred to in SUPPORT, which is a list of support structures.

(define (needed-support-tables support)
  (let ((primops (make-table 'primops)))
    (iterate loop ((support support) (tables '()))
      (cond ((null? support)
             (values tables (table->list primops)))
            ((support.value (cdar support))
             => (lambda (value)
                  (loop (cdr support)
                        (find-support-tables value primops tables))))
            (else
             (loop (cdr support) tables))))))

;;; (FIND-SUPPORT-TABLES node primops tables)
;;;===========================================================================
;;;  Dispatches on NODE to find the environments of primops and varaibles.
;;; PRIMOPS is a table of primops that are defined in the current locale.
;;; TABLES is a list of names of support envrionments that contain primops
;;; or variables referred to in the current locale.

(define (find-support-tables node primops tables)
  (cond ((empty? node)
         tables)
        ((not (node? node))
         (bug "bogus node in find-support-tables"))
        ((primop-node? node)
         (find-support-table-of-primop (primop-value node) primops tables))
        ((reference-node? node)
         (find-support-table-of-var (reference-variable node) tables))
        ((lambda-node? node)
         (find-support-tables (lambda-body node) primops tables))
        ((call-node? node)
         (iterate loop ((nodes (call-proc+args node)) (res tables))
           (cond ((null? nodes)
                  res)
                 (else
                  (loop (cdr nodes)
                        (find-support-tables (car nodes) primops res))))))
        (else
         tables)))

;;; (FIND-SUPPORT-TABLE-OF-PRIMOP primop primops tables)
;;;===========================================================================
;;;   The environment name of PRIMOP's environment is added to TABLES, or it is
;;; added to table PRIMOPS if it defined in this locale.

(define (find-support-table-of-primop primop primops tables)
  (cond ((primop.environment primop)
         =>(lambda (env)
             (if (memq? env tables)
                 tables
                 (cons env tables))))
        ((table-entry primops (primop.id primop))
         tables)
        (else
         (set (table-entry primops (primop.id primop)) primop)
         tables)))

;;; (FIND-SUPPORT-TABLE-OF-VAR var tables)
;;;===========================================================================
;;;   Add the name of VAR's support environment to TABLES if it isn't lambda
;;; bound or defined in the current locale.

(define (find-support-table-of-var var tables)
  (cond ((and (not (variable-binder var))
              (variable-support-env var))
         =>(lambda (env)
             (let ((name (support-env-id env (variable-name var))))
               (if (or (null? name)
                       (memq? name tables))
                   tables
                   (cons name tables)))))
        (else
         tables)))

;;; (EXPRESS-SUPPORT support)
;;;===========================================================================
;;;   Convert support to an s-expression.  The format is
;;; (<data a-list> <support variant> <value, a section of node tree>).

(define (express-support support)
  `(,(support.all-data support)
    ,(identification (support.variant support))
    ,(cond ((support.value support)
            => (lambda (value)
                 (express-node value)))
           (else
            nil))))

;;; (EXPRESS-NODE node)
;;;===========================================================================
;;; Convert a node to an s-expression.  Dispatches on the type of the node.

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

;;; (EXPRESS-LEAF node)
;;;===========================================================================
;;;  Literals are (LITERAL <value>), other leaves have their own procedure.

(define (express-leaf node)
  (xcase (leaf-variant node)
    ((literal)
     `(literal ,(literal-value node)))
    ((primop)
     (express-primop node))
    ((reference)
     (express-reference node))))

;;; (EXPRESS-REFERENCE node)
;;;===========================================================================
;;;  A reference node is (REFERENCE <variable name>) if the variable is a
;;; lambda variable and (REFERENCE <variable name> <environment name>)
;;; otherwise. <environment name> is '() for free variables.

(define (express-reference node)
  (let ((variable (reference-variable node)))
    `(reference
      ,(variable-unique-name variable)
      ,@(cond ((variable-binder variable)
               '())
              ((variable-support-env variable)
               => (lambda (env)
                    `(,(support-env-id env (variable-name variable)))))
              (else
               '(()))))))

;;; (EXPRESS-PRIMOP node)
;;;===========================================================================
;;; (PRIMOP <environment> <id> . <argument list>)

(define (express-primop node)
  (let ((primop (primop-value node)))
    `(primop
      ,(primop.environment primop)
      ,(primop.id primop)
      . ,(primop.arglist primop))))

;;; (EXPRESS-LAMBDA node)
;;;===========================================================================
;;; (LAMBDA <self variable name> <list of variable names> . <body>)

(define (express-lambda node)
  `(lambda ,(variable-unique-name (lambda-self-var node))
           ,(map variable-unique-name
                 (lambda-rest+variables node))
           . ,(express-call (lambda-body node))))

;;; (EXPRESS-CALL node)
;;;===========================================================================
;;; (<number of exits> . <children>)

(define (express-call node)
  `(,(call-exits node)
    . ,(map express-node (call-proc+args node))))



;;;========================================================================
;;; FILES => TABLES
;;;========================================================================

;;; (GET-SUPPORT-ENVIRONMENT id tables file-specs)
;;;========================================================================
;;;   Make a support environment.  ID is the identification, TABLES is a
;;; list of support tables to be included,  FILE-SPECS is a list of support
;;; files also to be included.

(define (get-support-environment id tables file-specs)
  (iterate loop ((tables (reverse tables)) (files (map ->filename file-specs)))
    (cond ((null? files)
           (make-support-env id (reverse tables)))
          (else
           (let ((table (get-support-table (car files))))
             (loop (if (memq? table tables)
                       tables
                       (cons table tables))
                   (cdr files)))))))

;;; (GET-SUPPORT-TABLE name)
;;;===========================================================================
;;;   Read the support file in if it hasn't already been done.

(define (get-support-table name)
  (let ((filename (->filename name)))
    (cond ((table-entry *support-tables* (filename-name filename))
           => identity)
          (else
           (set (table-entry *support-tables* (filename-name filename))
                (read-support-from-file filename))))))

;;; (READ-SUPPORT-FROM-FILE filename)
;;;===========================================================================
;;;  Retrieve the s-expression and convert it back into a table.  This may
;;; involve getting other support tables.

(define (read-support-from-file filename)
  (noise "Getting support for ~S~%" (filename-name filename))
  (with-open-streams ((input
                       (open-retrieve (filename-with-type filename 'sup))))
    (let* ((exp (read input))
           (tables (map (lambda (name)
                          `(,name . ,(get-support-table name)))
                        (cadr exp))))
      (expression->support-table exp tables))))


;;; (EXPRESSION->SUPPORT-TABLE expression support-envs)
;;;===========================================================================
;;;   Convert an s-expression back into a support table.  SUPPORT-ENVS is an
;;; a-list of (<identification> . <support table>) which includes all support
;;; tables referrence in EXPRESSION.

(define (expression->support-table expression support-envs)
  (destructure (((name #f primops support-defs) expression))
    (let* ((primop-table (primop-defs->table primops name))
           (shape (make-handle))
           (env (make-support-table name primop-table)))
      (walk (lambda (def)
              (set (env (car def))
                   (resurrect-support (car def)
                                      (cdr def)
                                      env
                                      shape
                                      `((#f . ,env) . ,support-envs))))
            support-defs)
      env)))

;;; (PRIMOP-DEFS->TABLE defs name)
;;;===========================================================================
;;;  Reconstruct a primop table.

(define (primop-defs->table defs name)
  (let ((table (make-primop-table name)))
    (walk (lambda (def)
            (set (table (car def))
                 (construct-primop (car def) name (cadr def) (cddr def))))
          defs)
    table))

;;; (RESURRECT-SUPPORT name exp env shape support-envs)
;;;===========================================================================
;;;   Convert an s-expression back into a support entry.

(define (resurrect-support name exp env shape support-envs)
  (destructure (((data variant value) exp))
    (let ((value (if value (resurrect-node value shape support-envs) nil))
          (var (create-variable name)))
      (set (variable-support-env var) env)
      (make-support-entry var
                          nil
                          data
                          (*value *orbit-env* variant)
                          (if value
                              (lambda () value)
                              false)))))

;;; (RESURRECT-NODE exp shape support-envs)
;;;===========================================================================
;;;    Convert an s-expression back into a node.  This is a very simple version
;;; of alphatize.

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

;;; (RESURRECT-LAMBDA exp shape support-envs)
;;;===========================================================================
;;;  Again, just like alphatize.

(define (resurrect-lambda exp shape support-envs)
  (destructure (((#f self-var-name var-names . body-exp) exp))
    (let* ((vars (map (lambda (name)
                        (if name (create-variable name) nil))
                      var-names))
           (node (create-lambda-node self-var-name vars)))
      (relate lambda-body
              node
              (resurrect-call body-exp
                              (bind-variables vars node shape)
                              support-envs))
      node)))

;;; (RESURRECT-CALL exp shape support-envs)
;;;===========================================================================
;;; "

(define (resurrect-call exp shape support-envs)
  (destructure (((exits . exps) exp))
    (let ((node (create-call-node (length exps) exits)))
      (relate-call-proc+args node
                             (map (lambda (exp)
                                    (resurrect-node exp shape support-envs))
                                  exps))
      node)))

;;; (RESURRECT-PRIMOP exp support-envs)
;;;===========================================================================
;;;  Find the appropriate environment and get the primop from it.

(define (resurrect-primop exp support-envs)
  (destructure (((#f env-name name . arglist) exp))
    (cond ((assq env-name support-envs)
           => (lambda (env)
                (get-primop-from-env name arglist (cdr env))))
          (else
           (bug "primop lookup error, can't find environment for ~S" exp)))))

;;; (GET-PRIMOP-FROM-ENV name arglist env)
;;;===========================================================================
;;;  Create a primop node.  This may involve adding argument values to the
;;; primop.

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

;;; (RESURRECT-REFERENCE exp shape support-envs)
;;;===========================================================================
;;;  Get the variable from SHAPE if it is a lambda variable, from SUPPORT-ENVS
;;; if it has an environment, and create it otherwise.

(define (resurrect-reference exp shape support-envs)
  (destructure (((() name env-name) exp))
    (cond ((null? (cddr exp))
           (create-reference-node (obtain-variable shape name)))
          ((cdr (assq env-name support-envs))
           => (lambda (env)
                (let ((support (env name)))
                  (cond (support
                         (create-reference-node (support.variable support)))
                        ((null? env-name)
                         (create-reference-node (create-variable name)))
                        (else
                         (bug "no support for ~S in ~S" name env-name))))))
          (else
           (bug "variable lookup error, can't find environment for ~S" exp)))))
