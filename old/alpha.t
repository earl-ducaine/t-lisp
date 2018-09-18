(herald alpha
    (syntax-table *orbit-syntax-table*))

;;; Primitive syntax
;;;========================================================================
;;;   The compiler knows about several special forms.  Some of them are part
;;; of T, the others are internal to the compiler.  T special forms that are
;;; not of interest to the compiler are expanded by their standard macro-
;;; expanders and the expanded code is compiled.
;;;   Handlers for the special forms that are of interest to the compiler
;;; are defined with the following forms.

;;; The syntax table containing all special forms of interest to the
;;; compiler.

(define *primitive-syntax-table*
  (make-syntax-table false '*primitive-syntax-table*))

;;; A table of compilator procedures to handle instances of special forms the
;;; compiler knows about.

(define *primitive-handler-table*
  (make-table '*primitive-handler-table*))

;;; Define a compilator for a T special form.

(define-local-syntax (define-exported-compilator pattern vars . body)
  (if (or (not (list? vars))
          (n= 2 (length vars)))
      (bug "Define-exported-compilator - incorrect VARS argument ~S" vars)
      `(define-alpha-compilator ,pattern ,vars #t ,body)))

;;; Define a compiler internal special form.

(define-local-syntax (define-compilator pattern vars . body)
  (if (or (not (list? vars))
          (n= 2 (length vars)))
      (bug "Define-compilator - incorrect VARS argument ~S" vars)
      `(define-alpha-compilator ,pattern ,vars #f ,body)))

;;; The guts of both of the above macros.  Puts a syntax descriptor into
;;; *primitive-syntax-table* and a handler in *primitive-handler-table*
;;; using the syntax descriptor as a key.

(define-local-syntax (define-alpha-compilator pattern vars export? body)
  (let* ((name (car pattern))
         (sym (concatenate-symbol 'syntax/ name))
         (exp (generate-symbol 'exp)))
    `(let ((descr ,(if export?
                       `(syntax-table-entry *standard-syntax-table* ',name)
                       `(make-syntax-descriptor
                            ',name
                            ',((*value *t-implementation-env*
                                       'arglist->argspectrum)
                               (cdr pattern))))))
       (set (syntax-table-entry *primitive-syntax-table* ',name) descr)
       (set (table-entry *primitive-handler-table* descr)
            (lambda (,exp ,@vars)
              (ignorable ,(car vars))
              (ignorable ,(cadr vars))
              (destructure ((,(cdr pattern) (cdr ,exp)))
                ,@body)))
       (define ,sym descr))))
       

;;; Alpha-conversion
;;;=========================================================================

;;; (ALPHA expression syntax-table shape) -> node
;;;=========================================================================
;;;   This performs dispatch for alpha-conversion.  It could be called
;;; 'COERCE-TO-NODE' since EXP may include syntax-descriptors, primops, and
;;;  node and variable structures as well as lists and symbols.

(define (alpha exp syntax shape)
  (cond ((symbol? exp)
         (alpha-reference exp shape))
        ((pair? exp)
         (cond ((symbol? (car exp))
                (let ((probe (syntax-table-entry syntax (car exp))))
                  (cond ((null? probe)
                         (alpha-call exp syntax shape))
                        (else
                         (alpha-special-form probe exp syntax shape)))))
               ((syntax-descriptor? (car exp))
                (alpha-special-form (car exp) exp syntax shape))
               (else
                (alpha-call exp syntax shape))))
        ((self-evaluating? exp)
         (alpha-literal exp))
        ((primop? exp)
         (alpha-primop-reference exp))
        ((node? exp)
         exp)
        (else
         (alpha (syntax-error "uncompilable object~%  ~S" exp) syntax shape))))

(define (alpha-special-form descr exp syntax shape)
  (let ((proc (table-entry *primitive-handler-table* descr))
        (new-exp (check-special-form-syntax descr exp)))
    (cond ((neq? exp new-exp)
           ;; An error was reported, and luser gave us a new form.
           (alpha new-exp syntax shape))
          (proc
           (proc exp syntax shape))
          ((macro-expander? descr)
           (alpha (invoke-macro-expander descr exp) syntax shape))
          (else
           (alpha (syntax-error "special form unknown to this compiler~%  ~S"
                                exp)
                  syntax
                  shape)))))

;;; --- Primitive node converters: primop, reference, lambda, call

(define (alpha-literal literal)
  (create-literal-node literal))

(define (alpha-primop-reference primop)
  (create-primop-node primop))

(define-exported-compilator (quote value) (syntax shape)
  (alpha-literal value))

(define (alpha-reference exp shape)
  (create-reference-node (obtain-variable shape exp)))

(define (immediate? value)
  (or (fixnum? value)
      (char? value)
      (eq? value '#F)
      (eq? value '#T)))

;;; (lambda (a b c . rest) . body)
;;;  ==>  (n-ary (lambda (a b c rest) . body))

(define-exported-compilator (lambda vars . body) (syntax shape)
  (alpha-lambda nil (cons 'k vars) body syntax shape))

(define-exported-compilator (named-lambda name vars . body) (syntax shape)
  (alpha-lambda name (cons 'k vars) body syntax shape))

(define-compilator (clambda vars . body) (syntax shape)
  (alpha-lambda nil vars body syntax shape))

(define (alpha-lambda proc-name var-names body syntax shape)
  (cond ((proper-list? var-names)
         (let* ((vars (map (lambda (name)
                             (cond ((null? name) nil)
                                   ((variable? name) name) ;for alpha-top etc.
                                   (else (create-variable name))))
                           (cons proc-name var-names)))
                (node (create-lambda-node vars)))
           (relate lambda-body node
                   (alpha-block body syntax
                                (bind-variables (cdr vars)
                                                node
                                                shape)))
           node))
        (else
         (do ((l var-names (cdr l))
              (z '() (cons (car l) z)))
             ((atom? l)
              (let ((node (alpha-lambda proc-name (reverse! (cons l z))
                                  body syntax shape)))
                (set (variable-rep (car (lambda-variables node))) 'n-ary)
                node))))))


(define-exported-compilator (set-var . body) (syntax shape)
  (alpha-call `(,primop/*set-var ,@body) syntax shape))

(define-exported-compilator (define-var var value) (syntax shape)
  (alpha-call `(,primop/*define ,var ,value) syntax shape))

(define-exported-compilator (define-integrable form . body) (syntax shape)
  (cond ((atom? form)
         (alpha `(define-constant ,form . ,body) syntax shape))
        (else
         (alpha-call `(,primop/*define-integrable
                       ,(car form)
                       (lambda ,(cdr form) . ,body))
                     syntax shape))))

(define-exported-compilator (define-wired form . body) (syntax shape)
  (cond ((atom? form)
         (alpha `(define-constant ,form . ,body) syntax shape))
        (else
         (alpha-call `(,primop/*define-wired
                       ,(car form)
                       (lambda ,(cdr form) . ,body))
                     syntax shape))))

(define-exported-compilator (define-constant var value) (syntax shape)
  (alpha-call `(,primop/*define-constant ,var ,value) syntax shape))


(define-exported-compilator (lset var . rest) (syntax shape)
  (alpha-call `(,primop/*lset ,var ,@rest) syntax shape))

(set (syntax-table-entry *standard-syntax-table* 'primop)
     (macro-expander (primop . rest)
       `(error "primops don't interpret yet ~S" '(primop . ,rest))))

(define-exported-compilator (primop id vars . clauses) (syntax shape)
  (let ((primop (construct-primop id nil vars clauses)))
    (alpha-call `(,primop/*primop ',primop . ,vars) syntax shape)))

(define (construct-primop name env formals clauses)
  (eval `(object nil
           ((primop? self) t)
           ((identification self) ',name)
           ((primop.id self) ',name)
           ((primop.formals self) ',formals)
           ((primop.source self) ',clauses)
           ((primop.environment self) ',env)
           . ,clauses)
        *orbit-env*))


;;; (foo x y)  ==>  (exits 1 %continuation x y)

(define (alpha-call exp syntax shape)
  (alpha-call-1 (car exp)
                (cons primop/continuation (cdr exp))
                1
                syntax
                shape))

(define-compilator (exits count proc . args) (syntax shape)
  (alpha-call-1 proc args count syntax shape))

(define (alpha-call-1 proc args exits syntax shape)
  (let ((node (create-call-node (fx+ 1 (length args)) exits)))
    (relate call-proc node (alpha proc syntax shape))
    (relate-call-args node (map (lambda (arg) (alpha arg syntax shape)) args))
    node))

(define (alpha-error . rest)
  (apply error rest)
  (alpha-primop-reference primop/undefined))

;;; --- Everything else is done source-to-source.

;;; (if p c a)
;;;   ==> (if (lambda () a) (lambda () c) true? p)
;;; (if p c)
;;;   ==> (if p c $)

(define-exported-compilator (if tested con . maybe-alt) (syntax shape)
  (alpha `(,syntax/exits 2
                         ,primop/conditional
                         (,syntax/clambda () ,con)
                         (,syntax/clambda ()
                           ,(cond ((null? maybe-alt)
                                   primop/undefined)
                                  (else
                                   (car maybe-alt))))
                         ,primop/test
                         ,primop/true?
                         ,tested)
          syntax
          shape))

;;; Compute fixed point F for operator H, in CPS style.
;;; Derivation for one-variable case:
;;;     (Y H)
;;;  == (exits 1 Y (clambda (F) F) H)
;;;  == (exits 1 Y (clambda (F) F) (lambda (F) F-val))
;;;  == (exits 1 Y (clambda (F) F) (clambda (C F) (C F-val)))
;;; Extension for multi-variable case:
;;;     (Y H)
;;;  == (exits 1 Y (clambda (F G) ...) H)
;;;  == (exits 1 Y (clambda (F G) ...) (lambda (F G) (values F-val G-val)))
;;;  == (exits 1 Y (clambda (F G) ...) (clambda (C F G) (C F-val G-val)))

;;; (labels ((v1 e1) (v2 e2) ... (vn en)) . body)
;;;   ==> (exits 1 Y (clambda (b v1 v2 ... vn) (b))
;;;                  (clambda (c v1 v2 ... vn) (c (lambda () . body)
;;;                                               e1 e2 ... en)))

(define-exported-compilator (labels specs . body) (syntax shape)
  (let ((body-var (create-variable 'b))
        (bogus-vars (map (lambda (spec)
                           nil)
                     specs))
        (vars (map (lambda (spec)
                     (let ((pat (car spec)))
                       (if (atom? pat) pat (car pat))))
                   specs))
        (vals (map (lambda (spec)
                     (let ((pat (car spec)))
                       (cond ((atom? pat) (cadr spec))
                             (else `(,syntax/named-lambda ,(car pat) ,(cdr pat)
                                       . ,(cdr spec))))))
                   specs)))
    (cond ((null? specs) (alpha-block body syntax shape))
          (else
           (alpha `(,syntax/exits 1
                      ,primop/y
                      (,syntax/clambda (,body-var . ,bogus-vars)
                         (,(variable-name body-var)))
                      (,syntax/lambda ,vars
                         (,primop/values (,syntax/lambda () . ,body)
                                         . ,vals)))
                  syntax
                  shape)))))

;;; (block a . b)
;;;   ==>  (*block (clambda () . b) (clambda () a))

(define-exported-compilator (block . exp-list) (syntax shape)
  (alpha-block exp-list syntax shape))

(define (alpha-block exp-list syntax shape)
  (cond ((null? (cdr exp-list))
         (alpha (car exp-list) syntax shape))
        (else
         (let ((node (alpha (car exp-list) syntax shape)))
           (alpha `(,syntax/exits 1
                                  ,primop/*block
                                  (,syntax/clambda () . ,(cdr exp-list))
                                  (,syntax/lambda () ,node))
                  syntax
                  shape)))))

(define-exported-compilator (define-local-syntax . spec) (syntax shape)
  (set-local-syntax syntax spec)
  (alpha-primop-reference primop/undefined))

(define-exported-compilator (let-syntax specs . body) (syntax shape)
  (let ((new-syntax (make-syntax-table syntax nil)))
    (walk (lambda (spec) (set-local-syntax new-syntax spec))
          specs)
    (alpha-block body new-syntax shape)))

(define (set-local-syntax syntax spec)
  (let ((pat (car spec))
        (body (cdr spec)))
    (receive (sym exp)
             (cond ((pair? pat)
                    (values (car pat) `(macro-expander ,pat ,@body)))
                   (else
                    (values pat (car body))))
      (set (syntax-table-entry syntax sym)
           (eval exp ((*value *t-implementation-env*
                              'env-for-syntax-definition)
                      syntax)))
      sym)))

(define (alpha-top exp syntax)
  (let* ((handle (make-handle))
         (new-syntax (make-syntax-table syntax nil))
         (exp-node (alpha `(,syntax/lambda () ,exp) new-syntax handle)))
    (set (node-parent exp-node) nil)
    (values exp-node handle)))

(define (s-exp->node-tree exp)
  (let* ((handle (make-handle))
         (exp-node (alpha exp *primitive-syntax-table* handle)))
    (cond ((the-free-variables handle)
           => (lamba (vars)
                (bug "free variable(s) ~S in integrated expression ~S"
                     vars exp)))
          (else
           (convert exp-node)
           exp-node))))

;;; The following are not yet implemented:

;;; (let-reference ((a x) (b y)) ...)
;;;   ==>  (*let-reference (lambda (a b) ...)
;;;                        (locative x)
;;;                        (locative y))

;;; (locale () ... (define a x) ... (define b y) ...)
;;;   ==>  (labels ((a (block ... x))
;;;                 (b (block ... y)))
;;;          ...)

;;; (locale var . body)
;;;   ==>  (let ((env (make-locale (environment))))
;;;          (((expression (lambda (var) . body)) env) env)

;;; (expression E) ==> (*expression (lambda (env ... free vars ...) E))
;;; (environment)  ==> (*environment outer-env '(name1 ...) var1 ...)
;;; (define-unless var pred thunk) ==> (*define-unless env 'var pred thunk)
