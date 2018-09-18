(herald convert
        (syntax-table *orbit-syntax-table*))

;;; Orbit compiler, part 1.

;;;  Structure definitions
;;;  Node creation; interconnect manipulation
;;;  Primops
;;;  Alpha-conversion
;;;  SET conversion
;;;  Continuation-passing style (CPS) conversion

(import *t-implementation-env*
        print-type-string
        self-evaluating?
        blockify
        make-table
        table-entry
        make-syntax-descriptor)

(define *empty*
  (object nil ((print self stream) (writes stream "#{Empty}"))))

(define (empty? obj) (eq? obj *empty*))

(define (bug f . rest)
  (apply error (list f "~%  (compiler error)") rest))

;;; ---------------------------------------------------------------------
;;; Variables

;;; This is getting pretty hairy.  What's going on here?

(define used? true?)

(define-structure-type variable
  name          ; Source code name for variable
  id            ; Unique numeric identifier
  binder        ; LAMBDA node which binds this variable
  number        ; K: var = (nth (lambda-variables (variable-binder var)) K)
  refs          ; List of reference nodes n for which (reference-variable n) = var
  type          ; The type of the variable's value at point of binding
  rep           ; Representation for variable's value
  home          ; Register or slot allocated for this variable (?)
  )

(define-methods handle-variable
  ((print self stream)
   (format stream "#{Variable~_~S~_~A}"
           (object-hash self)
           self))
  ((display self stream)        ; hack for ~A (?!)
   (format stream "~S_~S"
           (variable-name self)
           (variable-id self))))

(let ((var (stype-master variable-stype)))
  (set (variable-refs var) '())
  (set (variable-rep var) 'rep/pointer))

(lset *variable-id* 0)

(define (create-variable name)
  (let ((var (make-variable)))
    (set (variable-name var) name)
    (set (variable-id var) *variable-id*)
    (set (variable-type var) 'top?)
    (increment *variable-id*)
    var))

(define (obtain-variable shape name) (shape name))

;;; Create a lexical contour, inferior to SUPER, in which the given
;;; variables are bound.

(define (bind-variables variables binder super)
  (cond ((null? variables) super)
        (else
         (lambda (name)
           (or (find (lambda (var)
                       (and (used? var) (eq? (variable-name var) name)))
                     variables)
               (obtain-variable super name))))))

;;; A "handle" is a repository for variables which are free in the outermost
;;; scope.  After alpha-conversion, an outer LAMBDA-node is created whose
;;; parameters are exactly these variables.

(define (make-handle)
  (let ((table (make-table 'handle))
        (l '()))
    (object (lambda (name)
              (or (table-entry table name)
                  (let ((var (create-variable name)))
                    (push l var)
                    (set (variable-refs var) '())
                    (set (variable-binder var) nil)
                    (set (table-entry table name) var))))
            ((the-free-variables self) l)
            ((print self stream)
             (format stream "#{Handle~_~S}" (object-hash self))))))

(define-operation (the-free-variables handle))

;;; ---------------------------------------------------------------------
;;; Nodes

;;; There are four node types:
;;;  - Literal
;;;  - Variable reference
;;;  - LAMBDA-expression
;;;  - Call

(define-structure-type node
  variant           ; Node type (e.g., the predicate LITERAL-NODE?)
  parent            ; Parent node
  role              ; node == ((node-role node) (node-parent node))
  simplified?       ; True if doesn't need to be simplified
  children          ; List of inferior nodes
  stuff-1           ; Variant components
  stuff-2
  stuff-3
  stuff-4)

(define-methods handle-node
  ((print node stream)
   (print-node (node-variant node) node stream))
  ((disclose node)
   (express node)))           ; For PP, and for CRAWL's P command.

(let ((m (stype-master node-stype)))
  (set (node-parent m) *empty*)
  (set (node-simplified? m) nil))

;;; A "node variant" is a predicate which answers true to nodes which
;;; belong to this variant node type.
;;; It is also an object which handles the CREATE-NODE operation.

(define (create-node-variant id)
  (labels ((self
            (object (lambda (obj)
                      (eq? (node-variant obj) self))
                    ((print self stream)
                     (format stream "#{Node-variant~_~S}" id))
                    ((print-node self node stream)
                     (format stream "#{~S-node~_~S}" id (object-hash node)))
                    ((create-node self q)
                     (let ((node (make-node)))
                       (set (node-variant node) self)
                       (set (node-children node)
                            (do ((i 0 (1+ i))
                                 (l '() (cons *empty* l)))
                                ((= i q) l)))
                       (set *loose* (fx+ *loose* q))
                       node)))))
    (set (table-entry *node-variants* id) self)    ; For dispatches
    self))

(lset *loose* 0)        ; Count of unconnected nodes.

(define-operation (print-node variant node stream))
(define-operation (create-node variant q))

(define *node-variants* (make-table '*node-variants*))

(define (node-field variant field id)
  (object (lambda (node)
            (field (check-arg variant node id)))
          ((setter self)
           (lambda (node val)
             (set (field (check-arg variant node id)) val)))
          ((identification self) id)))

;;; ---------------------------------------------------------------------
;;; Relations

;;; A "relation" is a selector procedure - something appropriate to put in
;;; the ROLE slot of a node.

(define (make-relation id variant index pred)
  (object (lambda (node)
            (let ((node (check-arg variant node id)))
              (nth (node-children node) index)))
          ((relate self parent child)
           (proclaim-empty (swap (node-parent child) parent))
           (proclaim-empty (self parent))
           (set *loose* (fx- *loose* 1))
           (set (nth (node-children parent) index) child)
           (set (node-role child) self))
          ((pred self) t)               ; hack
          ((relation-index self) index) 
          ((print self stream)
           (format stream "#{Relation~_~S}" id))))

(define-operation (relate relation parent child))
(define-operation (relation-index relation))

;;; Disconnect node from its parent.

(define (detach node)
  (set (nth (node-children (node-parent node))
            (relation-index (node-role node)))
       *empty*)
  (set *loose* (fx+ *loose* 1))
  (set (node-role node) nil)
  (set (node-parent node) *empty*)
  node)

;;; Replace node in tree with value of applying proc to node.
;;; Note the fact that a change has been made, for the simplifier.

(define (replace node proc)
  (let ((role (node-role node))
        (parent (node-parent node)))
    (do ((p parent (node-parent p)))
        ((or (null? p) (not (node-simplified? p))))
      (set (node-simplified? p) nil))
    (detach node)
    (relate role parent (proc node))))

(define (proclaim-empty probe)
  (cond ((not (empty? probe))
         (bug "not empty - ~S" probe))))

;;; ---------------------------------------------------------------------
;;; Specific node types

;;; Literal:
;;;   value - either a primop or the object which has been quoted.

(define literal-node? (create-node-variant 'literal))

(define literal-value
  (node-field literal-node? node-stuff-1 'literal-value))

(define (create-literal-node lit)
  (let ((node (create-node literal-node? 0)))
    (set (literal-value node) lit)
    node))

;;; Variable reference
;;;   variable - the structure for the variable to which this node is a
;;;              reference
;;;   type     - data type required by the node's parent (?)

(define reference-node? (create-node-variant 'reference))

(define reference-variable
  (node-field reference-node? node-stuff-1 'reference-variable))

(define reference-via
  (node-field reference-node? node-stuff-2 'reference-via))

(define (create-reference-node variable)
  (let ((node (create-node reference-node? 0)))
    (set (reference-variable node) variable)
    (push (variable-refs variable) node)
    node))

;;; Lambda-expression
;;;   variables - list of variables which are bound by this lambda
;;;               (the first variable gets bound to the procedure itself)
;;;   body      - the node for the body (after CPS, always a call node)

(define lambda-node? (create-node-variant 'lambda))

(define lambda-variables
  (node-field lambda-node? node-stuff-1 'lambda-variables))

(define lambda-env
  (node-field lambda-node? node-stuff-2 'lambda-env))

(define lambda-cdepth
  (node-field lambda-node? node-stuff-3 'lambda-cdepth))

(define lambda-body
  (make-relation 'lambda-body lambda-node? 0 nil))

(define (create-lambda-node vars)
  (let ((node (create-node lambda-node? 1))
        (vars (cond ((null? (car vars))
                     (cons (create-variable 'p) (cdr vars)))
                    (else vars))))
    (set (lambda-variables node) vars)
    (do ((vars vars (cdr vars))
         (n 0 (fx+ n 1)))
        ((null? vars) node)
      (let ((var (car vars)))
        (cond ((used? var)
               (set (variable-binder var) node)
               (set (variable-number var) n)))))))

;;; Call
;;;   exits      - the number of initial arguments that are continuations
;;;   complexity - an integer estimating evaluation complexity
;;;   proc+args  - list of child nodes

(define call-node? (create-node-variant 'call))

(define call-exits
  (node-field call-node? node-stuff-2 'call-exits))

(define call-complexity
  (node-field call-node? node-stuff-1 'call-complexity))

(define-integrable call-proc+args node-children)

(define-integrable (call-args node)
  (cdr (call-proc+args node)))

;;; Argument relations are created on demand whenever a newly created
;;; call node is going to have mre arguments than any previous call node
;;; has had.

;;; This is not done quite the right way.  Fix.

(define-predicate call-arg?)

(define (make-arg-relation i)
  (make-relation `(call-arg ,i) call-node? i call-arg?))

(define call-arg-number relation-index)

(define *call-arg-relations*
  (list (make-arg-relation 0)))

(define call-proc (car *call-arg-relations*))

(define (call-arg i)
  (nth *call-arg-relations* i))

(define (relate-call-proc+args parent children)
  (relate call-proc parent (car children))
  (relate-call-args parent (cdr children)))

(define (relate-call-args node args)
  (walk (lambda (relation child)
          (relate relation node child))
        (cdr *call-arg-relations*)
        args))

(define (replace-call-args node new-args)
  (walk detach (call-args node))
  (relate-shorter-call-args node new-args))

(define (relate-shorter-call-args node new-args)
  ;; Shorten the list of empties, if necessary.
  (modify (cdr (node-children node))
          (lambda (l)
            (let ((n (fx- (length l) (length new-args))))
              (set *loose* (fx- *loose* n))
              (nthcdr l n))))
  (relate-call-args node new-args))

(define (create-call-node n exits)
  (iterate loop ((i 0)
                 (l *call-arg-relations*))
    (cond ((fx= i n)
           (let ((node (create-node call-node? n)))
             (set (call-exits node) exits)
             (set (call-complexity node) 0)
             node))
          (else
           (cond ((null? (cdr l))
                  ;; Avoid FORCE/DELAY - they make it harder to use MAP
                  (set (cdr l)
                       (list (make-arg-relation (fx+ i 1))))))
           (loop (fx+ i 1) (cdr l))))))

;;; ---------------------------------------------------------------------
;;; Tree-walk utilities

;;; These two routines coordinate with the macros defined in SYNTAX.T.

(define (make-dispatcher id)
  (let ((table (make-table id)))
    (object (lambda (node)
              (let* ((v (node-variant node))
                     (probe (table-entry table v)))
                (cond ((not probe)
                       (bug "no ~S specialist for node type ~S" id v))
                      (else probe))))
            ((get-dispatch-table self) table))))

(define (set-node-dispatch generalist type-name proc)
  (set (table-entry (get-dispatch-table (get-dispatcher generalist))
                    (table-entry *node-variants* type-name))
       proc))

(define-operation (get-dispatcher generalist))

(define-operation (get-dispatch-table dispatcher))

;;; Multiple-value stuff for T 2 - move this somewhere else!

(lset **a1** '**a1**)
(lset **a2** '**a2**)
(lset **a3** '**a3**)
(lset **a4** '**a4**)
(lset **a5** '**a5**)

(define (losing-receive **nvals** nvars)
  (error "wrong number of return values - expected ~s, received ~s"
         nvars **nvals**))

;;; ---------------------------------------------------------------------
;;; Primitive syntax

(define *primitive-syntax-table*
  (make-syntax-table false '*primitive-syntax-table*))

;;; External syntax descriptors

(define *exported-syntax* '(quote lambda named-lambda if labels block set-var))

(walk (lambda (sym)
        (set (syntax-table-entry *primitive-syntax-table* sym)
             (syntax-table-entry *standard-syntax-table* sym)))
      *exported-syntax*)

(apply (lambda (q l n i y b s)
         (define quote-syntax        q)
         (define lambda-syntax       l)
         (define named-lambda-syntax n)
         (define if-syntax           i)
         (define labels-syntax       y)
         (define block-syntax        b)
         (define set-var-syntax      s))
      (map (lambda (sym)
             (syntax-table-entry *primitive-syntax-table* sym))
           *exported-syntax*))

;;; Internal syntax descriptors

(define *internal-syntax* '(clambda exits primop))

(walk (lambda (sym)
        (set (syntax-table-entry *primitive-syntax-table* sym)
             (make-syntax-descriptor sym)))
      *internal-syntax*)

(apply (lambda (c e p)
         (define clambda-syntax c)      ; CPS LAMBDA
         (define exits-syntax   e)      ; CPS call with N continuations
         (define primop-syntax  p))     ; Primop reference
      (map (lambda (sym)
             (syntax-table-entry *primitive-syntax-table* sym))
           *internal-syntax*))



;;; ---------------------------------------------------------------------
;;; Alpha-conversion

;;; --- Dispatch.

;;; ALPHA - return a node.

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
         (alpha-primop exp))
        (else
         (alpha (syntax-error "uncompilable object~%  ~S" exp) syntax shape))))

(define (alpha-special-form descr exp syntax shape)
  (select descr
    ((quote-syntax)
     (alpha-literal (cadr exp)))
    ((lambda-syntax)
     (alpha-lambda nil        (cons 'k (cadr exp))  (cddr exp)  syntax shape))
    ((named-lambda-syntax)
     (alpha-lambda (cadr exp) (cons 'k (caddr exp)) (cdddr exp) syntax shape))
    ((clambda-syntax)
     (alpha-lambda nil        (cadr exp)            (cddr exp)  syntax shape))
    ((if-syntax)
     (alpha-if (cadr exp) (caddr exp) (cdddr exp) syntax shape))
    ((labels-syntax)
     (alpha-labels (cadr exp) (cddr exp) syntax shape))
    ((block-syntax)
     (alpha-block (cdr exp) syntax shape))
    ((set-var-syntax)
     (alpha-call (cons set-primop (cdr exp)) syntax shape))
    ;; define-var-syntax, lset-syntax
    ((exits-syntax)
     (alpha-exits exp syntax shape))
    ((primop-syntax)
     (cond ((support-env-entry *primitive-support-env* (cadr exp))
            => (lambda (primop) (alpha-primop primop)))
           (else (alpha-error "no such primop - ~s" exp))))
    ;; Internal
    (else
     (cond ((macro-expander? descr)
            (alpha (invoke-macro-expander descr exp) syntax shape))
           (else
            (alpha (syntax-error "special form unknown to this compiler~%  ~S"
                                 exp)
                   syntax
                   shape))))))

;;; --- Primitive node converters: literal, reference, lambda, call

(define (alpha-literal lit)
  (create-literal-node lit))

(define (alpha-primop primop)
  (create-literal-node primop))

(define (alpha-reference exp shape)
  (create-reference-node (obtain-variable shape exp)))

;;; (lambda (a b c . rest) . body)
;;;  ==>  (n-ary (lambda (a b c rest) . body))

(define (alpha-lambda proc-name var-names body syntax shape)
  (cond ((proper-list? var-names)
         (let* ((vars (map (lambda (name)
                             (cond ((null? name) nil)
                                   ((variable? name) name)  ;for alpha-top
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
              (alpha `(,n-ary-primop
                       (,named-lambda-syntax ,proc-name
                                             ,(append-reverse! z (list l))
                          . ,body))
                     syntax
                     shape))))))

;;; (foo x y)  ==>  (exits 1 %continuation x y)

(define (alpha-call exp syntax shape)
  (alpha-call-1 (car exp)
                (cons continuation-primop (cdr exp))
                1
                syntax
                shape))

(define (alpha-exits exp syntax shape)
  (alpha-call-1 (caddr exp) (cdddr exp) (cadr exp) syntax shape))

(define (alpha-call-1 proc args exits syntax shape)
  (let ((node (create-call-node (fx+ 1 (length args)) exits)))
    (relate call-proc node (alpha proc syntax shape))
    (relate-call-args node (map (lambda (arg) (alpha arg syntax shape)) args))
    node))

(define (alpha-error . rest)
  (apply error rest)
  (alpha-primop undefined-primop))

;;; --- Everything else is done source-to-source.

;;; (if p c a)
;;;   ==> (if (lambda () a) (lambda () c) true? p)
;;; (if p c)
;;;   ==> (if p c $)

(define (alpha-if test con maybe-alt syntax shape)
  (alpha `(,exits-syntax 2
                         ,if-primop
                         (,clambda-syntax () ,con)
                         (,clambda-syntax ()
                           ,(cond ((null? maybe-alt)
                                   undefined-primop)
                                  (else
                                   (car maybe-alt))))
                         ,true?-primop
                         ,test)
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
;;;   ==> (exits 1 Y (clambda (v1 v2 ... vn) . body)
;;;                  (clambda (c v1 v2 ... vn) (c e1 e2 ... en)))

(define (alpha-labels specs body syntax shape)
  (let ((vars (map (lambda (spec)
                     (let ((pat (car spec)))
                       (if (atom? pat) pat (car pat))))
                   specs))
        (vals (map (lambda (spec)
                     (let ((pat (car spec)))
                       (cond ((atom? pat) (cadr spec))
                             (else `(,named-lambda-syntax ,(car pat) ,(cdr pat)
                                       . ,(cdr spec))))))
                   specs)))
    (cond ((null? specs) (alpha-block body syntax shape))
          (else
           (alpha `(,exits-syntax 1
                      ,Y-primop (,clambda-syntax ,vars . ,body)
                                (,lambda-syntax ,vars
                                   (,values-primop . ,vals)))
                  syntax
                  shape)))))

;;; (block a . b)
;;;   ==>  (%block (clambda () . b) a)

(define (alpha-block exp-list syntax shape)
  (cond ((null? (cdr exp-list))
         (alpha (car exp-list) syntax shape))
        (else
         (alpha `(,exits-syntax 1
                                ,block-primop
                                (,clambda-syntax () . ,(cdr exp-list))
                                ,(car exp-list))
                syntax
                shape))))

;;; --- Top level entry!

;;; Returns two values: the node (with a LAMBDA wrapped around it),
;;; and a list of free variables.

(define (alpha-top exp syntax values)
  (let* ((h (make-handle))
         (exp-node (alpha `(,lambda-syntax () ,exp) syntax h)))
    (set (node-parent exp-node) nil)
    (values exp-node (the-free-variables h))))

(define (alpha-integrable exp)
  (let* ((h (make-handle))
         (node (alpha exp *primitive-syntax-table* h))
         (vars (early-bind (the-free-variables h) *primitive-support-env*)))
    (cond ((not (null? vars))
           (bug "free variables in integrable procedure definition~%  ~s"
                exp)))
    node))

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
;;; (local var)    ==> (*local env 'var)
;;; (lset var val) ==> (block (local var) (set var val))

;;; ---------------------------------------------------------------------
;;; Deal with implicit cell dereferencing.

(define (assignment-analyze node)
  (cond ((lambda-node? node)
         (let ((assigned-vars (filter variable-assigned?
                                      (lambda-variables node))))
           (if (not (null? assigned-vars))
               (introduce-cells node assigned-vars))))
        ((call-node? node)
         (let ((proc (call-proc node)))
           (cond ((and (literal-node? proc)
                       (eq? (literal-value proc) let-reference-primop))
                  (replace proc
                           (lambda (lit)
                             (erase lit)
                             (create-literal-node call-primop)))
                  (let ((vars (cdr (lambda-variables (car (call-args node))))))
                    (hack-references vars vars)))))))
  (walk assignment-analyze (node-children node)))

;;; Returns true if there are any SET's or LOCATIVE's to var.

(define (variable-assigned? var)
  (and (used? var)
       (any? (lambda (ref)
               (and (eq? (node-role ref) (call-arg 2))
                    (let ((proc (call-proc (node-parent ref))))
                      (and (literal-node? proc)
                           (or (eq? (literal-value proc) set-primop)
                               (eq? (literal-value proc) locative-primop))))))
             (variable-refs var))))

;;; (lambda (x) ... x ... (set x ...) ...)
;;;   ==>  (lambda (x)
;;;          (let ((x' (make-cell x)))
;;;             ... (value x) ... (set-value x ...) ...))

(define (introduce-cells node vars)          
  (let ((new-vars (map (lambda (var)
                         (create-variable (variable-name var)))
                       vars))
        (args (map (lambda (var)
                     (let ((mnode (create-call-node 3 1)))
                       (relate call-proc mnode 
                               (create-literal-node make-locative-primop))
                       (relate (call-arg 1) mnode
                               (create-literal-node continuation-primop))
                       (relate (call-arg 2) mnode
                               (create-reference-node var))
                        mnode))
                    vars)))  
    (replace (lambda-body node)
             (lambda (body)
               (let ((lnode (create-lambda-node 
                                `(,nil ,(create-variable 'k) ,@new-vars)))
                     (cnode (create-call-node (fx+ (length vars) 2) 1)))
                 (relate call-proc cnode lnode)
                 (relate-call-args cnode 
                    (cons (create-literal-node continuation-primop) args))
                 (relate lambda-body lnode body)
                 cnode)))    
    (hack-references vars new-vars args)))
    

;;; Replace references to variables with indirections through locatives.
;;;   x              ==>  (value x')
;;;   (set-var x y)  ==>  (set-value x' y)
;;;   (locative x)   ==>  x'

(define (hack-references vars new-vars locs) 
  (walk (lambda (var new-var loc)
          (walk (lambda (ref)           
                  (cond ((neq? (node-role ref) (call-arg 2))
                         (replace ref (dereference-var new-var)))
                        (else
                         (let* ((parent (node-parent ref))
                                (proc (call-proc parent)))
                           (cond ((not (literal-node? proc))
                                  (replace ref (dereference-var new-var)))
                                 ((eq? (literal-value proc) set-primop)
                                  (assigner (create-reference-node new-var) parent))
                                 ((eq? (literal-value proc) locative-primop)
                                  (erase-all parent)
                                  (replace ref (lambda (ref) (ignore ref)
                                                 (create-reference-node new-var))))
                                 ((eq? parent loc))  ;; this was introduced
                                 (else
                                  (replace ref (dereference-var new-var))))))))
                (variable-refs var)))
        vars
        new-vars
        locs))  

(define (dereference-var new-var)
    (lambda (ref)
        (erase ref)
        (dereferencer (create-reference-node new-var)))) 


;;;  x ==> (value x')

(define (dereferencer new-ref)
  (let ((node (create-call-node 3 1)))
    (relate call-proc node
            (create-literal-node value-primop))
    (relate (call-arg 1) node (create-literal-node continuation-primop))
    (relate (call-arg 2) node new-ref)
    node))

;;;  (set x foo) ==> (set-value x' foo)

(define (assigner new-ref parent)
  (set (literal-value (call-proc parent)) set-value-primop)
  (replace ((call-arg 2) parent)
           (lambda (ref)
             (erase ref)
             new-ref)))

;;; ---------------------------------------------------------------------
;;; CPS conversion

;;; These routines are all destructuve; none returns an interesting value.

(define (convert node)
  (cond ((lambda-node? node)
         (convert-lambda node (cadr (lambda-variables node))))))

;;; (lambda () x)  ==>  (clambda (c) (c x))

(define (convert-lambda node cvar)
  (let ((body (lambda-body node)))
    (cond ((call-node? body)
           (convert-call body cvar))
          (else
           (convert body)
           (replace body
                    (lambda (body)
                      (let ((new-body (create-call-node 2 0)))
                        (relate call-proc new-body
                                (create-reference-node cvar))
                        (relate (call-arg 1) new-body body)
                        new-body)))))))

;;; Eliminate composition by creating a continuation for each argument
;;; which is a call.
;;; Also, propagate our continuation into the call's exit arguments.
;;; "Cont" is either a variable structure or a LAMBDA-node.  Kinda kludgey.

;;; YUCK.  What a mess.

(define (convert-call node cont)
  (cond ((and (not (variable? cont))
              (fx> (call-exits node) 1))
         (let* ((jvar (create-variable 'j))         ;join point
                (lnode (create-lambda-node (list nil jvar)))
                (cnode (create-call-node 2 1)))
           (relate call-proc cnode lnode)           ;((lambda (j) node) cont)
           (relate (call-arg 1) cnode cont)     ;what J gets bound to
           (replace node
             (lambda (node)
               (relate lambda-body lnode node)
               cnode))
           (really-convert-call node jvar)))
        (else
         (really-convert-call node cont))))

;;; (f *cont* (g *cont* ...) ...) => (g (lambda (val) (f *cont* val ...)) ...)

(define (really-convert-call node cont)
  (walk (lambda (arg)
          (cond ((call-node? arg)
                 (let* ((var (create-variable 'v))
                        (cont (create-lambda-node (list (create-variable 'c)
                                                        var))))
                   (replace arg     ;Replace the sub-call with result var ref.
                     (lambda (arg)
                       (create-reference-node var)))
                   (replace node    ;Invert child and parent in tree.
                     (lambda (node)
                        (relate lambda-body cont node)
                        arg))
                   (convert-call arg cont)))
                ((and (literal-node? arg)
                      (eq? (literal-value arg) continuation-primop))
                 (replace arg
                          (lambda (arg)
                            (erase arg)
                            (if (variable? cont)
                                (create-reference-node cont)
                                cont))))
                ((lambda-node? arg)
                 (let ((n (call-arg-number (node-role arg))))
                   (cond ((and (fx> n 0)
                               (fx<= n (call-exits node)))
                          (convert-lambda arg cont))  ;propagate cont into body
                         (else
                          (convert arg)))))))
          (sort (call-proc+args node)
                (lambda (node1 node2)
                  (fx< (node-rank node1)
                       (node-rank node2)))))
  (cond ((and (not (variable? cont))
              (empty? (node-parent cont)))
         (erase-all cont))))

;;; Evaluate the most complex arguments first.

(define (node-rank node)
  (cond ((call-node? node)
         (if *left-to-right?*
             (call-arg-number (node-role node)))
             (fx- 0 (call-complexity node)))
        (else 0)))

(lset *left-to-right?* nil)

;;; Complexity analysis.

(define (complexity-analyze node)
  (cond ((reference-node? node) 1)
        (else
         (do ((q 0 (fx+ q (complexity-analyze (car l))))
              (l (node-children node) (cdr l)))
             ((null? l)
              (if (call-node? node)
                  (set (call-complexity node) q))
              q)))))

;;; ---------------------------------------------------------------------
;;; Erase node structure.  Mostly to update the REFS slot of
;;; variables free to this node.

(define (erase-all node)
  (walk erase-all (node-children node))
  (erase node))

(define (erase node)
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
  (set (node-role node) '<erased>))

;;; ---------------------------------------------------------------------
;;; Convert node to s-expression

(define-generalist express dispatch
  (lambda (node)
    (cond ((node? node) ((dispatch node) node))
          ((empty? node) node)
          (else (bug "bogus node")))))

(define-specialist ((express literal) node)
  (let ((lit (literal-value node)))
    (cond ((and lit (self-evaluating? lit)) lit)
          ((primop? lit) `(primop ,(identification lit)))
          (else `',lit))))

(define-specialist ((express reference) node)
  (variable-unique-name (reference-variable node)))

;;; Returns a lexically unique name for the variable.

(define (variable-unique-name var)
  (and (used? var)
       (let ((name (variable-name var)))
         (cond ((and (variable-binder var)
                     (or (memq? name '(v c k p j))
                         (apparent? name (variable-binder var)))
                     (concatenate-symbol name "_" (variable-id var))))
               (else
                name)))))

(define (apparent? name node)
  (let ((node (node-parent node)))
    (and node
         (or (and (lambda-node? node)
                  (any? (lambda (var)
                          (and (used? var) (eq? name (variable-name var))))
                        (lambda-variables node)))
             (apparent? name node)))))

(define-specialist ((express lambda) node)
  (let ((vars (lambda-variables node))
        (body (express (lambda-body node))))
    (cond ((and (not (null? (cdr vars)))
                (used? (cadr vars))
                (null? (variable-refs (cadr vars))))
           `(lambda ,(map variable-unique-name (cddr vars))
              ,body))
          (else
           `(clambda ,(map variable-unique-name (cdr vars))
              ,body)))))

(define-specialist ((express call) node)
  (let ((proc (call-proc node)))
    (cond ((and (node? proc)
                (lambda-node? proc)
                (fx= (length (call-args node))
                     (length (cdr (lambda-variables proc)))))
           `(let ,(map (lambda (v n) `(,(variable-unique-name v) ,(express n)))
                       (cdr (lambda-variables proc))
                       (call-args node))
              ,(express (lambda-body proc))))
          ((let ((cont ((call-arg 1) node)))
             (and (literal-node? cont)
                  (eq? (literal-value cont) continuation-primop)))
           `(,(express (call-proc node))
              ,@(map express (cdr (call-args node)))))
          (else
           `(exits ,(call-exits node)
                   ,@(map express (call-proc+args node)))))))

;;; ---------------------------------------------------------------------
;;; Print CPS code in linear form.

(define (pp-cps node)
  (pp-cps-1 node 2))

(define (pp-cps-1 node indent-to)
  (let ((z (pp-cps-2 node))
        (stream (terminal-output)))
    (cond ((lambda-node? node)
           (let ((vars (lambda-variables node)))
             (set (hpos stream) indent-to)
             (writec stream #\()
             (write stream (map variable-unique-name vars))
             (set (hpos stream) (fx+ indent-to 18))     ;format sux
             (pretty-print (pp-cps-2 (lambda-body node)) stream)    ;for '
             (format stream ")~%")
             (walk (lambda (node) (pp-cps-1 node (fx+ indent-to 1)))
                   (call-proc+args (lambda-body node))))))
    z))

(define (pp-cps-2 node)
  (cond ((not (node? node))
         `(not-a-node ,node))
        (else
         (xselect (node-variant node)
           ((lambda-node?)
            (lambda-name node))
           ((literal-node?)
            (let ((val (literal-value node)))
              (cond ((primop? val) (identification val))
                    (else `',val))))
           ((reference-node?)
            (express node))
           ((call-node?)
            (map pp-cps-2 (call-proc+args node)))))))

(define (lambda-name node)
  (variable-unique-name (car (lambda-variables node))))

;;; ---------------------------------------------------------------------
;;; Little utilities.

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

;(define (delete!-nth l n) ...)

;;; ---------------------------------------------------------------------
;;; Phase coordination for part 1.

(define (pass-1 exp syntax)
  (alpha-top exp syntax
    (lambda (top-node free-vars)
      (format t "~&Free variables (~s): ~s~2%"
              (object-hash free-vars)
              (map variable-name free-vars))
      (assignment-analyze top-node)

;      (format t "~&Before CPS (~s):~%  " (object-hash top-node))
;      (pretty-print (express top-node) (standard-output))
;      (format t "~2%")

      (complexity-analyze top-node)
      (convert top-node)
      (set (node-role top-node) 'top)
      (set (node-parent top-node) nil)

 ;     ;(format t "After CPS:~%  ")
 ;     ;(pretty-print (express top-node) (standard-output))
 ;     ;(format t "~2%")

 ;     (format t "Linearized CPS code:~%")
 ;     (pp-cps top-node)
 ;     (format t "~%")

      (values top-node free-vars))))

(define (test-p1 exp)
  (bind ((*variable-id* 1))
    (receive (node free-vars)
             (pass-1 (or (disclose exp) exp)
                     *standard-syntax-table*)
      (ignore free-vars)
      node)))
