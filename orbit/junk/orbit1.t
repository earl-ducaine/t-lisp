(herald orbit
        (env orbit))

;;;; A Scheme compiler

(import *t-implementation-env*
        self-evaluating?
        print-type-string)

(define *empty*
  (object nil ((print self stream) (format stream "#{Empty}"))))

(define (empty)
  *empty*)

(define (empty? obj)
  (eq? obj *empty*))

;;; ---------------------------------------------------------------------
;;; Nodes

(define-structure-type node
  type              ; Node type
  parent            ; Parent
  role              ; Role with respect to parent
  children          ; Inferior nodes
  shape             ; Lexical environment
  map               ; Map variables to routes
  cps?              ; True if post-CPS
  refs              ; Variables referred to...
  target            ; Place where value is going
  stuff-1           ; Variant components
  stuff-2
  stuff-3
  stuff-4
  )

(let ((m (stype-master node-stype)))
  (set (node-parent m) (empty))
  (set (node-role m) nil)
  (set (node-cps? m) nil))

(set (stype-handler node-stype)
     (handler ((print node stream)
               (print-node (node-type node) node stream))
              ((disclose node)
               (express node))          ; for P command in CRAWL
              ((structure-type node) node-stype)))

(define *node-pool* (make-pool 'node-pool make-node))

(define *node-types* (make-table '*node-types*))

(define (create-node-type id)
  (labels ((self
            (object (lambda (obj)
                      (eq? (node-type obj) self))
                    ((print self stream)
                     (format stream "#{Node-type~_~S}" id))
                    ((print-node self node stream)
                     (format stream "#{~S-node~_~S}" id (object-hash node)))
                    ((create-node self q)
                     (let ((node (obtain-from-pool *node-pool*)))
                       (set (node-type node) self)
                       (set (node-children node)
                            (do ((i 0 (1+ i))
                                 (l '() (cons (empty) l)))
                                ((= i q) l)))
                       node)))))
    (set (table-entry *node-types* id) self)
    self))

(define-operation (print-node type node stream))
(define-operation (create-node type q))

(define (delete-node node)
  (copy-structure! node (stype-master node-stype))
  (return-to-pool *node-pool* node))

(define (node-field type field id)
  (object (lambda (node)
            (field (check-arg type node id)))
          ((setter self)
           (lambda (node val)
             (set (field (check-arg type node id)) val)))
          ((identification self) id)))

;;; ---------------------------------------------------------------------
;;; Relations

(define (make-relation id type index pred)
  (object (lambda (node)
            (let ((node (check-arg type node id)))
              (nth (node-children node) index)))
          ((relate self parent child)
           (proclaim-empty (swap (node-parent child) parent))
           (proclaim-empty (self parent))
           (set (nth (node-children parent) index) child)
           (set (node-role child) self))
          ((pred self) t)               ; hack
          ((relation-index self) index) 
          ((print self stream)
           (format stream "#{Relation~_~S}" id))))

(define-operation (relate relation parent child))
(define-operation (relation-index relation))

(define (proclaim-empty probe)
  (cond ((not (empty? probe))
         (error "not empty - ~S" probe))))

;;; ---------------------------------------------------------------------
;;; Specific node types

;;; Literal

(define literal-node? (create-node-type 'literal))

(define literal-value
  (node-field literal-node? node-stuff-1 'literal-value))

(define (create-literal-node lit)
  (let ((node (create-node literal-node? 0)))
    (set (literal-value node) lit)
    node))

;;; Variable reference

(define reference-node? (create-node-type 'reference))

(define reference-variable
  (node-field reference-node? node-stuff-1 'reference-variable))
(define reference-route
  (node-field reference-node? node-stuff-2 'reference-route))

(define (create-reference-node variable)
  (let ((node (create-node reference-node? 0)))
    (set (reference-variable node) variable)
    (push (variable-refs variable) node)
    node))

;;; Assignment
;;; KIND is one of {SET, LSET, DEFINE}.

(define assignment-node? (create-node-type 'assignment))

(define assignment-kind
  (node-field assignment-node? node-stuff-1 'assignment-kind))
(define assignment-variable
  (node-field assignment-node? node-stuff-2 'assignment-variable))
(define assignment-route
  (node-field assignment-node? node-stuff-3 'assignment-route))

(define assignment-body
  (make-relation 'assignment-body assignment-node? 0 nil))

(define (create-assignment-node variable kind)
  (let ((node (create-node assignment-node? 1)))
    (set (assignment-variable node) variable)
    (set (assignment-kind  node) kind)
    (push (variable-sets variable) node)
    node))

;;; Lambda-expression

(define lambda-node? (create-node-type 'lambda))

(define lambda-continuation?
  (node-field lambda-node? node-stuff-1 'continuation?))
(define lambda-variables
  (node-field lambda-node? node-stuff-2 'lambda-variables))
(define lambda-lexpr?
  (node-field lambda-node? node-stuff-3 'lambda-lexpr?))
(define lambda-sequence
  (node-field lambda-node? node-stuff-4 'lambda-sequence))

(define-integrable (env-var       vars) (car vars))
(define-integrable (rest-var      vars) (cadr vars))
(define-integrable (required-vars vars) (cddr vars))

(define lambda-body
  (make-relation 'lambda-body lambda-node? 0 nil))

(define (create-lambda-node vars)
  (let ((node (create-node lambda-node? 1)))
    (do ((v vars (cdr v))
         (a '() (cons (create-variable (car v) node) a)))
        ((atom? v)
         (set (lambda-lexpr? node) v)
         (set (lambda-variables node)
              (cons* (create-variable '=env= node)
                     (create-variable v node)
                     (reverse! a)))))
    node))

;;; Locale-expression

(define locale-node? (create-node-type 'locale))

(define locale-variable
  (node-field locale-node? node-stuff-1 'locale-variable))
(define locale-refs
  (node-field locale-node? node-stuff-2 'locale-refs))

(define locale-body
  (make-relation 'locale-body locale-node? 0 nil))

(define (create-locale-node v)
  (let ((node (create-node locale-node? 1)))
    (set (locale-variable node)
         (cond ((null? v) nil)
               (else (create-variable v node))))
    (set (locale-refs node) '())
    node))

;;; Call

(define call-node? (create-node-type 'call))

(define call-return?            ; Filled in by CPS conversion
  (node-field call-node? node-stuff-1 'call-return?))

(define-integrable call-proc+args node-children)

(define-integrable (call-args node)
  (cdr (call-proc+args node)))

(define-predicate call-arg?)

(define (make-arg-relation i)
  (make-relation `(call-arg ,i) call-node? i call-arg?))

(define call-proc
  (make-arg-relation 0))

(define call-arg-number relation-index)

(define *call-arg-relations* (list call-proc))

(define (relate-call-proc+args parent children)
  (relate call-proc parent (car children))
  (relate-call-args parent (cdr children)))

(define (relate-call-args parent args)
  (walk (lambda (relation child)
          (relate relation parent child))
        (cdr *call-arg-relations*)
        args))
        
(define (create-call-node n)
  (iterate loop ((i 0)
                 (l *call-arg-relations*))
    (cond ((fx= i n)
           (create-node call-node? n))
          (else
           (cond ((null? (cdr l))
                  (set (cdr l)
                       (list (make-arg-relation (fx+ i 1))))))
           (loop (fx+ i 1) (cdr l))))))

;;; ---------------------------------------------------------------------
;;; Variables

(define-structure-type variable
  name
  binder
  refs
  sets                  ; not used after cps conversion!
  defs
  rep
  home)

(let ((m (stype-master variable-stype)))
  (set (variable-refs m) '())
  (set (variable-sets m) '()))

(define-methods handle-variable
  ((print self stream)
   (format stream "#{Variable~_~S~_~S}"
           (object-hash self)
           (variable-name self))))

(define (obtain-variable shape name)
  (xselect (node-type shape)
    ((lambda-node?)
     (or (find (lambda (a) (eq? (variable-name a) name))
               (lambda-variables shape))
         (obtain-variable (node-shape shape) name)))
    ((locale-node?)
     (or (find (lambda (a) (eq? (variable-name a) name))
               (locale-refs shape))
         (let ((a (create-variable name shape)))
           (push (locale-refs shape) a)
           a)))))

(define (create-variable name binder)
  (let ((variable (make-variable)))
    (set (variable-name   variable) name)
    (set (variable-binder variable) binder)
    variable))

;;;

(define (shape-node? node)
  (select (node-type node)
    ((lambda-node? locale-node?) t)
    (else nil)))

;;; Random list utilities which don't properly belong here.

(define (find pred l)
  (iterate loop ((l l))
    (cond ((null? l) nil)
          ((pred (car l)) (car l))
          (else (loop (cdr l))))))

(define (terminator l)
  (do ((l l (cdr l)))
      ((atom? l) l)))

;;; ---------------------------------------------------------------------
;;; Alpha-conversion

(define (alpha-top exp syntax)
  (let ((shape (create-locale-node nil)))
    (let ((node (alpha exp syntax shape)))
      (relate locale-body shape node)
      node)))

;;; --- Dispatch.

;;; ALPHA - return a node.

(define (alpha exp syntax shape)
  (let ((node (really-alpha exp syntax shape)))
    (set (node-shape node) shape)
    node))

(define (really-alpha exp syntax shape)
  (cond ((self-evaluating? exp)
         (alpha-literal exp))
        ((symbol? exp)
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
        (else
         (alpha (syntax-error "uncompilable object~%  ~S" exp) syntax shape))))

(let ((s (env-syntax-table *system-env*)))
  (define *quote-syntax*        (syntax-table-entry s 'quote))
  (define *lambda-syntax*       (syntax-table-entry s 'lambda))
  (define *locale-syntax*       (syntax-table-entry s 'locale))
  (define *set-syntax*          (syntax-table-entry s 'set))
  (define *lset-syntax*         (syntax-table-entry s 'lset))
  (define *define-syntax*       (syntax-table-entry s 'define))
  (define *if-syntax*           (syntax-table-entry s 'if))
  (define *block-syntax*        (syntax-table-entry s 'block))
  (define *define-macro-syntax* (syntax-table-entry s 'define-macro)))

(define (alpha-special-form descr exp syntax shape)
  (select descr
    ((*quote-syntax*)
     (alpha-literal (cadr exp)))
    ((*lambda-syntax*)
     (alpha-lambda (cadr exp) (cddr exp) nil syntax shape))
    ((*locale-syntax*)
     (alpha-locale (cadr exp) (cddr exp) syntax shape))
    ((*set-syntax*)
     (alpha-set (cadr exp) (caddr exp) syntax shape))
    ((*lset-syntax*)
     (alpha-lset (cadr exp) (caddr exp) syntax shape))
    ((*define-syntax*)
     (alpha-define (cadr exp) (cddr exp) syntax shape))
    ((*if-syntax*)
     (alpha-if (cadr exp) (caddr exp) (cdddr exp) syntax shape))
    ((*block-syntax*)
     (alpha-block (cdr exp) syntax shape))
    ((*define-macro-syntax*)
     (alpha-define-macro (cadr exp) (cddr exp) syntax shape))
    (else
     (cond ((macro-expander? descr)
            (alpha (invoke-macro-expander descr exp) syntax shape))
           (else
            (alpha (syntax-error "special form unknown to this compiler~%  ~S"
                                 exp)
                   syntax
                   shape))))))

;;; --- Specialist routines.

(define (alpha-literal lit)
  (create-literal-node lit))

(define (alpha-reference exp shape)
  (create-reference-node (obtain-variable shape exp)))

(define (alpha-lambda vars body id syntax shape)
  (ignore id)
  (let ((node (create-lambda-node vars)))
    (set (node-shape node) shape)
    (relate lambda-body node (alpha-block body syntax node))
    node))

(define (alpha-locale name body syntax shape)
  (let ((new-syntax (make-syntax-table syntax))
        (node (create-locale-node name)))
    (set (node-shape node) shape)
    (relate locale-body node (alpha-block body new-syntax node))
    node))

(define (alpha-assignment loc body shape kind)
  (let ((node (create-assignment-node (obtain-variable shape loc) kind)))
    (set (node-shape node) shape)
    (relate assignment-body node body)
    node))

(define (alpha-set loc val syntax shape)
  (cond ((pair? loc)
         (alpha `((setter ,(car loc)) ,@(cdr loc) ,val) syntax shape))
        (else
         (alpha-assignment loc (alpha val syntax shape) shape 'set))))

(define (alpha-lset loc val syntax shape)
  (alpha-assignment loc (alpha val syntax shape) shape 'lset))

(define (alpha-define pat body syntax shape)
  (cond ((pair? pat)
         (alpha-assignment (car pat)
                           (alpha-lambda (cdr pat) body (car pat) syntax shape)
                           shape
                           'define))
        (else
         (alpha-assignment pat
                           (alpha (car body) syntax shape)
                           shape
                           'define))))

(define (alpha-if test con maybe-alt syntax shape)
  (let ((node (create-call-node 4)))
    (set (node-shape node) shape)
    (relate call-proc node (create-primop-node if-true-primop))
    (relate-call-args node
                      (list (alpha test syntax shape)
                            (thunkify (alpha con syntax shape))
                            (thunkify (cond ((null? maybe-alt)
                                             (alpha-undefined 'if))
                                            (else
                                             (alpha (car maybe-alt)
                                                    syntax
                                                    shape))))))
    node))

(define (thunkify node)
  (let ((lnode (create-lambda-node '())))
    (relate lambda-body lnode node)
    lnode))

(define (alpha-block forms syntax shape)
  (cond ((null? (cdr forms))
         (alpha (car forms) syntax shape))
        (else
         ;; Assume left-to-right argument evaluation.
         (let* ((len (length forms))
                (but-last (sublist forms 0 (fx- len 1)))
                (node (create-call-node len))
                (lnode (create-lambda-node (map (always 'ignore) but-last))))
           (set (node-shape node) shape)
           (relate lambda-body lnode (alpha (last forms) syntax shape))
           (relate-call-proc+args node
                             (cons lnode
                                   (map (lambda (exp) (alpha exp syntax shape))
                                        but-last)))
           node))))

(define (alpha-call exp syntax shape)
  (let ((node (create-call-node (length exp))))
    (set (node-shape node) shape)
    (relate-call-proc+args node (map (lambda (exp) (alpha exp syntax shape))
                                exp))
    node))

(define (alpha-define-macro pat body syntax shape)
  (let ((form `(lambda (exp)
                 (destructure ((,(cdr pat) exp))
                   . ,body))))
    (set (syntax-table-entry syntax)
         (eval form (env-for-macro-definition syntax))) ; ?!?!?
    (alpha-undefined 'define-macro)))

(define (make-syntax-table outer-table)
  ;; Fix later.
  outer-table)

(define (alpha-undefined stuff)         ; !?!?
  (ignore stuff)
  (create-literal-node *undefined*))

(define (undefined-value-node? node)
  (and (literal-node? node)
       (eq? (literal-value node) *undefined*)))

(define *undefined*
  (object nil ((identification self) '*undefined*)))

;;; ---------------------------------------------------------------------
;;; Tree-walk utilities

;;; E.g.
;;;    (define foo-dispatch (make-dispatcher 'foo-dispatch))
;;;    (define foo
;;;      (object (lambda (node arg) ((foo-dispatch node) node arg))
;;;              
;;;    (define-dispatch ((foo literal) node args) ...)

(define (make-dispatcher id)
  (let ((table (make-table id)))
    (object (lambda (node)
              (let* ((type (node-type node))
                     (probe (table-entry table type)))
                (cond ((not probe)
                       (error "no ~S specialist for node type ~S" id type))
                      (else probe))))
            ((get-dispatch-table self) table))))

(define (set-node-dispatch generalist type-name proc)
  (set (table-entry (get-dispatch-table (get-dispatcher generalist))
                    (table-entry *node-types* type-name))
       proc))

(define-operation (get-dispatcher generalist))

(define-operation (get-dispatch-table dispatcher))

(define (walk-children proc node . args)
  (do ((l (node-children node) (cdr l)))
      ((null? l) t)
    (apply proc (car l) args)))

;;; ---------------------------------------------------------------------
;;; Convert node to s-expression

;;; Think about making this actually be a true inverse.
;;; Have to deal with variable renaming, etc.

(define-generalist express dispatch
  (lambda (node)
    ((dispatch node) node)))

(define-specialist ((express literal) node)
  (let ((lit (literal-value node)))
    (cond ((and lit (self-evaluating? lit)) lit)
          (else `',lit))))

(define-specialist ((express reference) node)
  (express-variable (reference-variable node)))

(define (express-variable variable)
  (variable-name variable))

(define-specialist ((express assignment) node)
  `(,(assignment-kind node) ,(express-variable (assignment-variable node))
                            ,(express (assignment-body node))))

;;; Don't call hack-lambda-variables here - this routine is called by COPY.

(define (express-lambda-variables node)
  (let ((vars (lambda-variables node)))
    (cond ((lambda-lexpr? node)
           (append! (map variable-name (required-vars vars))
                    (variable-name (rest-var vars))))
          (else (map variable-name (required-vars vars))))))

(define-specialist ((express lambda) node)
  (walk variable-name (cdr (lambda-variables node)))       ; !!
  `(lambda ,(express-lambda-variables node)
     ,@(express-block (lambda-body node))))

(define-specialist ((express locale) node)
  `(locale ,(cond ((null? (locale-variable node)) nil)
                  (else (variable-name (locale-variable node))))
     ,@(express-block (locale-body node))))

(define (express-block node)            ; utility
  (list (express node)))

(define-specialist ((express call) node)
  (let ((proc (call-proc node)))
    (cond ((and (lambda-node? proc)
                (not (lambda-lexpr? proc))
                (= (length (call-args node))
                   (length (required-vars (lambda-variables proc)))))
           `(let ,(map (lambda (b n) `(,(variable-name b) ,(express n)))
                       (required-vars (lambda-variables proc))
                       (call-args node))
              ,@(express-block (lambda-body proc))))
          ((maybe-primop node)
           => (lambda (primop) (express-call-primop primop node)))
          (else
           (map express (call-proc+args node))))))

;;; ---------------------------------------------------------------------
