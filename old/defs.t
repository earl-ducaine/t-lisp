(herald defs
        (syntax-table *orbit-syntax-table*))

;;;  Structure definitions
;;;  Node creation; interconnect manipulation

(define *empty*
  (object nil ((print self stream) (writes stream "#{Empty}"))))

(define (empty? obj) (eq? obj *empty*))

(define (bug f . rest)
  (apply error (list f "~%  (compiler error)") rest))

(lset *noise-stream* nil)
(lset *noise-flag* t)
(lset *debug-flag* t)

(define (warning f . rest)
  (apply format *noise-stream* (list "~%;;; Warning: " f) rest))

(define (noise f . rest)
  (if *noise-flag*
      (apply format *noise-stream* (list "~&;;; " f) rest)))

(define (orbit-debug . args)
  (if *debug-flag*
      (apply format *noise-stream* args)))

;;; ---------------------------------------------------------------------
;;; Variables

;;; This is getting pretty hairy.  What's going on here?

(define-structure-type variable
  name          ; Source code name for variable
  id            ; Unique numeric identifier
  binder        ; LAMBDA node which binds this variable
  number        ; K: var = (nth (lambda-variables (variable-binder var)) K)
  refs          ; List of leaf nodes n for which (leaf-variable n) = var
  type          ; The type of the variable's value at point of binding
  rep           ; Representation for variable's value
  reg           ; Register or slot allocated for this variable (?)
  support-env   ; Support environment with support for this variable
  temp
  )

(define-methods handle-variable
  ((print self stream)
   (format stream "#{Variable~_~S~_~A}"
           (object-hash self)
           self))
  ((display self stream)        ; hack for ~A (?!)
   (format stream "~S_~S"
           (cond ((primop? (variable-name self))
                  (identification (variable-name self)))
                 (else
                  (variable-name self))) 
           (variable-id self))))

(let ((var (stype-master variable-stype)))
  (set (variable-refs var) '())
  (set (variable-rep var) 'rep/pointer)       
  (set (variable-reg var) nil)
  (set (variable-support-env var) nil)
  (set (variable-binder var) nil)
  (set (variable-temp var) nil))

(lset *variable-id* 0)

(define (create-variable name)
  (let ((var (make-variable)))
    (set (variable-name var) name)
    (set (variable-id var) *variable-id*)
    (set (variable-type var) 'top?)
    (increment *variable-id*)
    var))

(define used? true?)

(define (obtain-variable shape name) (shape name))

;;; Create a lexical contour, inferior to SUPER, in which the given
;;; variables are bound.

(define (bind-variables variables binder super)
  (cond ((null? variables) super)
        (else
         (lambda (name)
           (or (find (lambda (var)
                       (eq? (variable-name var) name))
                     variables)
               (obtain-variable super name))))))

;;; A "handle" is a repository for variables which are free in the outermost
;;; scope.  After alpha-conversion, an outer LAMBDA-node is created whose
;;; parameters are exactly these variables.

(define (make-handle)
  (let ((table (make-table 'handle))
        (l '()))
    (object (lambda (name)
              (cond ((table-entry table name)
                     => identity)
                    (else
                     (let ((var (create-variable name)))
                       (push l var)
                       (set (variable-refs var) '())
                       (set (variable-binder var) nil)
                       (set (table-entry table name) var)))))
            ((the-free-variables self) l)
            ((print self stream)
             (format stream "#{Handle~_~S}" (object-hash self))))))

(define-operation (the-free-variables handle))

;;; ---------------------------------------------------------------------
;;; Nodes

;;; There are three node types:
;;;  - Leaf nodes
;;;  - LAMBDA-expression
;;;  - Call

(define-structure-type node
  variant           ; Node type (e.g., the predicate LITERAL-NODE?)
  parent            ; Parent node
  role              ; node == ((node-role node) (node-parent node))
  simplified?       ; True if doesn't need to be simplified
  children          ; List of inferior nodes
  instructions
  stuff-1           ; Variant components
  stuff-2
  stuff-3
  stuff-4
  stuff-5)

(define-methods handle-node
  ((print node stream)
   (print-node (node-variant node) node stream))
  ((disclose node)
   (express node)))           ; For PP, and for CRAWL's P command.

(let ((m (stype-master node-stype)))
  (set (node-instructions m) '())
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

(define literal? fixnum?)

;;; ---------------------------------------------------------------------
;;; Specific node types

;;; Leaf nodes
;;;   value    - Either a primop, variable, or literal
;;;   via      - the closure a variable is referenced through
;;;   source   - where this node was copied from
;;;   variant  - literal, primop, reference

(define leaf-node? (create-node-variant 'leaf))

(define leaf-value
  (node-field leaf-node? node-stuff-1 'leaf-value))

(define leaf-variant
  (node-field leaf-node? node-stuff-2 'leaf-variant))

(define leaf-via
  (node-field leaf-node? node-stuff-3 'leaf-via))

(define leaf-source
  (node-field leaf-node? node-stuff-4 'leaf-source))

(define (create-leaf-node value variant)
  (let ((node (create-node leaf-node? 0)))
    (set (leaf-value node) value)
    (set (leaf-variant node) variant)
    (set (leaf-via node) nil)
    (set (leaf-source node) '())
    node))

(define (create-primop-node primop)
  (create-leaf-node primop 'primop))

(define (primop-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'primop)))

(define (primop-ref? node primop)
  (and (primop-node? node)
       (eq? (primop-value node) primop)))

(define primop-value leaf-value)

(define (create-literal-node value)
  (create-leaf-node value 'literal))

(define (literal-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'literal)))

(define literal-value leaf-value)
  
(define (create-reference-node variable)
  (let ((node (create-leaf-node variable 'reference)))
    (push (variable-refs variable) node)
    node))

(define (reference-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'reference)))

(define reference-variable leaf-value)
(define reference-copy-sources leaf-source)

;;; Lambda-expression
;;;   variables - list of variables which are bound by this lambda
;;;               (the first variable gets bound to the procedure itself)
;;;   body      - the node for the body (after CPS, always a call node)
;;;   env       - list of variables live on entry to this lambda
;;;   strategy  - label, stack, or heap (where are closures over this lambda?)


(define lambda-node? (create-node-variant 'lambda))

(define lambda-variables
  (node-field lambda-node? node-stuff-1 'lambda-variables))

(define lambda-env
  (node-field lambda-node? node-stuff-2 'lambda-env))

(define lambda-strategy
  (node-field lambda-node? node-stuff-3 'lambda-strategy))

(define lambda-live
  (node-field lambda-node? node-stuff-4 'lambda-live))

(define lambda-temps
  (node-field lambda-node? node-stuff-5 'lambda-temps))

(define lambda-body
  (make-relation 'lambda-body lambda-node? 0 nil))

(define (create-lambda-node vars)
  (let ((node (create-node lambda-node? 1))
        (vars (cond ((null? (car vars))
                     (cons (create-variable 'p) (cdr vars)))
                    (else vars))))
    (set (lambda-variables node) vars)
    (set (lambda-strategy node) nil) 
    (set (lambda-live node) nil)
    (set (lambda-env node) nil)
    (do ((vars vars (cdr vars))
         (n 0 (fx+ n 1)))
        ((null? vars))
      (let ((var (car vars)))
        (cond ((used? var)
               (set (variable-binder var) node)
               (set (variable-number var) n)))))
    node))

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

(define (call-exit-args node)
  (sublist (call-args node) 0 (call-exits node)))

(define (call-non-exit-args node)
  (nthcdr (call-args node) (call-exits node)))

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

(define (call-exit? node)
  (let ((role (node-role node)))
    (cond ((lambda-node? (call-proc (node-parent node)))
           (eq? role call-proc))
          (else
           (and (call-arg? role)
                (neq? role call-proc)
                (fx<= (call-arg-number role)
                      (call-exits (node-parent node))))))))

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
