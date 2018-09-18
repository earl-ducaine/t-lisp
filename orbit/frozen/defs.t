(herald defs
        (syntax-table *orbit-syntax-table*))

;;;  Structure definitions
;;;  Node creation; interconnect manipulation

;;; VARIABLES
;;;===========================================================================
;;; Structures to represent variables.

(define-structure-type variable
  name          ; Source code name for variable (temporary, for debugging only)
  id            ; Unique numeric identifier
  binder        ; LAMBDA node which binds this variable
  support-env   ; Support environment that contains support for this variable
  number        ; K: var = (NTH (LAMBDA-ALL-VARIABLES (VARIABLE-BINDER var)) K)
  refs          ; List of leaf nodes n for which (REFERENCE-VARIABLE n) = var.
  type          ; The type of the variable's value at point of binding
  rep           ; Representation for variable's value
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
  (set (variable-support-env var) nil)
  (set (variable-binder var) nil))

(lset *variable-id* 0)

(define (create-variable name)
  (let ((var (make-variable)))
    (set (variable-name var) name)
    (set (variable-id var) *variable-id*)
    (set (variable-type var) 'top?)
    (increment *variable-id*)
    var))

(define (used? var)
  (and var
       (variable-refs var)))

;;; HANDLES
;;;============================================================================
;;;    A "handle" is a repository for variables which are free in the outermost
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

;;; SHAPES
;;;============================================================================
;;;   A shape contains all the variables lexically bound at a particular
;;; moment.  The top-level shape is a handle.  Variables are added to a shape
;;; using the procedure BIND-VARIABLES.

;;; Create a lexical contour, inferior to SUPER, in which the given
;;; variables are bound.

(define (bind-variables variables binder super)
  (cond ((null? variables) super)
        (else
         (lambda (name)
           (or (find (lambda (var)
                       (and var
                            (eq? (variable-name var) name)))
                     variables)
               (obtain-variable super name))))))

(define (obtain-variable shape name) (shape name))

;;; NODES
;;;============================================================================
;;; There are three node types:
;;;  - LAMBDA
;;;  - CALL
;;;  - LEAF
;;; Calls have a nonzero number of children, lambda nodes have a single child,
;;; leaf node have none.

(define-structure-type node
  variant           ; Node type, a predicate (e.g. LAMBDA-NODE?)
  parent            ; Parent node
  role              ; node == ((NODE-ROLE node) (NODE-PARENT node))
  simplified?       ; True if it has already been simplified.
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
  (set (node-simplified? m) nil))

;;; *EMPTY*
;;;==========================================================================
;;; *EMPTY* is used to mark empty parent and child slots in nodes.

(define *empty*
  (object nil ((print self stream) (writes stream "#{Empty}"))))

(define (empty? obj) (eq? obj *empty*))

(define (proclaim-empty probe)
  (cond ((not (empty? probe))
         (bug "not empty - ~S" probe))))

(set (node-parent (stype-master node-stype)) *empty*)

;;; (CREATE-NODE-VARIANT id)
;;;==========================================================================
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
    self))

(lset *loose* 0)    ; Count of unconnected nodes, set but never looked at.

(define-operation (print-node variant node stream))
(define-operation (create-node variant q))

;;; NODE FIELDS
;;;===========================================================================
;;;  These are used to rename the NODE-STUFF fields of particular node
;;; variants.  Could be defined integrably, this definition is for debugging
;;;  purposes only.

(define (node-field variant field id)
  (object (lambda (node)
            (field (check-arg variant node id)))
          ((setter self)
           (lambda (node val)
             (set (field (check-arg variant node id)) val)))
          ((identification self) id)))

;;; RELATIONS
;;;=========================================================================
;;; A "relation" is a selector procedure - something appropriate to put in
;;; the ROLE slot of a node.   Most of what they do is argument checking.

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

;;; (DETACH node)
;;;===========================================================================
;;; Disconnect node from its parent.

(define (detach node)
  (set (nth (node-children (node-parent node))
            (relation-index (node-role node)))
       *empty*)
  (set *loose* (fx+ *loose* 1))
  (set (node-role node) nil)
  (set (node-parent node) *empty*)
  node)

;;; (MOVE node proc)
;;;===========================================================================
;;; Replace node in tree with value of applying proc to node.
;;; Note the fact that a change has been made, for the simplifier.

(define (move node proc)
  (let ((role (node-role node))
        (parent (node-parent node)))
    (mark-changed node)
    (detach node)
    (relate role parent (proc node))))

;;; (REPLACE old-node new-node)
;;;===========================================================================
;;; Replace old-node with new-node.
;;; Note the fact that a change has been made, for the simplifier.

(define (replace old-node new-node)
  (let ((role (node-role old-node))
        (parent (node-parent old-node)))
    (mark-changed old-node)
    (detach old-node)
    (erase-all old-node)
    (relate role parent new-node)))

(define (mark-changed node)
  (do ((p (node-parent node) (node-parent p)))
      ((or (null? p)
           (not (node-simplified? p))))
    (set (node-simplified? p) nil)))


;;; LEAF NODES
;;;=========================================================================
;;;   There are three kinds of leaf nodes - PRIMOP, LITERAL, REFERENCE
;;;
;;; Fields:
;;;   variant  - 'literal, 'primop, or 'reference
;;;   value    - Either a primop, a variable, or a literal value
;;;   via      - the closure a variable is referenced through
;;;   source   - where this node was copied from

(define leaf-node? (create-node-variant 'leaf))

(define leaf-value
  (node-field leaf-node? node-stuff-1 'leaf-value))

(define leaf-variant
  (node-field leaf-node? node-stuff-2 'leaf-variant))

(define leaf-via
  (node-field leaf-node? node-stuff-3 'leaf-via))

(define (create-leaf-node value variant)
  (let ((node (create-node leaf-node? 0)))
    (set (leaf-value node) value)
    (set (leaf-variant node) variant)
    (set (leaf-via node) nil)
    node))

;;; PRIMOP NODES
;;;=========================================================================

(define (create-primop-node primop)
  (create-leaf-node primop 'primop))

(define (primop-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'primop)))

;;; Checks to see if NODE is a reference to on of the primops in PRIMOPS.
(define (primop-ref? node . primops)
  (and (primop-node? node)
       (memq? (primop-value node) primops)))

(define primop-value leaf-value)

;;; LITERAL NODES
;;;=========================================================================

(define (create-literal-node value)
  (create-leaf-node value 'literal))

(define (literal-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'literal)))

(define literal-value leaf-value)
  
;;; REFERENCE NODES
;;;=========================================================================

(define (create-reference-node variable)
  (let ((node (create-leaf-node variable 'reference)))
    (push (variable-refs variable) node)
    node))

(define (reference-node? node)
  (and (leaf-node? node)
       (eq? (leaf-variant node) 'reference)))

(define (variable-ref? node . variables)
  (and (reference-node? node)
       (memq? (reference-variable node) variables)))

(define reference-variable leaf-value)

;;; LAMBDA NODES
;;;============================================================================
;;; Fields:
;;;   variables - list of variables which are bound by this lambda.  The first
;;;        variable gets bound to the procedure itself; the second is a 'rest'
;;;        variable, if it is non-null the lambda is n-ary.
;;;   body      - the node for the body (after CPS, always a call node)
;;;   env       - list of variables live on entry to this lambda
;;;   strategy  - label, stack, or heap (where are closures over this lambda?)
;;;   live      - the variables live in the body of the lambda.
;;;   temps     - ?


(define lambda-node? (create-node-variant 'lambda))

(define lambda-all-variables
  (node-field lambda-node? node-stuff-1 'lambda-all-variables))

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

;;; Selecting various subsets of a lambda's variables.

(define (lambda-variables node)
  (cddr (lambda-all-variables node)))

(define (lambda-rest+variables node)
  (cdr (lambda-all-variables node)))

(define (lambda-self-var node)
  (car (lambda-all-variables node)))

(define (lambda-rest-var node)
  (cadr (lambda-all-variables node)))

(define (lambda-cont-var node)
  (caddr (lambda-all-variables node)))

;;; (CREATE-LAMBDA-NODE name vars)
;;;=========================================================================
;;;    Creates a lambda node.  NAME is used as the name of the lambda node's
;;; self variable.   VARS is a list of variables.  The VARIABLE-BINDER and
;;; VARIABLE-NUMBER slots of the variables are set.

(define (create-lambda-node name vars)
  (let ((node (create-node lambda-node? 1))
        (vars (cons (create-variable name)
                    vars)))
    (set (lambda-all-variables node) vars)
    (set (lambda-strategy node) nil) 
    (set (lambda-live node) nil)
    (set (lambda-env node) nil)
    (do ((vars vars (cdr vars))
         (n 0 (fx+ n 1)))
        ((null? vars))
      (let ((var (car vars)))
        (cond (var
               (set (variable-binder var) node)
               (set (variable-number var) n)))))
    node))

;;; CALL NODES
;;;==========================================================================
;;; Fields:
;;;   exits      - the number of initial arguments that are continuations
;;;   complexity - an integer estimating evaluation complexity
;;;   proc+args  - list of child nodes

(define call-node? (create-node-variant 'call))

(define call-exits
  (node-field call-node? node-stuff-2 'call-exits))

(define call-complexity
  (node-field call-node? node-stuff-1 'call-complexity))

;;; Selecting various subsets of the children of a call node.

(define-integrable call-proc+args node-children)

(define-integrable (call-args node)
  (cdr (call-proc+args node)))

(define (call-exit-args node)
  (sublist (call-args node) 0 (call-exits node)))

(define (call-non-exit-args node)
  (nthcdr (call-args node) (call-exits node)))

;;; ARGUMENT RELATIONS
;;;========================================================================
;;; Argument relations are created on demand whenever a newly created
;;; call node is going to have more arguments than any previous call node
;;; has had.

;;; This is not done quite the right way.  Fix.  -Jonathon Rees

(define-predicate call-arg?)

(define (make-arg-relation i)
  (make-relation `(call-arg ,i) call-node? i call-arg?))

(define call-arg-number relation-index)

(define *call-arg-relations*
  (list (make-arg-relation 0)))

(define call-proc (car *call-arg-relations*))

(define (call-arg i)
  (nth *call-arg-relations* i))

;;; (CALL-EXIT? node)
;;;===========================================================================
;;;   T if NODE is an exit of a call node, NIL otherwise.

(define (call-exit? node)
  (let ((role (node-role node)))
    (cond ((lambda-node? (call-proc (node-parent node)))
           (eq? role call-proc))
          (else
           (and (call-arg? role)
                (neq? role call-proc)
                (fx<= (call-arg-number role)
                      (call-exits (node-parent node))))))))

;;; (RELATE-CALL-PROC+ARGS parent children)
;;;===========================================================================
;;; Make CHILDREN the procedure and arguments of call node PARENT.

(define (relate-call-proc+args parent children)
  (relate call-proc parent (car children))
  (relate-call-args parent (cdr children)))

;;; (RELATE-CALL-ARGS node args)
;;;===========================================================================
;;; Make ARGS the arguments of call node PARENT.

(define (relate-call-args node args)
  (walk (lambda (relation child)
          (relate relation node child))
        (cdr *call-arg-relations*)
        args))

;;; (REPLACE-CALL-ARGS node new-args)
;;;===========================================================================
;;; Replace the arguments of call node NODE with NEW-ARGS.

(define (replace-call-args node new-args)
  (walk detach (call-args node))
  (relate-shorter-call-args node new-args))

;;; (RELATE-SHORTER-CALL-ARGS node new-args)
;;;===========================================================================
;;; Replace the arguments of call node NODE with (possibly shorter) NEW-ARGS.

(define (relate-shorter-call-args node new-args)
  (modify (cdr (node-children node))
          (lambda (l)
            (let ((n (fx- (length l) (length new-args))))
              (set *loose* (fx- *loose* n))
              (nthcdr l n))))
  (relate-call-args node new-args))

;;; (CREATE-CALL-NODE n exits)
;;;===========================================================================
;;; Create a call node with N children and EXITS exits.  Add new argument
;;; relations if their aren't enough.

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
