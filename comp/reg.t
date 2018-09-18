(herald reg)
(define (rr)
  (set *lambda-queue* '()) 
  (set *locations* (make-table 'locations))
  (vector-fill *registers* nil))

(define-constant tag/pair 3)
(define-constant tag/extend 2)



(lset *assembly-comments?* nil)
(lset *lambda-queue* '())         ;; queue of lambda bodies to process
(lset *stack-pos* 0)              ;; distance of stack-pointer from "frame"
(lset *max-temp* 0)               ;; maximum number of temporaries used
(lset *lambda* nil)               ;; the procedure being compiled
           


;;; GENERATE-CODE Initialize lambda queue. Go.

(define (generate-code-top top)
  (set *lambda-queue* '())
  (generate-code top))

(define (generate-code node)
  (if *debug-flag* (format t "~%~d~%" (object-hash node)))
  (check-clear)
  (set *stack-pos* 0)
  (allocate-registers node)                                          
  (process-lambda-queue))

(define (lambda-queue node)
  (set *lambda-queue* (append *lambda-queue* (list node))))

(define (process-lambda-queue)
  (cond (*lambda-queue*
         (generate-code (pop *lambda-queue*)))))

(define (register? x)
  (and (fixnum? x) (fx>= x 0) (fx< x *real-registers*)))

;;; Registers and temps are represented in the same structure

(define reg-node
  (object (lambda (reg) 
            (vref *registers* reg))
          ((setter self) 
           (lambda (reg node)
             (vset *registers* reg node)))))
                         
(define temp-node reg-node)


(define (reg-type reg)
    (if (fx< reg *scratch-registers*) 'scratch 'pointer))

;;; ALLOCATE-REGISTERS Sets *lambda* to be the lambda-node representing the
;;; environment the node argument is compiled in.  Generate code for the body.

(define (allocate-registers node)
    (case (lambda-strategy node)
      ((strategy/stack strategy/heap)
       (set *lambda* node)
       (emit-template node))
      (else
       (cond ((join-point-contour (lambda-env node))
              => (lambda (contour)
                   (set *lambda* (variable-binder contour)))))
       (emit-tag node)))
    (initialize-registers node)
    (if (n-ary? node)
        (n-ary-setup node))
    (allocate-call (lambda-body node)))
    
;;; INITIALIZE-REGISTERS Here we mark the arguments of a closure as being in
;;; the argument registers.  For a heaped lambda there is also the environment
;;; in the P register.  For a join point the state is initialized.


(define (initialize-registers node)
  (xcase (lambda-strategy node)
    ((strategy/heap)
         (if (closure-env (environment-closure (lambda-env node)))
             (mark (lambda-self-var node) P))
         (mark-vars-in-regs (cdr (lambda-variables node))))
    ((strategy/stack)
     (mark-vars-in-regs (lambda-variables node)))
    ((strategy/label)
     (cond ((join-point-contour (lambda-env node))
            => (lambda (contour)
                 (mark contour P))))
     (walk mark
          (cdr (lambda-variables node))
          (join-point-arg-specs (lambda-env node)))
     (walk (lambda (pair)
             (mark (cdr pair) (car pair)))
           (join-point-global-registers (lambda-env node))))))


(define (mark-vars-in-regs vars)
  (do ((vars vars (cdr vars))
       (reg A1 (fx+ reg 1)))
      ((or (fx>= reg AN) (null? vars))
       (do ((vars vars (cdr vars))
            (reg (fx+ reg 1) (fx+ reg 1)))
           ((null? vars))
         (cond ((variable-refs (car vars))
                (mark-temp (car vars) reg)))))
    (cond ((variable-refs (car vars))
           (mark (car vars) reg)))))
     
;;; A closure is n-ary if it has a non null rest arg.

(define n-ary? lambda-rest-var)

(define (n-ary-setup node)
  (cond ((used? (lambda-rest-var node))
         (generate-nary-setup node
                              (if (eq? (lambda-strategy node) 'strategy/stack)
                                  (length (lambda-variables node))
                                  (length (cdr (lambda-variables node))))))))

;;; ALLOCATE-CALL The "top".  Dispatch on the type of call.

(define (allocate-call node)
  (if *assembly-comments?* (emit-comment "~A" (pp-cps node)))
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (allocate-primop-call node))
          ((lambda-node? proc)
           (generate-let&labels node (call-proc node)
                                (call-args node)))
          ((variable-known (leaf-value proc))
           => (lambda (proc)
                (xcond ((eq? (lambda-strategy proc) 'strategy/label)
                        (allocate-label-call node proc))
                       ((fx= (call-exits node) 0)
                        (allocate-return node))
                       ((fx= (call-exits node) 1)
                        (allocate-general-call node)))))
          ((fx= (call-exits node) 0)
           (allocate-return node))
          ((fx= (call-exits node) 1)
           (allocate-general-call node))
          (else
           (bug "too many exits - ~s" node)))))
  
;;; ALLOCATE-LABEL-CALL If this is the path that arrives first, go to
;;; determine where the free variables of the join are to be kept.
;;; Make the state of the registers the way the join point wants it.
;;; Parallel assign and jump to the label.

(define (allocate-label-call node proc)
  (let ((join (get-or-set-join-state node proc))
        (cont ((call-arg 1) node)))
    (cond ((lambda-node? cont)
           (make-stack-closure node cont)
           (parallel-assign node
                            (cdr (call-args node))
                            (join-point-arg-specs join)
                            nil
                            (join-point-global-registers join)))
          (else
           (parallel-assign node
                            (cdr (call-args node))
                            (join-point-arg-specs join)
                            nil
                            (join-point-global-registers join))
           (fetch-continuation node (leaf-value cont)))))
  (clear-slots)
  (generate-jump (label proc)))


  
(define (allocate-general-call node)
  (let ((cont ((call-arg 1) node)))
    (cond ((lambda-node? cont)     
           (make-stack-closure node cont)
           (parallel-assign-general node))
          (else                          
           (parallel-assign-general node)          
           (fetch-continuation node (leaf-value cont)))))
  (clear-slots)
  (generate-general-call (fx- (length (call-args node)) 1)))

                                   
(define (allocate-return node)
  (parallel-assign-return node)      
  (fetch-continuation node (leaf-value (call-proc node)))
  (clear-slots)
  (generate-return 1))
                         

(define (parallel-assign-general node)
  (parallel-assign node (cons (call-proc node) (cdr (call-args node)))
                        nil t '()))

(define (parallel-assign-return node)
  (parallel-assign node (call-args node) nil nil '()))


;;; PARALLEL-ASSIGN Cons a closure if necessary.  It is known that there
;;; will only be one that needs to be consed.

(define (parallel-assign node args p-list proc? solve-list)
  (let* ((pos-list (if p-list p-list (reg-positions (length args) proc?)))
         (closure (get-closure args)))
    (cond (closure
           (make-heap-closure node closure)
           (lock AN)
           (really-parallel-assign node args pos-list solve-list))
          (else
           (really-parallel-assign node args pos-list solve-list)))))

(define (get-closure args)
  (any (lambda (arg)
         (and (lambda-node? arg)
              (neq? (environment-closure (lambda-env arg)) *unit*)
              (environment-closure (lambda-env arg))))
       args))


;;; do-now - register or temp pairs (source . target)
;;; trivial - immediate or lambda
;;; do-later - environment
;;; See implementor for this stuff. Hairy!!

(define (really-parallel-assign node args pos-list solve-list)
  (receive (do-now trivial do-later) (sort-by-difficulty args pos-list)
    (receive (do-now do-later) (add-on-free-list do-now do-later solve-list)
      (cond ((null? do-later)
             (solve node do-now))
            (else
             (cond ((eq? (lambda-strategy *lambda*) 'strategy/stack)
                    (walk (lambda (pair)
                            (cond ((reg-node (cdr pair))
                                   => (lambda (value)
                                        (set (register-loc value) nil)
                                        (set (temp-loc value) nil)))))
                          do-now))
                   (else
                    (walk (lambda (pair)
                            (let ((value (reg-node (cdr pair))))
                              (and (variable? value)
                                   (fx= (variable-number value) 0)
                                   (free-register node (cdr pair)))))
                          do-now)))
             (solve node do-now)
             (iterate loop ((pairs do-later))
               (cond ((null? (cdr pairs)))
                     (else
                      (lock (cdar pairs))
                      (loop (cdr pairs)))))
             (walk (lambda (pair) (do-indirect node pair))
                   do-later)))
      (walk (lambda (pair)
              (do-trivials node (car pair) (cdr pair) AN))
            trivial))))


(define (add-on-free-list do-now do-later solve-list)
  (iterate loop ((pairs solve-list) (do-now do-now) (do-later do-later))
    (cond ((null? pairs)
           (values do-now do-later))
          ((or (register-loc (cdar pairs))
               (temp-loc (cdar pairs)))
           => (lambda (reg)
                (loop (cdr pairs)
                      (cons (cons reg (caar pairs)) do-now)
                      do-later)))
          (else
           (loop (cdr pairs)
                 do-now
                 (if (fx= (caar pairs) P)
                     (append! do-later (list (cons (cdar pairs) P)))
                     (cons (cons (cdar pairs) (caar pairs))
                           do-later)))))))


(define (sort-by-difficulty args pos-list)
  (iterate loop ((args args) (do-now '()) (trivial '()) (do-later '())
                 (pos-list pos-list))
    (cond ((null? args)
           (values do-now trivial do-later))
          ((lambda-node? (car args))
           (cond ((eq? (environment-closure (lambda-env (car args))) *unit*)
                  (loop (cdr args)
                        do-now
                        trivial
                        (cons (cons (car args) (car pos-list)) do-later)
                        (cdr pos-list)))
                 (else
                  (loop (cdr args)
                        do-now
                        (cons (cons (car args) (car pos-list)) trivial)
                        do-later
                        (cdr pos-list)))))
          ((immediate? (leaf-value (car args)))
           (loop (cdr args)
                 do-now
                 (cons (cons (car args) (car pos-list)) trivial)
                 do-later
                 (cdr pos-list)))
          ((or (register-loc (leaf-value (car args)))
               (temp-loc (leaf-value (car args))))
           => (lambda (reg)
                (loop (cdr args)
                      (cons (cons reg (car pos-list)) do-now)
                      trivial
                      do-later
                      (cdr pos-list))))
          (else
           (loop (cdr args)
                 do-now
                 trivial
                 (if (fx= (car pos-list) P)
                     (append! do-later (list (cons (leaf-value (car args)) P)))
                     (cons (cons (leaf-value (car args)) (car pos-list))
                           do-later))
                 (cdr pos-list))))))


(define (do-trivials call-node node reg closure-reg)
  (cond ((lambda-node? node)
         (let ((offset (environment-cic-offset (lambda-env node))))
           (cond ((fx= offset 0)
                  (generate-move closure-reg reg))
                 (else
                  (generate-move-address
                           (reg-offset closure-reg offset)
                           reg)))))
        (else
         (generate-move (access-value call-node (leaf-value node))
                                  reg))))

(define (do-indirect node pair)
  (cond ((lambda-node? (car pair))
         (lambda-queue (car pair))
         (generate-move-address
                               (lookup node (car pair) nil)
                               (cdr pair)))
        (else
         (generate-move (access-value node (car pair))
                             (cdr pair))
         (mark (car pair) (cdr pair))))
  (lock (cdr pair)))

(define (solve node pairs)
  (let ((ppairs (filter (lambda (pair) (fxn= (car pair) (cdr pair))) pairs))
        (vals (map (lambda (pair)
                       (reg-node (car pair)))
                     pairs)))
    (iterate loop1 ((pairs ppairs) (targets (map cdr ppairs)) (temp nil))
      (cond ((null? pairs))
            (else
             (iterate loop2 ((candidates targets))
               (cond ((null? candidates)
                      (let ((pair (car pairs)))
                        (generate-move (cdr pair) TN)
                        (generate-move (car pair) (cdr pair))
                        (loop1 (cdr pairs)
                               (delq (cdr pair) targets)
                               (cdr pair))))
                     ((not (assq (car candidates) pairs))
                      (let ((pair (cdrassq (car candidates) pairs)))
                        (generate-move (if (eq? (car pair) temp)
                                                TN
                                                (car pair))
                                            (cdr pair))
                        (loop1 (delq pair pairs)
                               (delq (cdr pair) targets)
                               temp)))
                     (else
                      (loop2 (cdr candidates))))))))
    (walk kill vals)
    (walk (lambda (pair value)
            (mark value (cdr pair))
            (lock (cdr pair)))
          pairs
          vals)))



(define (cdrassq x l)
  (do ((l l (cdr l)))
      ((eq? (cdar l) x)
       (car l))))


;;; MAKE-STACK-CLOSURE Push a continuation on the stack.  For now there are no
;;; scratch values.  When there are we will need to push zeroes for all the
;;; scratch slots and fill them in after pushing the template.  This is because
;;; the GC assumes that anything on top of the stack until the first template
;;; is a valid pointer.


(define (make-stack-closure node cont)
  (lambda-queue cont)
  (if *assembly-comments?* (emit-comment "consing stack closure"))
  (walk (lambda (pair)
          (generate-push (access-value node (car pair))))
        (reverse (cdr (closure-env (environment-closure (lambda-env cont))))))
  (generate-push-address (template cont)))

                                                                            
;;; MAKE-HEAP-CLOSURE The first member of the closure corresponds to the
;;; template so we call %make-extend with this template and the size of the
;;; closure to be created.  Then we fill in the slots with the need variables
;;; and the addresses of templates for any closure-internal-closures.

(define (make-heap-closure node closure)
  (if *assembly-comments?* (emit-comment "consing heap closure"))
  (let* ((members (closure-members closure))
         (first (car members)))
    (walk (lambda (var)
            (lambda-queue (variable-binder var)))
          members)
    (free-register node AN)
    (generate-move-address (template (variable-binder first))
                                AN)
    (generate-extend node (closure-size closure))
    (walk (lambda (pair)
      (let ((var (car pair))
            (offset (cdr pair)))
        (cond ((memq? var members)
               (generate-move-address (template (variable-binder var))
                                      (reg-offset AN
                                                  (fx- offset tag/extend))))
              (else
               (generate-move (access-value node var)
                                   (reg-offset AN
                                               (fx- offset tag/extend)))))))
      (cdr (closure-env closure)))))


(define (generate-extend node n)
  (free-register node S1)
  (free-register node S2)
  (free-register node AN-1)
  (generate-move (lit n) S1)
  (lock AN)
  (generate-move (access-value node primop/%make-extend) AN-1)
  (unlock AN)
  (generate-jump-to-subroutine))


(define (allocate-primop-call node)
  (let* ((primop (primop-value (call-proc node))))
    (cond ((primop.conditional? primop)
           (allocate-conditional-primop node primop))
          ((primop.special? primop)
           (primop.generate primop node))
          (else           
           (really-allocate-primop-call node primop)))))
                                       

;;; ALLOCATE-CONDITIONAL-PRIMOP When we come to a split we save the state of
;;; the world and traverse one arm, then restore the state and traverse the
;;; other.

(define (allocate-conditional-primop node primop)
  (primop.generate primop node)
  (bind ((*registers* (copy-registers))
         (*stack-pos* *stack-pos*)
         (*lambda* *lambda*)) 
    (walk (lambda (n)
            (kill-if-dead n (then-cont node)))
          (cdr (call-args node)))
    (emit-tag (then-cont node))
    (allocate-call (lambda-body (then-cont node)))
    (check-clear))
  (restore-slots)
  (walk (lambda (n)
          (kill-if-dead n (else-cont node)))
        (cdr (call-args node)))
  (emit-tag (else-cont node))
  (allocate-call (lambda-body (else-cont node))))
    
(define (really-allocate-primop-call node primop)
  (let ((c (cont node)))
    (primop.generate primop node)
    (cond ((lambda-node? c)
           (walk (lambda (node)
                   (kill-if-dead node c))
                 (cdr (call-args node)))
           (allocate-call (lambda-body c)))
          (else                            
           (walk (lambda (node)
                   (if (leaf-node? node) (kill (leaf-value node))))
                 (cdr (call-args node)))
           (fetch-continuation node (leaf-value c))
           (clear-slots)
           (generate-return 1)))))
           
;;; ACCESS-VALUE This is the primary routine to get addressability to values.
;;; Just a giant case statement.

(define (access-value node value)
  (cond ((register-loc value)
         => identity)
        ((temp-loc value)
         => identity)
        (else
         (cond ((variable? value)
                (lookup node value (variable-binder value)))
               ((primop? value)
                (lookup node value nil))
               (else
                (cond ((eq? value '#T)
                       (machine-num 2))
                      ((or (eq? value '#F) (eq? value '()))
                       nil-reg)
                      ((immediate? value)
                       (lit value))
                      (else
                       (lookup node value nil))))))))

;;; LOOKUP If the value is a known procedure, if it is in the unit we get it
;;; from there, otherwise we get the variable which the known procedure is
;;; bound to.

(define (lookup node value lambda-bound?)
  (cond ((and lambda-bound? (variable-known value))
         => (lambda (label)
              (cond ((eq? (environment-closure (lambda-env label)) *unit*)
                     (lookup node label nil))
                    (else
                     (access-value node (lambda-self-var label))))))
        (else
         (xcase (lambda-strategy *lambda*)
           ((strategy/stack)
            (fetch-from-stack node value lambda-bound?))
           ((strategy/heap)
            (let ((contour (lambda-self-var *lambda*)))
              (->register 'pointer node contour '*)
              (fetch-from-heap node contour value lambda-bound?)))))))




;;; This isn't called from this file or vaxprimops
(define (make-locative-accessable node ref)
  (let ((var (leaf-value ref)))
    (cond ((variable-binder var)
           (bug "set on bound variables"))
          (else
           (access-value node var)))))

                                
;;; ACCESS-FROM-UNIT Get from unit when there is a closure-internal-template.
;;; If we have one, just offset from template-pointer. If we are internal to
;;; a closure which has one, get it first and then offset into unit.

(define (access-from-unit node contour var)
  (let ((closure (environment-closure (lambda-env (variable-binder contour)))))
    (cond ((closure-cit-offset closure)
           => (lambda (current-offset)
                (cond ((eq? contour (car (closure-members closure)))
                       (reg-offset  TP (fx- (cdr (assq var (closure-env *unit*)))
                                            (fx+ current-offset 6))))
                      ((register-loc (car (closure-members closure)))
                       => (lambda (reg)
                            (reg-offset reg (fx- (cdr (assq var (closure-env *unit*)))
                                                 (fx+ current-offset 6)))))
                      (else
                       (lock (register-loc contour))
                       (let ((reg (get-register 'pointer node '*)))
                         (unlock (register-loc contour))
                         (mark (car (closure-members closure)) reg)
                         (generate-move-address
                            (reg-offset (register-loc contour)
                                        (fx- (fx- 0 tag/extend)
                                             (cdr (assq contour
                                                  (closure-env closure)))))
                            reg)
                         (reg-offset reg (fx- (cdr (assq var (closure-env *unit*)))
                                              (fx+ current-offset 6))))))))
          (else nil))))


(define (get-env var)
  (lambda-env (variable-binder var)))
                                      

;;; Yukk.  Here we get a variable from a stack frame.  If it is in the frame
;;; we are OK.  Otherwise we chain down stack frames as long as they are there.
;;; These frames are all simple offsets from SP.  When we arrive at a pointer
;;; into the heap, we load that pointer into a register and go to the heap
;;; code to do the rest.


(define (fetch-from-stack node value lambda-bound?)
  (iterate loop ((offset 0) (l *lambda*))
    (case (lambda-strategy l)
      ((strategy/label strategy/open)
       (loop offset (node-parent (node-parent l))))
      (else
       (let ((env (closure-env (environment-closure (lambda-env l)))))
         (cond ((and lambda-bound? (assq value env))
                => (lambda (env-pair)
                     (reg-offset SP (fx+ *stack-pos*
                                         (fx+ offset (cdr env-pair))))))
               ((and (cdr env)
                     (eq? (variable-number (caadr env)) 0))
                (let ((accessor (reg-offset SP (fx+ *stack-pos*
                                                    (fx+ offset CELL))))
                      (contour (caadr env)))
                  (into-register 'pointer node contour accessor '*)
                  (fetch-from-heap node contour value lambda-bound?)))
               (else
                (loop (fx+ (closure-size (environment-closure (lambda-env l)))
                           offset)
                      (node-parent (node-parent l))))))))))
                          



(define (closure-internal-closure? value closure)
  (and (variable? value)
       (memq? value (closure-members closure))))

(define (fetch-from-heap node contour value lambda-bound?)
  (iterate loop ((env (get-env contour)) (contour contour)) 
    (let ((a-list (closure-env (environment-closure env)))
          (current-offset (environment-cic-offset env)))
      (cond ((assq value a-list)
             => (lambda (pair)
                  (if (closure-internal-closure? value
                                                 (environment-closure env))
                      (list (reg-offset (register-loc contour)  ; *** hack
                                        (fx- (cdr pair) current-offset)))
                      (reg-offset (register-loc contour)
                                  (fx- (cdr pair)
                                       (fx+ current-offset tag/extend))))))
            ((and (not lambda-bound?) (access-from-unit node contour value)))
            (else
             (into-register 'pointer node (caadr a-list)
                (reg-offset  (register-loc contour)
                             (fx+ current-offset tag/extend))
                '*)
             (loop (get-env (caadr a-list)) (caadr a-list)))))))
                 
(define (fetch-continuation node var)
  (let ((access (fetch-continuation-from-stack node var)))
    (if (fxn= (cdr access) 0)  ; detect MOVE 0(SP),SP and don't generate it
        (generate-move-address access SP))))
                                          
;;; Code to get a continuation off the stack.
;;; Search up the tree until we find it.



(define (fetch-continuation-from-stack node var)
  (iterate loop ((offset 0) (l (node-parent node)))
    (cond ((eq? var (car (lambda-variables l)))
           (reg-offset SP offset))
          (else
           (case (lambda-strategy l)
             ((strategy/stack)
              (loop (fx+ (closure-size (environment-closure (lambda-env l)))
                         offset)
                   (node-parent (node-parent l))))
             (else
              (loop offset
                    (node-parent (node-parent l)))))))))

;;; ->REGISTER Move the value of leaf-node REF into a register of type TYPE
;;; which can be either '* or a specific register. Force an existing value out
;;; if necessary,

(define (->register type node var where)
  (let ((accessor (access-value node var)))
    (cond ((and (register? accessor)
                (or (and (eq? (reg-type accessor) type)
                         (eq? where '*))
                    (eq? accessor where)))
           accessor)
          (else  
           (into-register type node var accessor where)))))
          
(define (in-register? type value where)
  (let ((reg (register-loc value)))
    (and reg
         (eq? (reg-type reg) type)
         (or (eq? where '*)
             (eq? where reg)))))

(define (get-register type node where)
  (cond ((neq? where '*)
         (free-register node where)
         where)
        ((neq? type 'scratch)
         (really-get-register 'pointer *scratch-registers* *real-registers* t))
        (else
         (really-get-register 'scratch
                              0
                              *scratch-registers* t))))

(define (get-reg-if-free spec)
  (xcond ((register? spec)
          (if (reg-node spec) nil spec))
         ((eq? spec 'pointer)
          (really-get-register spec *scratch-registers* *real-registers* nil))
         ((eq? spec 'scratch)
          (really-get-register spec 0 *scratch-registers* nil))
         ((eq? spec '*)
          (really-get-register spec 0 *real-registers* nil))))

(define (really-get-register type start stop kick?)
  (iterate loop ((i start))
    (cond ((fx>= i stop)
           (if kick? (select-and-kick-register node type) nil))
          ((not (reg-node i))
           i)
          (else
           (loop (fx+ i 1))))))

(define (into-register type node value access where)
  (cond ((in-register? type value where)
         (register-loc value))
        (else         
         (let ((reg (get-register type node where)))
           (generate-move access reg)
           (cond ((register-loc value)
                  => (lambda (reg)
                       (set (reg-node reg) nil))))
           (mark value reg)
           reg))))


;;; SELECT-AND-KICK-REGISTER The first register which is not locked or used soo
;;; is selected.  If none satisfy then the first register  is selected.
                                          
(define (select-and-kick-register node type)
  (cond ((eq? type 'pointer) 
         (iterate loop ((i (fx+ *scratch-registers* 1)) (default nil)) ;kick P?
           (cond ((fx>= i *real-registers*)
                  (kick-register node default)
                  default)
                 ((locked? i) 
                  (loop (fx+ i 1) default))
                 ((not (used-soon? node (reg-node i)))
                  (kick-register node i) 
                  i)
                 (else (loop (fx+ i 1) i)))))
        (else
         (iterate loop ((i 0) (default nil))
           (cond ((fx>= i *scratch-registers*)
                  (free-register node default)
                  default)
                 ((locked? i) 
                  (loop (fx+ i 1) default))
                 ((not (used-soon? node (reg-node i))) 
                  (free-register node i)
                  i)
                 (else (loop (fx+ i 1) i)))))))
                                         

;;; USED-SOON? Is this variable used at this node or at one of its
;;; continuations?

(define (used-soon? node value)
  (let ((var-used? (lambda (arg)
                     (and (leaf-node? arg)
                          (eq? (leaf-value arg) value)))))
     (or (any? var-used? (call-args node))
         (any? (lambda (cont)
                 (any? var-used? (call-args cont)))
               (continuations node)))))

(define (free-register node reg)
  (cond ((reg-node reg)
         => (lambda (value)
              (let ((new (where-used value)))
                (cond ((and (register? new) (get-reg-if-free new))
                       => (lambda (new)
                            (mark value new)
                            (set (reg-node reg) nil)
                            (generate-move reg new)))
                      (else
                       (kick-register node reg))))))))

(define (kick-register node reg) 
  (let ((value (reg-node reg)))
    (cond ((locked? reg)
           (error "attempt to kick out of locked register"))
          ((or (temp-loc value)
               (not (variable? value))
               (and (neq? value (lambda-self-var *lambda*))
                    (fx= (variable-number value) 0)))
           (set (register-loc value) nil)
           (set (reg-node reg) nil))
          (else
           (let ((temp (get-temp value (reg-type reg))))
             (set (register-loc value) nil)
             (set (temp-loc value) temp)
             (set (reg-node reg) nil)
             (generate-move reg temp))))))


(define (really-get-temp value type)
  (cond ((eq? type 'scratch)
         (really-get-register 'scratch
                              (fx+ *real-registers* *pointer-temps*)
                              *no-of-registers*
                              nil))
        (else
         (really-get-register 'pointer
                              *real-registers*
                              (fx+ *real-registers* *pointer-temps*)
                              nil))))

(define (get-temp value type)
  (cond ((really-get-temp value type)
         => (lambda (temp)
              (if (fx> temp *max-temp*)
                  (set *max-temp* temp))
              (set (temp-node temp) value)
              temp))
        (else
         (bug "all temps used"))))

(define (cont node)
  (car (call-args node)))
             
(define (continuations node)
  (sublist (call-args node) 0 (call-exits node)))

(define (then-cont node)
  (car (call-args node)))

(define (else-cont node)
  (cadr (call-args node)))


(define (kill-if-dead node where)
  (if (and (leaf-node? node)
           (or (not (variable? (leaf-value node)))
               (not (memq? (leaf-value node) (lambda-live where)))))
      (kill (leaf-value node))))

(define (kill value)
    (cond ((register-loc value)
           => (lambda (reg)
                (if (and (reg-node reg)
                         (neq? (reg-node reg) value))
                    (bug "horrible inconsistancy reg ~S value ~S"
                         reg
                         value))
                (set (reg-node reg) nil)
                (set (register-loc value) nil))))
    (cond ((temp-loc value)
           => (lambda (temp)
                (set (temp-node temp) nil)
                (set (temp-loc value) nil)))))

(define (live? value node)
  (cond ((or (not (variable? value))
             (leaf-node? ((call-arg 1) node)))
         nil)
        (else 
         (any? (lambda (cont)
                 (memq? value (lambda-live cont)))
               (continuations node)))))

(define (dying? value node)
  (not (live? value node)))

(define (dead? value node)
  (let ((parent (node-parent node)))
    (not (and (variable? value)
              (or (memq? value (lambda-variables parent))
                  (memq? value (lambda-live parent)))))))

(define (copy-registers)
  (copy-vector *registers*))


(define (restore-slots)
    (restore-registers)
    (restore-temps))

(define (restore-registers)
  (do ((i 0 (fx+ i 1)))
      ((fx>= i *real-registers* ))
    (cond ((reg-node i)
           (set (register-loc (reg-node i)) i)))))

(define (restore-temps)
  (do ((i *real-registers* (fx+ i 1)))
      ((fx>= i *no-of-registers* ))
    (cond ((temp-node i)
           (set (temp-loc (temp-node i)) i)))))



(define (clear-slots)
  (vector-fill *registers* nil)
  (set *locations* (make-table 'locations)))

(define (lock reg)
  (if (fx< reg *no-of-registers*)
      (set (reg-node reg)
           (cons 'locked (reg-node reg)))))

(define (unlock reg)
  (if (fx< reg *no-of-registers*)
      (set (reg-node reg)
           (cdr (reg-node reg)))))

(define (locked? reg)
  (pair? (reg-node reg)))

(define (protect-access access)
  (cond ((fixnum? access)
         (cond ((fx>= access 0)
                (lock access))))
        ((fg? access))
        ((register? (car access)) 
         (if (fxn= (car access) SP)
             (lock (car access))))
        ((pair? (car access))
         (lock (caar access))
         (lock (cdar access)))))
         
(define (release-access access)
  (cond ((fixnum? access)
         (cond ((fx>= access 0)
                (unlock access))))
        ((fg? access))
        ((register? (car access)) 
         (if (fxn= (car access) SP)
             (unlock (car access))))
        ((pair? (car access))
         (unlock (caar access))
         (unlock (cdar access)))))
              
(define (mark value reg)
  (set (reg-node reg) value)
  (if (register? reg)
      (set (register-loc value) reg)
      (set (temp-loc value) reg)))


(define (mark-temp value reg)
  (set (temp-node reg) value)
  (set (temp-loc value) reg))


(define (check-clear)
  (if (vector-pos neq? nil *registers*)
      (bug "registers not empty ~S" *registers*)))
                         

(define *pos-list1* '#(() (5) (5 6) (5 6 7) (5 6 7 8)))
(define *pos-list2* '#(() (4) (4 5) (4 5 6) (4 5 6 7) (4 5 6 7 8)))

(define (reg-positions i proc?)       
  (cond ((fx<= i (if proc? 5 4))
         (vref (if proc? *pos-list2* *pos-list1*) i))
        (else
         (append (if proc? '(4 5 6 7 8) '(5 6 7 8))
                 (make-num-list (fx- i (if proc? 5 4)))))))

(define (make-num-list amount)
  (let ((end (fx+ *real-registers* amount)))
    (do ((i *real-registers* (fx+ i 1))
         (l '() (cons i l)))
        ((fx>= i end) (reverse! l)))))


;;; Locations
;;;==========================================================================
;;;   Keeps track of where values are.
;;; A table of a-lists of form ((<type-of-location> . <index>)...) indexed by
;;; leaf values, i.e. variables, primops, or literals.

(lset *locations* (make-table 'locations))

(define (leaf-locations value)
   (table-entry *locations* value))

(define register-loc
  (object (lambda (value)
            (get-location value 'reg))
    ((identification self) 'register-loc)
    ((setter self)
     (lambda (value reg)
       (if (null? reg)
           (clear-location value 'reg)
           (set-location value 'reg reg))))))

(define temp-loc
  (object (lambda (value)
            (get-location value 'temp))
    ((identification self) 'temp-loc)
    ((setter self)
     (lambda (value temp)
       (if (null? temp)
           (clear-location value 'temp)
           (set-location value 'temp temp))))))

(define (get-location value type)
  (cdr (assq type (leaf-locations value))))

(define (set-location value type number)
  (let ((locs (leaf-locations value)))
    (cond ((assq type locs)
           => (lambda (pair)
                (set (cdr pair) number)))
          (else
           (set (table-entry *locations* value)
                `((,type . ,number) . ,locs))
           ))))

(define (clear-location value type)
  (let ((locs (leaf-locations value)))
    (set (table-entry *locations* value)
         (del! (lambda (x y)
                 (eq? (car x)
                      (car y)))
               `(,type)
               locs))
    nil))
