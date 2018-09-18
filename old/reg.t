(herald reg)
(define (rr)
  (set *lambda-queue* '()) 
  (set *locations* (make-table 'locations))
  (vector-fill *registers* nil))

;;; Copyright (c) 1984 David Kranz

;;; MC68000 i.e. two-address machine with mem-mem move

;;; values:
;;;      reg  -- number, nil    
;;;      temp -- number, nil

;;; leafs
;;;      access -- register number or (register . offset) or 
;;;                 ((base . index) . offset)  or (lit .  x)
;;;      via -- locative 
                  
;;; GENERATE-CODE Establish initial state of registers and temps, emit code to
;;; set up temps on stack, clear state, and process next lambda.

(define (generate-code-top top)
  (set *lambda-queue* '())
  (generate-code top))

(define (generate-code node)
  (if *debug-flag* (format t "~%~d ~a~%" (object-hash node) (lambda-name node)))
  (check-clear)
  (set *stack-pos* 0)
  (allocate-registers node)                                          
  (process-lambda-queue))

(define (lambda-queue node)
  (set *lambda-queue* (append *lambda-queue* (list node))))

(define (process-lambda-queue)
  (cond (*lambda-queue*
         (generate-code (pop *lambda-queue*)))))

(lset *assembly-comments?* nil)
(lset *lambda-queue* '())
(lset *stack-pos* 0)
(lset *temp-length* 0)
(lset *lambda* nil)
           
(define (register? x)
  (and (fixnum? x) (fx>= x 0) (fx< x *real-registers*)))


(define reg-node
  (object (lambda (reg) 
            (vref *registers* reg))
          ((setter self) 
           (lambda (reg node)
             (vset *registers* reg node)))))
                         
(define temp-node reg-node)


(define (reg-type reg)
    (if (fx< reg *scratch-registers*) 'scratch 'pointer))

(define (allocate-registers node)
    (case (lambda-strategy node)
      ((strategy/stack strategy/heap)
       (set *lambda* node)
       (emit-template node))
      (else
       (set *lambda* (variable-binder (join-point-contour (lambda-env node))))
       (emit-tag node)))
    (initialize-registers node) 
    (if (eq? (variable-rep (car (lambda-variables node))) 'n-ary)
        (n-ary-setup node))
    (allocate-call (lambda-body node)))
    
;;; INITIALIZE-REGISTERS Here we mark the arguments of a lambda as being in
;;; the argument registers.  For a heaped lambda this is also the environment
;;; in the P register.                                                    


(define (initialize-registers node)
  (xcase (lambda-strategy node)
    ((strategy/heap)
         (if (closure-env (environment-closure (lambda-env node)))
             (mark (car (lambda-variables node)) P))
         (mark-vars-in-regs (cddr (lambda-variables node)))
         (cond (*assembly-comments?*
         (emit-comment "heaped closure (LAMBDA ~a ...)"
                       (map variable-unique-name (cddr (lambda-variables node))))
         (emit-comment  "environment = ~a"
                       (let ((closure (environment-closure (lambda-env node))))
                         (if (eq? closure *unit*)
                             'unit
                             (printable-closure-env closure)))))))
    ((strategy/stack)
     (mark-vars-in-regs (cdr (lambda-variables node)))
     (cond (*assembly-comments?*
     (emit-comment "stacked closure (LAMBDA ~a ...)"
                   (map variable-unique-name (cdr (lambda-variables node))))
     (emit-comment "environment = ~a"
           (printable-closure-env (environment-closure (lambda-env node)))))))
    ((strategy/label strategy/ezclose)
     (mark (join-point-contour (lambda-env node)) P)
     (walk mark
          (cddr (lambda-variables node))
          (join-point-arg-specs (lambda-env node)))
     (walk (lambda (pair)
             (mark (cdr pair) (car pair)))
           (join-point-global-registers (lambda-env node)))
     (cond (*assembly-comments?*
     (emit-comment "join-point (LAMBDA ~a ...)"
                   (map variable-unique-name (cddr (lambda-variables node))))
     (emit-comment "with args in ~a"
                   (map mn-assembly-syntax
                        (join-point-arg-specs (lambda-env node))))
     (emit-comment "global assignments = ~a"
                 (map (lambda (pair)
                        (list (mn-assembly-syntax (car pair))
                              (variable-unique-name (cdr pair))))
                      (join-point-global-registers (lambda-env node)))))))))


(define (mark-vars-in-regs vars)
  (do ((vars vars (cdr vars))
       (reg A1 (fx+ reg 1)))
      ((or (fx>= reg A5) (null? vars))
       (do ((vars vars (cdr vars))
            (reg (fx+ reg 1) (fx+ reg 1)))
           ((null? vars))
         (cond ((car vars)
                => (lambda (var)
                     (mark-temp var reg))))))
    (cond ((car vars)
           => (lambda (var)
                (mark var reg))))))
     
(define (n-ary-setup node)
  (generate-nary-setup (lambda-body node)
                       (fx- (length (lambda-variables node)) 3)))
    

(define (allocate-call node)
  (if *assembly-comments?* (emit-comment "~A" (pp-cps node)))
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (allocate-primop-call node))
          ((lambda-node? proc)
           (generate-let&labels node (lambda-body (call-proc node))
                                (call-args node)))
          ((variable-known (leaf-value proc))
           => (lambda (proc)
                (case (lambda-strategy proc)
                  ((strategy/label)
                   (allocate-label-call node proc))
                  (else
                   (allocate-general-call node)))))
          ((fx= (call-exits node) 0)
           (allocate-return node))
          ((fx= (call-exits node) 1)
           (allocate-general-call node))
          (else
           (bug "too many exits - ~s" node)))))
  

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
  (generate-jump node (label proc)))


  
(define (allocate-general-call node)
  (let ((cont ((call-arg 1) node)))
    (cond ((lambda-node? cont)     
           (make-stack-closure node cont)
           (parallel-assign-general node))
          (else                          
           (parallel-assign-general node)          
           (fetch-continuation node (leaf-value cont)))))
  (clear-slots)
  (generate-general-call node (fx- (length (call-args node)) 1)))

                                   
(define (allocate-return node)
  (parallel-assign-return node)      
  (fetch-continuation node (leaf-value (call-proc node)))
  (clear-slots)
  (generate-return node 1))
                         

(define (parallel-assign-general node)
  (parallel-assign node (cons (call-proc node) (cdr (call-args node)))
                        nil t '()))

(define (parallel-assign-return node)
  (parallel-assign node (call-args node) nil nil '()))


;;; Parallel assignment
;;; divide arguments into two lists of pairs (arg-position . node) lambdas, refs
;;; If there are no closures to be consed we do the parallel assignment using
;;; an auxilliary register gotten on the fly if necessary (or temp).
;;; If there is one closure we cons it and move it into its register, then
;;; parallel assign the refs.  If there is more than one closure, we generate
;;; a closure-internal closure and move it into all of its places.


(define (parallel-assign node args p-list proc? solve-list)
  (let* ((pos-list (if p-list p-list (reg-positions (length args) proc?)))
         (big-lambda (get-big-lambda args)))
    (cond (big-lambda
           (make-heap-closure node big-lambda)
           (lock A5)
           (really-parallel-assign node args pos-list solve-list))
          (else
           (really-parallel-assign node args pos-list solve-list)))))

(define (get-big-lambda args)
  (any (lambda (arg)
         (and (lambda-node? arg)
              (neq? (environment-closure (lambda-env arg)) *unit*)
              (environment-closure (lambda-env arg))))
       args))


;;; do-now - register pairs (source . target)
;;; trivial - immediate or lambda or on stack
;;; do-later - environment


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
                                        (set (temp-loc value) nil))))
                            (mark (reg-node (car pair)) (cdr pair))
                            (lock (cdr pair)))
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
             (walk (lambda (pair)
                     (generate-move node (make-value-accessable node (car pair))
                                         (r (cdr pair)))
                     (mark (car pair) (cdr pair)))
                   do-later)))
      (walk (lambda (pair)
              (do-trivials node (car pair) (cdr pair) A5))
            trivial))))


(define (add-on-free-list do-now do-later list)
  (iterate loop ((pairs list) (do-now do-now) (do-later do-later))
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
          ((or (lambda-node? (car args))
               (immediate? (leaf-value (car args))))
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
           (cond ((eq? (environment-closure (lambda-env node)) *unit*)
                  (lambda-queue node)
                  (generate-move-address call-node
                        (lookup call-node node nil)   (r reg)))
                 ((fx= offset 0)
                  (generate-move call-node (r closure-reg) (r reg)))
                 (else
                  (generate-move-address call-node
                           (d@r closure-reg (fx- offset tag/extend))
                           (r reg))))))
        (else
         (generate-move call-node (make-leaf-accessable call-node node)
                                  (r reg)))))



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
                        (generate-move node (r (cdr pair)) (r XP))
                        (generate-move node (r (car pair)) (r (cdr pair)))
                        (loop1 (cdr pairs)
                               (delq (cdr pair) targets)
                               (cdr pair))))
                     ((not (assq (car candidates) pairs))
                      (let ((pair (cdrassq (car candidates) pairs)))
                        (generate-move node (if (eq? (car pair) temp)
                                                (r XP)
                                                (r (car pair)))
                                            (r (cdr pair)))
                        (loop1 (delq pair pairs)
                               (delq (cdr pair) targets)
                               temp)))
                     (else
                      (loop2 (cdr candidates))))))))
    (walk kill vals)
    (walk (lambda (pair value)
            (mark value (cdr pair)))
          pairs
          vals)))



(define (cdrassq x l)
  (do ((l l (cdr l)))
      ((eq? (cdar l) x)
       (car l))))






(define (make-stack-closure node cont)
  (lambda-queue cont)
  (if *assembly-comments?* (emit-comment "consing stack closure"))
  (walk (lambda (pair)
          (generate-push node (make-value-accessable node (car pair))))
        (reverse (cdr (closure-env (environment-closure (lambda-env cont))))))
  (generate-push-address node (label cont)))

                                                                            
(define (make-heap-closure node closure)
  (if *assembly-comments?* (emit-comment "consing heap closure"))
  (let* ((members (closure-members closure))
         (first (car members)))
    (walk (lambda (var)
            (lambda-queue (variable-binder var)))
          members)
    (free-register node A5)
    (generate-move-address node (label (variable-binder first))
                                (r A5))
    (generate-extend node (closure-size closure))
    (walk (lambda (pair)
      (let ((var (car pair))
            (offset (cdr pair)))
        (cond ((memq? var members)
               (generate-move-address node (label (variable-binder var))
                                      (d@r A5
                                           (fx- offset tag/extend))))
              (else
               (emit-comment "moving ~a" (variable-unique-name var))
               (generate-move node (make-value-accessable node var)
                                   (d@r A5
                                        (fx- offset tag/extend)))))))
      (cdr (closure-env closure)))))


(define (generate-extend node n)                      
  (free-register node S1)
  (free-register node S2)
  (free-register node A5)
  (generate-move node (lit n) (r S1))
  (generate-move node
                 (make-value-accessable node primop/%make-extend)
                 (r XP))
  (generate-jump-to-subroutine node))


(define (allocate-primop-call node)
  (let* ((primop (primop-value (call-proc node))))
    (cond ((primop.conditional? primop)
           (allocate-conditional-primop node primop))
          ((primop.special? primop)
           (primop.generate primop node))
          ((primop.accessor primop)
           (allocate-accessor node primop))
          (else           
           (really-allocate-primop-call node primop)))))
                                       

(define (allocate-conditional-primop node primop)
  (primop.generate primop node)
  (walk kill-if-dead (cddr (call-args node)))
  (bind ((*registers* (copy-registers))
         (*stack-pos* *stack-pos*)
         (*lambda* *lambda*)) 
    (emit-tag (then-cont node))
    (allocate-call (lambda-body (then-cont node)))
    (check-clear))
  (restore-slots)
  (emit-tag (else-cont node))
  (allocate-call (lambda-body (else-cont node))))
    
(define (really-allocate-primop-call node primop)
  (let ((c (cont node)))
    (primop.generate primop node)
    (walk kill-if-dead (cdr (call-args node)))
    (cond ((lambda-node? c)
           (allocate-call (lambda-body c)))
          (else                            
           (fetch-continuation node (leaf-value c))
           (clear-slots)
           (generate-return node 1)))))
           
                                
(define (allocate-accessor node primop)
  (let ((cont ((call-arg 1) node)))
    (cond ((or (leaf-node? cont)
               (need-to-dereference-accessor? node))
           (really-allocate-primop-call node primop))
          (else
           (mark-leaf-vias (cadr (lambda-variables cont)) node)
           (allocate-call (lambda-body cont))))))

(define (mark-leaf-vias var node)
  (walk (lambda (ref)
          (set (leaf-via ref) node))
        (variable-refs var)))
         

(define (need-to-dereference-accessor? node)
  t) (comment
  (let ((c-var (cadr (lambda-variables ((call-arg 1) node))))
        (var (leaf-value ((call-arg 2) node))))
    (or (fx> (length (variable-refs c-var)) 1)
        (and (fx> (length (variable-refs var)) 1)
             (any? (lambda (ref)
                     (let ((parent (node-parent ref)))
                       (and (fx= (call-exits parent) 1)
                            (or (not (primop-node? (call-proc parent)))
                                (eq? (primop-value (call-proc parent))
                                     set-value-primop)))))
                   (variable-refs var))))))
           
(define (make-leaf-accessable node leaf)
  (cond ((leaf-via leaf)
         => (lambda (loc)
             ((primop.accessor (primop-value (call-proc loc))) node loc)))
        (else
         (make-value-accessable node (leaf-value leaf)))))



(define (make-value-accessable node value)
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
                       (r nil-reg))
                      ((immediate? value)
                       (lit value))
                      (else
                       (lookup node value nil))))))))

(define (lookup node value lambda-bound?)
  (cond ((and lambda-bound? (variable-known value))
         => (lambda (label)
              (make-value-accessable node (lambda-self-var label))))
        (else
         (xcase (lambda-strategy *lambda*)
           ((strategy/stack)
            (fetch-from-stack node value lambda-bound?))
           ((strategy/heap strategy/ezclose)
            (let ((contour (car (lambda-variables *lambda*))))
              (into-register 'pointer node contour
                  (make-value-accessable node contour) '*)
              (fetch-from-heap node contour value lambda-bound?)))))))




;;; This isn't called from this file or vaxprimops
(define (make-locative-accessable node ref)
  (let ((var (leaf-value ref)))
    (cond ((variable-binder var)
           (bug "set on bound variables"))
          (else
           (make-value-accessable node var)))))

                                
(define (access-from-unit node contour var)
  (let ((closure (environment-closure (lambda-env (variable-binder contour)))))
    (cond ((closure-cit-offset closure)
           => (lambda (current-offset)
                (cond ((eq? contour (car (closure-members closure)))
                       (d@r  TP (fx- (cdr (assq var (closure-env *unit*)))
                                     (fx+ current-offset 6))))
                      (else
                       (generate-move-address node
                          (d@r (register-loc contour)
                           (fx- 0
                                (cdr (assq contour (closure-env closure)))))

                          XP)
                       (d@r  XP (fx- (cdr (assq var (closure-env *unit*)))
                                     (fx+ current-offset 6)))))))
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
      ((strategy/label)
       (loop offset (node-parent (node-parent l))))
      (else
       (let ((env (closure-env (environment-closure (lambda-env l)))))
         (cond ((and lambda-bound? (assq value env))
                => (lambda (env-pair)
                     (d@r SP (fx+ *stack-pos*
                                   (fx+ offset (cdr env-pair))))))
               ((and (cdr env)
                     (eq? (variable-number (caadr env)) 0))
                (let ((accessor (d@r SP (fx+ *stack-pos*
                                             (fx+ offset 4))))
                      (contour (caadr env)))
                  (into-register 'pointer node contour accessor '*)
                  (fetch-from-heap node contour value lambda-bound?)))
               (else
                (loop (fx+ (closure-size (environment-closure (lambda-env l)))
                           offset)
                      (node-parent (node-parent l))))))))))
                          



(define (closure-internal-closure? value)
  (and (variable? value) (variable-known value)))

(define (fetch-from-heap node contour value lambda-bound?)
  (iterate loop ((env (get-env contour)) (contour contour)) 
    (let ((a-list (closure-env (environment-closure env)))
          (current-offset (environment-cic-offset env)))
      (cond ((assq value a-list)
             => (lambda (pair)
                  (if (closure-internal-closure? value)
                      (list (d@r (register-loc contour)
                                 (fx- (cdr pair) current-offset)))
                      (d@r (register-loc contour)
                           (fx- (cdr pair)
                                (fx+ current-offset tag/extend))))))
            ((and (not lambda-bound?) (access-from-unit node contour value)))
            (else
             (into-register 'pointer node (caadr a-list)
                (d@r  (register-loc contour)
                      (fx+ current-offset tag/extend))
                '*)
             (loop (get-env (caadr a-list)) (caadr a-list)))))))
                 
(define (fetch-continuation node var)            
  (let ((access (fetch-continuation-from-stack node var)))
    (if (fxn= (cdr access) 0)
        (generate-move-address node access (r SP)))))
                                          
;;; Code to get a continuation off the stack.

(define (fetch-continuation-from-stack node var)
  (iterate loop ((offset 0) (l (node-parent node)))
    (cond ((eq? var (cadr (lambda-variables l)))
           (d@r SP offset))
          (else
           (case (lambda-strategy l)
             ((strategy/stack strategy/ezclose)
              (loop (fx+ (closure-size (environment-closure (lambda-env l)))
                         offset)
                   (node-parent (node-parent l))))
             (else
              (loop offset
                    (node-parent (node-parent l)))))))))

(define (->register type node ref where)
  (let ((accessor (make-leaf-accessable node ref)))
    (cond ((and (register? accessor)
                (or (and (eq? (reg-type accessor) type)
                         (eq? where '*))
                    (eq? accessor where)))
           accessor)
          (else  
           (into-register type node (leaf-value ref) accessor where)))))
          
(define (gently->register type node ref where)
  (->register type node ref (if (reg-node where) '* where))) 
                                           
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
         (really-get-register 'pointer *scratch-registers* *no-of-registers*))
        (else
         (really-get-register 'scratch
                              0
                              *scratch-registers*))))


(define (really-get-register type start stop)
  (iterate loop ((i start))
    (cond ((fx>= i stop)
           (select-and-kick-register node type))
          ((not (reg-node i))
           i)
          (else
           (loop (fx+ i 1))))))

(define (into-register type node value access where)
  (cond ((in-register? type value where)
         (register-loc value))
        (else         
         (let ((reg (get-register type node where)))
           (generate-move node access (r reg))
           (cond ((register-loc value)
                  => (lambda (reg)
                       (set (reg-node reg) nil))))
           (mark value reg)
           reg))))


;;; SELECT-REGISTER The first register which is not reserved or used soon is
;;; selected.  If none satisfy then the first register  is selected,  to make
;;; parallel assignment work reasonably.  This is because the parallel assign-
;;; ment is set up to try to move the argument to pointer register 1 last.
                                          
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
              (cond ((eq? (reg-type reg) 'pointer)
                     (kick-register node reg))
                    ((vector-posq nil *registers*)
                      => (lambda (new-place)
                           (mark value new-place)
                           (set (reg-node reg) nil)
                           (generate-move node (r reg) (r new-place))))
                    (else
                     (bug "scratch-registers all full")))))))


(define (kick-register node reg) 
  (let ((value (reg-node reg)))
    (cond ((locked? reg)
           (error "attempt to kick out of locked register"))
          ((or (temp-loc value)
               (fixnum? value))
             ;      (dying? value node)
           (set (register-loc value) nil)
           (set (reg-node reg) nil))
          (else
           (let ((temp (get-temp value)))
             (set (register-loc value) nil)
             (set (temp-loc value) temp)
             (set (reg-node reg) nil)
             (generate-move node (r reg) (temp-access temp)))))))


(define (get-temp value)
  (iterate loop ((i *real-registers*))
    (cond ((fx>= i *no-of-registers*)
           (bug "all temporaries used"))
          ((not (temp-node i))
           (set (temp-node i) value)
           i)               
          (else 
           (loop (fx+ i 1))))))

(define (cont node)
  (car (call-args node)))
             
(define (continuations node)
  (sublist (call-args node) 0 (call-exits node)))

(define (then-cont node)
  (car (call-args node)))

(define (else-cont node)
  (cadr (call-args node)))

(define (cont-vars node)
    (lambda-variables (car (call-args node))))

(define (cont-var node)
    (car (cont-vars node)))

(define (kill-if-dead node)
  (if (and (leaf-node? node)
           (dying? (leaf-value node) (node-parent node)))
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
                         

(lset *pos-lists1* '#(() (5) (5 6) (5 6 7) (5 6 7 8) (5 6 7 8 10)
                      (5 6 7 8 10 11)))
(lset *pos-lists2* '#(() (4) (4 5) (4 5 6) (4 5 6 7) (4 5 6 7 8)
                      (4 5 6 7 8 10) (4 5 6 7 8 10 11)))

(define (reg-positions i proc?)       
  (vref (if proc? *pos-lists2* *pos-lists1*) i))


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
