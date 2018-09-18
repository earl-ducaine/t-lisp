(herald closure
        (syntax-table *orbit-syntax-table*))

;;; Live variable analysis
;;; Here we record (for each lambda-node) the list of all free variables
;;; of the lambda-body.  We also set the strategy for the lambda.  For now
;;; this means:
;;;    For LABELS, if all references to a label are calls, that label is given
;;;       strategy/label.
;;;    A LET lambda or the continuation to an open-coded-primop is also
;;;       strategy label.
;;;    The continuation to a call is strategy/stack.
;;;    Anything else is strategy/heap.
;;; Labels is analyzed iteratively, picking up variables until nothing changes. 

(define (live-analyze-top node)
  (set (lambda-strategy node) 'strategy/top)
  (set *unit-literals* '())
  (live-analyze (car (call-args (lambda-body node))) 
                (lambda-self-var node)))
     
(define (live-analyze node global)
  (cond ((lambda-node? node)
         (live-analyze-lambda node global))
        ((leaf-node? node)
         (live-analyze-leaf node global))
        (else
         (bug "live-analyze called on a call-node ~S" node))))

(define (live-analyze-lambda node global)
  (let ((live (setdiff (live-analyze-body (lambda-body node) global)
                       (lambda-all-variables node))))
    (obtain-lambda-strategy node)
    (case (lambda-strategy node)
      ((strategy/heap)
       (cond ((or (null? live)
                  (every? (lambda (var)
                            (or (eq? var global) (not (variable-binder var))))
                          live))
              (set (lambda-live node) live)
              (set (lambda-env node) 'unit-internal-closure))
             ((not (memq? global live))
              (set (lambda-live node) live))
             (else
              (set (lambda-live node) (delq global live))
              (set (lambda-env node) 'unit-internal-template))))
      (else
       (set (lambda-live node) live)))
    live))

(define (live-analyze-leaf node global)
  (cond ((literal-node? node)
         (cond ((immediate? (leaf-value node))
                '())
               (else
                (if (not (memq? (leaf-value node) *unit-literals*))
                    (push *unit-literals* (leaf-value node)))
               `(,global))))
        ((primop-node? node)
         (cond ((primop.closed-compiled? (primop-value node))
                (if (not (memq? (leaf-value node) *unit-literals*))
                    (push *unit-literals* (leaf-value node)))
                `(,global))
               (else '())))
        ((or (not (reference-node? node))
             (bound-to-continuation? (reference-variable node)))
         '())
        ((variable-known (reference-variable node))
         => (lambda (label)
              (case (lambda-strategy label)
                ((strategy/label)
                 (lambda-live label))
                (else
                 (cond ((labels-variable? (reference-variable node))
                        `(,(lambda-self-var label)))
                       (else
                        `(,(reference-variable node))))))))
        ((variable-binder (reference-variable node))
         `(,(reference-variable node)))
        (else
         `(,global))))


(define (labels-variable? var)
  (let ((proc (call-proc (node-parent (variable-binder var)))))
    (and (leaf-node? proc) (eq? (leaf-value proc) primop/y))))


(define (live-analyze-body node global)
  (let ((proc (call-proc node)))
    (cond ((and (primop-node? proc)
                (eq? (primop-value proc) primop/Y))
           (live-analyze-Y ((call-arg 1) node) ((call-arg 2) node) global))
          ((lambda-node? proc)
           (live-analyze-let node global))
          (else
           (do ((args (call-proc+args node) (cdr args))
                (vars '() (union (live-analyze (car args) global) vars)))
               ((null? args)
                (if (any? (lambda (node)
                            (and (lambda-node? node)
                                 (eq? (lambda-strategy node) 'strategy/heap)))
                          (call-args node))
                    (adjoin global vars)
                    vars)))))))




(define (live-analyze-let node global)
  (walk (lambda (var node)
          (cond ((not var))
                ((exit-refs? var)
                 (set (lambda-strategy node) 'strategy/stack))
                ((all-refs-are-calls? var)
                 (set (lambda-strategy node) 'strategy/label))
                (else
                 (set (lambda-strategy node) 'strategy/heap))))
        (lambda-variables (call-proc node))
        (call-args node))
  (set (lambda-strategy (call-proc node)) 'strategy/open)
  (do ((lambdas (call-args node) (cdr lambdas))
       (live (if (any? (lambda (node)
                         (eq? (lambda-strategy node) 'strategy/heap))
                       (call-args node))
                 `(,global)
                  '())
             (union live (live-analyze (car lambdas) global))))
      ((null? lambdas)
       (union live (live-analyze (call-proc node) global)))))

(define (exit-refs? var)
  (any? (lambda (ref)
          (fx<= (call-arg-number (node-role ref))
                (call-exits (node-parent ref))))
        (variable-refs var)))


(define (live-analyze-Y cont-node lambda-node global)
  (let* ((label-exprs (cdr (call-args (lambda-body lambda-node))))
         (body-expr ((call-arg 1) (lambda-body lambda-node))))
    (set (lambda-strategy body-expr) 'strategy/open)
    (walk (lambda (label-var expr)
            (if label-var (set (variable-type label-var) expr))
            (cond ((or (not label-var)
                       (all-refs-are-calls? label-var))
                   (set (lambda-strategy expr) 'strategy/label))
                  (else
                   (set (lambda-strategy expr) 'strategy/heap))))
          (cdr (lambda-variables lambda-node))
          label-exprs)
    (set-label-live label-exprs global)
    (do ((exprs label-exprs (cdr exprs))
         (live (if (any? (lambda (node)
                           (eq? (lambda-strategy node) 'strategy/heap))
                         label-exprs)
                   (adjoin global (live-analyze body-expr global))
                   (live-analyze body-expr global))
               (union live (lambda-live (car exprs)))))
        ((null? exprs)
         (setdiff live (map lambda-self-var label-exprs))))))
         

(define (set-label-live label-exprs global)
  (iterate again ()
    (iterate loop ((lambdas label-exprs) (changed? nil))
      (cond ((not (null? lambdas))
             (let ((live (setdiff (live-analyze-body
                                    (lambda-body (car lambdas))
                                    global)
                                  (lambda-all-variables (car lambdas)))))
               (cond ((set-eq? (lambda-live (car lambdas)) live)
                      (loop (cdr lambdas) changed?))
                     (else
                      (set (lambda-live (car lambdas)) live)
                      (loop (cdr lambdas) t)))))
            (changed?
             (again))
            (else
             nil)))))

(define (obtain-lambda-strategy node)
 (cond ((lambda-strategy node)
        => identity)
       (else
        (set (lambda-strategy node)
             (determine-lambda-strategy node)))))

(define (determine-lambda-strategy node)
  (let* ((parent (node-parent node))
         (proc   (call-proc parent)))
    (cond ((or (and (fx<= 2 (call-exits parent))
                    (call-exit? node))
               (and (call-exit? node)
                    (open-coded-primop? proc)))
           'strategy/open)
          ((call-exit? node)
           'strategy/stack)
          (else
           'strategy/heap))))

(define (open-coded-primop? node)
  (and (primop-node? node)
       (not (primop.closed-compiled? (primop-value node)))))


;;; Closure analysis.
;;;=========================================================================

(define (close-analyze-top node free-variables defined-variables)
    (set *unit-closures* nil)
    (set *unit-templates* nil)
    (let* ((l ((call-arg 1) (lambda-body node)))
           (env (list (lambda-self-var node)))
           (via (lambda-self-var l)))
      (close-analyze-body (lambda-body l) env via env via)
      (set *unit* (create-unit (cons primop/%make-extend
                                     (cons primop/%make-pair
                                           free-variables))
                               *unit-literals*
                               *unit-templates*
                               *unit-closures*
                               defined-variables))
      (noise "Unit = ~a~%" (closure-env *unit*))
      (create-environment l *unit* 0)
      (create-comex (closure-env *unit*) *unit-templates* l nil)))


(define (close-analyze-body node  senv svia henv hvia)
  (cond ((and (primop-node? (call-proc node))
              (eq? (primop-value (call-proc node)) primop/Y))
         (really-close-analyze-body
                       (call-args (lambda-body ((call-arg 2) node)))
                       senv svia henv hvia))
        (else
         (really-close-analyze-body (call-proc+args node)
                                    senv svia henv hvia))))


(define (really-close-analyze-body nodes senv svia henv hvia)
  (receive (live cics)
           (accumulate-environment nodes senv svia henv hvia)
    (cond (cics
           (let* ((cic-vars (map lambda-self-var cics))
                  (live (setdiff live cic-vars))
                  (link (if (intersection? live henv) hvia nil))
                  (delta (setdiff live henv)))
             (walk (lambda (cic)
                     (close-analyze-body (lambda-body cic)
                                         live
                                         (lambda-self-var cic)
                                         live
                                         (lambda-self-var cic)))
                   cics)
             (create-closure link cic-vars delta)))
          (else nil))))



(define (accumulate-environment nodes senv svia henv hvia)
  (iterate loop ((nodes nodes) (live '()) (cics '()))
    (cond ((null? nodes)
           (values live cics))
          ((not (lambda-node? (car nodes)))
           (loop (cdr nodes) live cics))
          (else
           (xcase (lambda-strategy (car nodes))
             ((strategy/heap)
              (cond ((eq? (lambda-env (car nodes)) 'unit-internal-closure)
                     (push *unit-closures* (car nodes))
                     (let ((env (lambda-live (car nodes)))
                           (via (lambda-self-var (car nodes))))
                       (close-analyze-body (lambda-body (car nodes))
                                           env via env via)
                       (loop (cdr nodes) (union env live) cics)))
                    (else
                     (loop (cdr nodes)
                           (union (lambda-live (car nodes)) live)
                           (adjoin (car nodes) cics)))))
             ((strategy/open)
              (close-analyze-body (lambda-body (car nodes)) senv svia henv hvia)
              (loop (cdr nodes) live cics))
             ((strategy/label)
              (close-analyze-label (car nodes) senv svia henv hvia)
              (loop (cdr nodes) live cics))
             ((strategy/stack)
              (close-analyze-stack (car nodes) senv svia henv hvia)
              (loop (cdr nodes) live cics)))))))


(define (close-analyze-stack node stackenv stackvia heapenv heapvia)
  (let* ((live (lambda-live node))
         (link-set (if (intersection? live stackenv) `(,stackvia) '()))
         (closure-env (setdiff live stackenv)))
    (create-closure (if (and link-set (eq? stackvia heapvia)) heapvia nil)
                    (list (lambda-self-var node))
                    closure-env)
    (close-analyze-body (lambda-body node)
                        live
                        (lambda-self-var node)
                        heapenv heapvia)))

(define (close-analyze-label node stackenv stackvia heapenv heapvia)
  (let ((delta (setdiff (lambda-live node) heapenv)))
    (set (lambda-env node)
         (create-join-point delta (if (intersection? (lambda-live node)
                                                     heapenv)
                                      heapvia
                                      nil)))
    (noise "join-point (~D) ~a env = ~S~%"
           (object-hash node)
           (lambda-name node)
           (map variable-unique-name delta))
    (close-analyze-body (lambda-body node)
                        heapenv
                        heapvia
                        heapenv heapvia)))


(define (set-eq? s1 s2)
  (if (fx= (length s1) (length s2))
      (every? (lambda (x) (memq? x s2)) s1)
      nil))      
        
;;; Environment structure is the lambda-env slot of each lambda which is
;;; strategy/stack or strategy/heap. The variables are sorted by size.
;;; (For stack closures) a continuation is represented as offset -1 in the
;;;  a-list.

(lset *unit* nil)
(lset *unit-closures* nil)
(lset *unit-templates* nil)
(lset *unit-literals* nil)


(define-structure-type environment
  closure    ; the closure this environment is a member of
  cic-offset ; offset of this environment's descriptor in the closure
  )

(define-methods handle-environment
  ((print self stream)
   (format stream "#{Environment_~S in Closure_~S}"
           (object-hash self)    
           (object-hash (environment-closure self)))))

(define-structure-type closure
  members     ; list of closure-internal-closures (variables)
  env         ; a-list of variables and offsets in the closure (in bytes)
  pointer     ; number of pointer slots
  scratch     ; number of scratch slots
  size        ; total size of closure (in bytes)
  cit-offset  ; offset of first
  )

(let ((closure (stype-master closure-stype)))
  (set (closure-members  closure) '())
  (set (closure-env      closure) '()))

(define-methods handle-closure
  ((print self stream)
   (format stream "#{Closure_~S with ~D vars, cics ~S}"
           (object-hash self)    
           (length (closure-env self))
           (map variable-unique-name
                (closure-members self)))))

(define-structure-type join-point
  env                  ;;; free variables
  arg-specs            ;;; list of numbers for argument-positions
  global-registers     ;;; list of (register . variable)
  contour              ;;; nearest superior template
  )

(define (create-join-point env contour)
  (let ((j (make-join-point)))
    (set (join-point-env j) env)
    (set (join-point-arg-specs j) 'not-yet-determined)
    (set (join-point-global-registers j) nil)
    (set (join-point-contour j) contour)
    j))


(define (create-unit variables literals templates closures defined)
 (let ((unit (make-closure)))
   (do ((vars (append variables defined literals) (cdr vars))
        (count CELL (fx+ count CELL))
        (a-list '() `((,(car vars) . ,count) ,@a-list)))
       ((null? vars)
        (do ((closures *unit-closures* (cdr closures))
             (count count (fx+ count CELL))
             (a-list a-list `((,(car closures) . ,count) ,@a-list)))
            ((null? closures)
             (mark-template-offsets unit templates count)
             (set (closure-env unit)  (reverse! a-list))
             (set (closure-cit-offset unit) nil)
             unit)
          (create-environment (car closures) unit count))))))




(define (mark-template-offsets unit templates count)
  (do ((offset count (fx+ offset 12))
       (templates templates (cdr templates)))
      ((null? templates))
    (set (closure-cit-offset (car templates))
         offset)))





(define (create-env-a-list pointer scratch)
  (do ((vars `(,@pointer . ,(sort! scratch scratch-compare)) (cdr vars))
       (count 0 (fx+ count (rep-size (variable-rep (car vars)))))
       (a-list '() `((,(car vars) . ,count) . ,a-list)))
      ((null? vars)
       (reverse! a-list))))

(define (rep-size rep)
  CELL)

(define (create-closure link cics vars)
  (let ((closure (make-closure))
        (front (cons (car cics) (if link (cons link (cdr cics)) (cdr cics)))))
    (receive (pointer scratch)
             (sort-vars vars)
      (let ((scratch-slots (length scratch))
            (pointer-slots (fx+ (length pointer) (length front)))
            (var-a-list (create-env-a-list
                          (append front pointer) scratch)))
          (set (closure-members closure) cics)
          (set (closure-cit-offset closure) nil)
          (set (closure-env        closure) var-a-list)
          (set (closure-scratch    closure) scratch-slots)
          (set (closure-pointer    closure) (fx- pointer-slots 1))
          (set (closure-size       closure)
               (fx* (fx+ scratch-slots pointer-slots) CELL))
          (create-environments var-a-list closure cics)
          closure))))

(define (create-environments var-a-list closure cics)
  (create-environment (variable-binder (car cics)) closure 0)
  (noise "~a (~d) ~s env = ~a~%" (lambda-strategy (variable-binder (car cics)))
          (object-hash (variable-binder (car cics)))
          (variable-unique-name (car cics))
          (map (lambda (var) (variable-unique-name (car var)))
               (closure-env closure)))
  (walk (lambda (cic)
          (create-environment (variable-binder cic)
                              closure
                              (cdr (assq cic var-a-list))))
        (cdr cics)))

(define (create-environment node closure offset)
  (let ((env (make-environment)))
    (set (environment-closure    env) closure)
    (set (environment-cic-offset env) offset)
    (if (and (eq? 'unit-internal-template (lambda-env node))
             (neq? closure (car *unit-templates*)))
        (push *unit-templates* closure))
    (set (lambda-env node) env)))

(define (sort-vars vars)
  (iterate loop ((vars vars) (pointer '()) (scratch '()))
    (cond ((null? vars)
           (values pointer scratch))
           ((eq? (variable-rep (car vars)) 'rep/pointer)
            (loop (cdr vars) (cons (car vars) pointer) scratch))
           (else
            (loop (cdr vars) pointer (cons (car vars) scratch))))))

(define (bound-to-continuation? var)
  (and (variable-binder var)
       (every? (lambda (ref)
                 (fx= (call-arg-number (node-role ref))
                       (call-exits (node-parent ref))))
               (variable-refs var))))


(define (scratch-compare var1 var2)
  var1)
