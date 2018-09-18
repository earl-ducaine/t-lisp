(herald flow
        (syntax-table *orbit-syntax-table*))

;;; ---------------------------------------------------------------------
;;; Flow analysis.

;;; The place to start is page 497 of Aho & Ullman.
;;; Four general kinds of data flow problems are enumerated.
;;; Bracketed topics are additional applications for our purposes.

;;;   I.   Forward - union
;;;          Reaching definitions = ud-chaining (uses point to defs)
;;;            [Closure environment availability - recursion analysis]
;;;   II.  Forward - intersection
;;;          Available expressions
;;;          Copy propagation
;;;   III. Backwards - union
;;;          Live variables
;;;            [Eliminating redundant type checks]
;;;          du-chaining
;;;            [Environment analysis]
;;;   IV.  Backwards - intersection
;;;          Very busy expressions
;;;            [Optimization of type verification]

;;; Basic block

(define-structure-type bb
  head          ; The first statement.
  tail          ; The last statement.
  dfn           ; Depth first number.
  next          ; Link to next node in depth-first order.
  prev          ; Link to previous node in depth-first order.
  predecessors  ; List of predecessors in flow graph.
  changed?      ; Flag.
  ud            ; Flow problem parameter blocks.
  avail
  live
  busy)

;;; Flow problem parameter block

(define-structure-type fp
  ;; Lists of (variable type) pairs.
  def           ; Type assertions provided.
  use           ; Type assertions used.
  in
  out)

;;; Flow problem

(define-structure-type problem
  bb-selector
  direction
  merge-function
  union?)

(define-local-syntax (define-problem name selector dir merge union-or-intersect)
  `(define ,name
     (let ((%problem% (make-problem)))
       (set (problem-bb-selector %problem%)
            (lambda (bb) (,selector bb)))
       (set (problem-direction %problem%)
            (lambda (bb) (,dir bb)))
       (set (problem-merge-function %problem%)
            merge-function)
       (set (problem-union? %problem%)
            ,(xcase union-or-intersect ((union) nil) ((intersect) t))))))

(define-problem ud    bb-ud    bb-next type-union     union)
(define-problem avail bb-avail bb-next type-intersect intersect)
(define-problem live  bb-live  bb-prev type-union     union)
(define-problem busy  bb-busy  bb-prev type-intersect intersect)

(define (flow-analyze bb problem)
  (let ((direction (problem-direction problem)))
    (iterate loop ((bb bb) (stop-at bb))
      (let ((next (direction bb)))
        (cond ((eq? next stop-at) 'done)
              (else
               (loop next
                     (if (propagate bb problem) next stop-at))))))))

(define (propagate bb problem)
  (let ((fp ((problem-bb-selector problem) bb)))
    (enny (lambda (pred)
            (flow-merge!
             (fp-in fp)
             (fp-out ((problem-bb-selector problem) pred))
             (problem-merge-function problem)
             (problem-union? problem)
             (lambda (new-in changed?)
               (cond (changed?
                      (set (fp-in fp)  newin)
                      (set (fp-out fp)
                           (flow-union!
                              (flow-setdiff in (fp-kill bb))        ;??
                              (fp-gen bb)
                              proj0))
                      t)
                     (else nil)))))
          (bb-predecessors bb))))

(define (flow-union! in out cont)
  (flow-merge! in out t cont))

(define (flow-intersect! in out cont)
  (flow-merge! in out nil cont))

;;; Continuation argument takes changed?, new-dest

(define *hack* (list nil))

(define (flow-merge! in out merge-function union? cont)
  (cont
   (cdr *hack*)
   (iterate loop ((l in)
                  (r out)
                  (change? nil)
                  (prev *hack*))      ; Place to store copied tail
     (cond ((null? l)
            (cond (union?
                   (cond ((null? r) change?)
                         (else (set (cdr prev) (copy-list r)) t)))
                  (else change?)))
           ((null? r)
            (cond ((not union?) (set (cdr prev) '()) t)
                  (else change?)))
           (else
            (let* ((change (lambda (new-l next-l new-r)
                             (set (cdr prev) new-l)
                             (loop next-l
                                   new-r
                                   t
                                   l)))
                   (no-change (lambda (new-l new-r)
                                (loop new-l
                                      new-r
                                      change?
                                      l)))
                   (z1 (car l))
                   (z2 (car r))
                   (z (merge-function z1 z2)))
              (cond ((eq? z '>)
                     (cond (union?
                            ;; Variable is already in in.  Skip over it.
                            (no-change (cdr l) r))
                           (else
                            ;; Variable is in in, but not in out.  Delete it.
                            (change (cdr l) (cdr l) r))))
                    ((eq? z '<)
                     (cond (intersect?
                            ;; Variable is in out but not in.  Ignore it.
                            (no-change l (cdr r)))
                           (else
                            ;; Needs to be merged into the in list.
                            (change (cons (car r) l) l (cdr r))))))))))))
                    ((eq? (car z1) (car z2))
                     (let ((type (merge-function
                                  (cdr z1)
                                  (cdr z2))))
                       (cond ((neq? type (cdr z1))
                              ;; Type has changed.
                              (change (cons (cons (car z1) type) l)
                                      (cdr l)
                                      (cdr r)))
                             (else
                              ;; No type change; simply proceed.
                              (no-change (cdr l)
                                         (cdr r))))))

(define (var-type-merger z1 z2 merge)
  (cond ((eq? z1 z2) t)
        ((neq? (car z1) (car z2))
         (if (fx> (variable-id (car z1)) (variable-id (car z2))) '> '<))
        (else
         (let ((merged-type (merge (cdr z1) (cdr z2))))
           (cond ((eq? merged-type (cdr z1)) z1)
                 ((eq? merged-type (cdr z2)) z2)
                 (else (cons (car z1) merged-type)))))))

;;; Type lattice

(define (type-union type1 type2)
  (cond ((alikev? type1 type2) type1)   ; careful about nodes
        ((eq? type1 'bottom?)  type2)
        ((eq? type2 'bottom?)  type1)
        ((or (atom? type1)
             (atom? type2)
             (neq? (car type1) (car type2)))
         'top?)
        (else (cons (car type1)
                    (map type-union (cdr type1) (cdr type2))))))

(define (type-intersect type1 type2)    ; greatest lower bound
  (cond ((eq? type1 type2) type1)
        ((eq? type1 'top?) type2)
        ((eq? type2 'top?) type1)
        (else 'bottom?)))  ; etc., like above.





;;;         ||||||                            ||||||
;;;         vvvvvv  these are older comments  vvvvvv



;;; The output that we want includes:
;;;   For every variable:
;;;     Its type at point of binding.
;;;   For every statement:
;;;     The type of each variable live at the statement.
;;;     The environment frame with respect to which this variable must
;;;       be accessed.  (These environments are themselves variables on
;;;       which we must do flow analysis.)

;;; For every LAMBDA node in the tree, we fill in:
;;;   lambda-live - list of live variables on entry to the lambda.

;;; For every VARIABLE structure V, we fill in:
;;;   closures - list of LAMBDA nodes N for which the following hold:
;;;                (a) N is a closure (rep/pointer)
;;;                (b) V is live at N
;;;                (c) there are no nodes superior to N at which V is live
;;;   frames - similarly, for continuation LAMBDA's.

(define (flow-analyze node)
  (cond ((lambda-node? node)
         (set (lambda-live node) nil)
         (walk (lambda (var)
                 (cond ((and var (not (variable-known var)))
                        (set (variable-closures var) '())
                        (set (variable-frames var) '())
                        (walk (lambda (ref)
                                (set-live var ref))
                              (variable-refs var)))))
               (lambda-variables node))
         (flow-analyze-call (lambda-body node)))))

(define (flow-analyze-call node)
  (walk flow-analyze (call-proc+args node))
  (let ((proc (call-proc node)))
    (cond ((lambda-node? proc)
           (flow-analyze-labels (cdr (lambda-variables proc))))
          ((and (literal-node? proc)
                (eq? (literal-value proc) Y-primop))
           (let ((l (cddr (lambda-variables (cadr (call-args node))))))
             (iterate loop ()
                (if (flow-analyze-labels l) (loop))))))))

(define (flow-analyze-labels vars)
  (enny (lambda (var)
          (cond ((and var (variable-known var))
                 ;; The live variables of a known procedure are live
                 ;; at every call to the procedure.
                 => (lambda (label)
                      (enny (lambda (ref)
                              (enny (lambda (z)
                                      (set-live z ref))
                                    (lambda-live label)))
                            (variable-refs label))))))
        vars))

(define (enny proc l)       ; Sort of like ANY, but keeps going after success
  (do ((flag nil (or flag (proc (car l))))
       (l l (cdr l)))
      ((null? l) flag)))

;;; Assert that the variable is live at the given leaf node.

(define (set-live var node)
  (let ((node (node-parent (node-parent node)))
        (stop (variable-binder var)))
    (cond ((or (eq? node stop)
               (memq? var (lambda-live node)))
           nil)
          (else
           (iterate loop ((node node)
                          (frame nil)
                          (closure nil))
             (cond ((or (eq? node stop)
                        (memq? var (lambda-live node)))
                    (if closure (push closure (variable-closures var)))
                    (if (and frame (neq? frame closure))        ; hack
                        (push frame (variable-frames var)))
                    t)
                   (else
                    (push var (lambda-live node))
                    (let ((p (node-parent (node-parent node))))
                      (xcase (lambda-rep node)
                        ((rep/label) closure)
                         (loop p frame closure))
                        ((rep/continuation)
                         (loop p node closure))
                        ((rep/pointer)
                         (if frame
                             (set-live (car (lambda-variables node))
                                       frame))
                         (if closure
                             (set-live (car (lambda-variables node))
                                       closure))     ; env backpointer
                         (loop p node node))))))))))
