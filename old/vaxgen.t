(herald vaxgen)

(define (result-want-loc cont)
  (cond ((lambda-node? cont)
         (iterate loop ((refs (variable-refs (cadr (lambda-variables cont))))
                        (default nil))
           (if (null? refs)
               default
               (let* ((parent (node-parent (car refs)))
                      (proc (call-proc parent))
                      (number (call-arg-number (node-role (car refs)))))
                 (cond ((primop-node? proc)
                        (cond ((primop.arg-specs (primop-value proc))
                               => (lambda (specs)
                                    (let ((pos (nth specs
                                              (fx- (fx- number (call-exits parent)) 1))))
                                      (if (register? pos)
                                          pos
                                          (loop (cdr refs) pos)))))
                              (else '*)))
                       (else
                        (fx- (fx+ number 4) (call-exits parent))))))))
        (else A1)))


(define (generate-labels node)
  (let ((args (call-args (lambda-body ((call-arg 2) node)))))
    (generate-let&labels node (car args) (cdr args))))


(define (generate-let&labels node body exprs)
  (iterate loop ((exprs exprs) (jumps '()) (closures '()))
    (cond ((null? exprs)
           (really-generate-let&labels node body jumps closures))
          ((eq? (lambda-strategy (car exprs)) 'strategy/label)
           (loop (cdr exprs) (cons (car exprs) jumps) closures))
          (else
           (loop (cdr exprs) jumps (cons (car exprs) closures))))))


(define (really-generate-let&labels node body jumps closures)
  (if closures
      (let ((closure (environment-closure (lambda-env (car closures)))))
        (make-heap-closure node closure)
        (lock A5)
        (walk (lambda (closure)
                (let ((reg (get-register 'pointer node '*))
                      (offset (cdr (assq closure (closure-env closure)))))
                  (generate-move-address node (d@r A5 offset) (r reg))
                  (mark closure reg)))
              (filter (lambda (closure)
                        (memq? closure (lambda-live body)))
                      (cdr (closure-members closure))))
        (if (memq? (car (closure-members closure)) (lambda-live body))
            (mark (car (closure-members closure)) A5))))
  (walk (lambda (label)
          (set (join-point-contour (lambda-env label))
               (lambda-self-var *lambda*)))
        jumps)
  (allocate-call (lambda-body body)))


(define (get-or-set-join-state node lamb)
  (let ((join (lambda-env lamb)))
    (if (eq? (join-point-arg-specs join) 'not-yet-determined)
        (set-join-state node join lamb))
    join))


(define (set-join-state node join lamb)
  (lambda-queue lamb)
  (let ((args (fx- (length (lambda-variables lamb)) 2))
        (env (join-point-env join)))
    (if env
        (push (join-point-global-registers join) (cons A4 (car env))))
    (walk (lambda (var)
            (cond ((temp-loc var)
                   => (lambda (temp)
                        (push (join-point-global-registers join)
                              (cons temp var))))
                  (else
                   (let* ((access (make-value-accessable node var))
                          (temp (get-temp var)))
                       (set (temp-loc var) temp)
                       (cond ((register-loc var)
                              => (lambda (reg)
                                   (set (reg-node reg) nil)
                                   (set (register-loc var) nil))))
                       (push (join-point-global-registers join)
                             (cons temp var))
                       (generate-move node access (r temp))))))
          (cdr env))
    (set (join-point-arg-specs join) (reg-positions args nil))))



(define (first-n l n)
  (do ((i 0 (fx+ i 1))
       (l l (cdr l))
       (result '() (cons (car l) result)))
      ((fx>= i n) (reverse! result))))

(define (at-top-level? node)
  (eq? (environment-closure (lambda-env node)) *unit*))


    
(define (generate-block node)
  (destructure (((cont body) (call-args node)))
    (xcase (lambda-strategy cont)
      ((strategy/open)
       (allocate-call (lambda-body body))
       (allocate-call (lambda-body cont)))
      ((strategy/stack)
       (make-stack-closure node cont)
       (allocate-call (lambda-body body))))))



;;; Call-Xenoid
;;; ---------------------------------------------------------------------

(define (generate-call-xenoid node)
  (let* ((args (cddr (call-args node)))
         (nargs (length args))
         (cont (car (call-args node))))
    (do ((args (reverse args) (cdr args)))
        ((null? args))
      (generate-push node (make-leaf-accessable node (car args))))
    (free-register node S1)
    (free-register node P)
    (let ((reg (->register 'pointer node ((call-arg 2) node) '*)))
      (generate-move node (d@r reg 2) (r P))
      (icreate node "calls" (machine-num nargs) (d@r P 0)))
    (set *stack-pos* (fx- *stack-pos* (fx* nargs 4)))
    (if (lambda-node? cont)
        (mark (cadr (lambda-variables cont)) S1)
        (generate-move node (r S1) (r A1)))))


;;; Data manipulation
;;; ---------------------------------------------------------------------

(define (generate-define-var node)
  (let ((value ((call-arg 3) node))
        (ref ((call-arg 2) node)))
    (cond ((lambda-node? value)
           (lambda-queue value)
           (if (not (at-top-level? *lambda*))
               (generate-set node ref value)))
          (else
           (generate-set node ref value)))))


(define (generate-set node location value)
  (cond ((lambda-node? value)
         (make-closure node value)
         (lock A5)
         (generate-move node (r A5)
                     (access-locative node location))
         (unlock A5))
        (else
         (let ((access (make-leaf-accessable node value)))
           (protect-access access)
           (generate-move node access (access-locative node location))
           (release-access access)))))

;;; Fields within structured types

(define (generate-%set-accessor node)
  (let ((offset (leaf-via (call-proc node)))
        (value ((call-arg 3) node))
        (loc ((call-arg 2) node)))
    (cond ((lambda-node? value)
           (make-closure node value)
           (lock A5)
           (let ((reg (->register 'pointer node loc '*)))
             (generate-move node (r A5)
                   (d@r reg offset))
             (unlock A5)))
          (else
           (let ((access (make-leaf-accessable node value)))
             (protect-access access)
             (let ((loc-reg (->register 'pointer node loc '*)))
               (generate-move node access (d@r loc-reg offset))
               (release-access access)))))))

(define (generate-accessor node offset)
  (destructure (((cont arg) (call-args node)))
    (let* ((rwl (result-want-loc cont))
           (var (leaf-value arg))
           (dest (cond ((register? rwl) rwl)
                        (else
                         (get-register rwl node '*)))))
      (cond ((dying? var node)
             (->register 'pointer node arg dest)
             (kill var)
             (generate-move node (d@r dest offset) (r dest)))
            (else
             (lock dest)
             (let ((reg (->register 'pointer node arg '*)))
               (unlock dest)
               (free-register node dest)
               (generate-move node (d@r reg offset) (r dest)))))
      (mark-continuation node dest))))


(define (access-locative node ref)
  (cond ((leaf-via ref)
         => (lambda (loc)
              ((primop.accessor (primop-value (call-proc loc)))
                 node loc)))
        ((variable-binder (leaf-value ref))
         (d@r (->register 'pointer node loc '*) (fx- 4 tag/extend)))
        (else
         (make-value-accessable node (leaf-value ref)))))
                    

(define (fixed-accessor node loc-ref offset)
  (let ((reg (->register 'pointer node loc-ref '*))
        (loc-var (leaf-value loc-ref)))
    (if (and (dead? loc-var node)
             (every? (lambda (ref)
                       (or (lambda-node? ref) (neq? (leaf-value ref) loc-var)))
                     (cdr (call-args node))))
        (kill loc-var))
    (d@r reg offset)))


(define (generate-aref node)
  (destructure (((cont base index) (call-args node)))
    (let* ((rwl (result-want-loc cont))
           (b-var (leaf-value base))
           (i-var (leaf-value index))
           (dest (cond ((register? rwl) rwl)
                       (else
                        (get-register rwl node '*)))))
      (let ((b-reg (->register 'pointer node base '*)))
        (lock b-reg)
        (let* ((i-acc (make-leaf-accessable node index))
               (i-reg (cond ((dying? i-var node)
                            (kill i-var)
                            (get-register 'scratch node '*))
                           (else
                            (get-register 'scratch node '*)))))
          (unlock b-reg)
          (if (dying? b-var node) (kill b-var))
          (free-register node dest)
          (icreate node 'ashl (machine-num -2) i-acc (i-reg 2))
          (icreate node 'movb (index (d@r b-reg 2) i-reg) (r XP))
          (icreate node 'ashl (machine-num 8) (r XP) (r dest))
          (icreate node 'movb (machine-num %char) (r dest))
          (mark-continuation node dest))))))

(define (generate-%set-aref node)
  (destructure (((() base index value) (call-args node)))
    (icreate node 'ashl (machine-num -8) (make-leaf-accessable node value)
                         (r XP))
    (let* ((b-reg (->register 'pointer node base '*)))
      (lock b-reg)
      (let ((i-acc (make-leaf-accessable node index)))
        (protect-access i-acc)
        (let ((i-reg (get-register 'scratch node '*)))
          (icreate node 'ashl (machine-num -2) i-acc (r i-reg))
          (icreate node 'movb (r XP) (index (d@r b-reg 2) i-reg))
          (release-access i-acc)
          (unlock b-reg))))))

;;; Comparisons
;;; ---------------------------------------------------------------------

(define (primitive-comparator node inst)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let ((access (make-leaf-accessable node ref2)))
      (protect-access access)
      (icreate node 'cmpl (make-leaf-accessable node ref1) access)
      (emit-jump node inst else then)
      (release-access access))))


;;; Arithmetic
;;; ---------------------------------------------------------------------

(define (mark-continuation node reg)
  (let ((cont (car (call-args node))))
    (if (lambda-node? cont)
        (mark (cadr (lambda-variables cont)) reg)
        (generate-move node (r reg) (r A1)))))

(define (one-arg-primitive node)
  (destructure (((cont arg) (call-args node)))
    (let* ((rwl (result-want-loc cont))
           (var (leaf-value arg))
           (dest (cond ((register? rwl) rwl)
                       (else
                        (get-register rwl node '*)))))
      (cond ((and (eq? (register-loc var) dest) (dying? var node))
             (kill var)
             (values dest dest))
            (else
             (let ((access (make-leaf-accessable node arg)))
               (free-register node dest)
               (values access dest)))))))


(define (generate-comm-binop node)
  (destructure (((cont left right) (call-args node)))
    (let* ((rwl (result-want-loc cont))
           (lvar (leaf-value left))
           (rvar (leaf-value right))
           (dest (cond ((register? rwl) rwl)
                       (else
                        (get-register rwl node '*)))))
      (cond ((and (eq? (register-loc lvar) dest) (dying? lvar node))
             (lock dest)
             (let ((access (make-leaf-accessable node right)))
               (unlock dest)
               (kill lvar)
               (values access dest dest)))
            ((and (eq? (register-loc rvar) dest) (dying? rvar node))
             (lock dest)
             (let ((access (make-leaf-accessable node left)))
               (unlock dest)
               (kill rvar)
               (values access dest dest)))
            (else
             (let ((l-acc (make-leaf-accessable node left)))
               (protect-access l-acc)
               (let ((r-acc (make-leaf-accessable node right)))
                 (release-access l-acc)
                 (free-register node dest)
                 (values l-acc r-acc dest))))))))

(define (generate-non-comm-binop node)
  (destructure (((cont right left) (call-args node)))
    (let* ((rwl (result-want-loc cont))
           (lvar (leaf-value left))
           (rvar (leaf-value right))
           (dest (cond ((register? rwl) rwl)
                       (else
                        (get-register rwl node '*)))))
      (cond ((and (eq? (register-loc rvar) dest) (dying? rvar node))
             (lock dest)
             (let ((access (make-leaf-accessable node left)))
               (unlock dest)
               (kill rvar)
               (values access dest dest)))
            (else
             (let ((l-acc (make-leaf-accessable node left)))
               (protect-access l-acc)
               (let ((r-acc (make-leaf-accessable node right)))
                 (release-access l-acc)
                 (free-register node dest)
                 (values l-acc r-acc dest))))))))


(define (generate-early-binding-call node)
 (let ((primop (primop-value (call-proc node))))
  (walk (lambda (reg)
          (free-register node reg))
        (primop.used-registers primop))
  (parallel-assign node (cdr (call-args node)) (primop.arg-specs primop))
  (let ((access (make-leaf-accessable node (call-proc node))))
    (generate-move node access (r XP)))
  (generate-jump-to-subroutine node)
  (cond ((reg-node (primop.return-reg primop))
         => (lambda (var) (kill var))))
  (if (lambda-node? ((call-arg 1) node))
      (mark (cadr (lambda-variables ((call-arg 1) node))) 
            (primop.return-reg primop))
      (generate-move node (r (primop.return-reg primop)) (r A1)))))
