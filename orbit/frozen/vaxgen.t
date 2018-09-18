(herald vaxgen)

;;; RESULT-WANT-LOC Determines where to target the result of a primop.
;;; If the continuation is a variable, it is a return of one argument in
;;; A1.  Otherwise we look at the most important use (now done non-optimally)
;;; and see where is is needed.  If it is the argument to a primop we look
;;; at the arg-specs of that primop, otherwise it is in a position for the
;;; standard calling sequence.

(define (result-want-loc cont)
  (cond ((lambda-node? cont)
         (cond ((n-ary? cont) nil)
               (else
                (where-used (car (lambda-variables cont))))))
        (else A1)))


(define (where-used var)
  (iterate loop ((refs (variable-refs var)) (default nil))
    (if (null? refs)
        default
        (let* ((parent (node-parent (car refs)))
               (proc (call-proc parent))
               (number (call-arg-number (node-role (car refs)))))
          (cond ((primop-node? proc)
                 (cond ((primop.arg-specs (primop-value proc))
                        => (lambda (specs)
                             (let ((pos (nth specs
                                       (fx- (fx- number
                                                 (call-exits parent))
                                            1))))
                               (if (register? pos)
                                   pos
                                   (loop (cdr refs) pos)))))
                       (else '*)))
                (else
                 (fx- (fx+ number *scratch-registers*)
                      (call-exits parent))))))))



(define (generate-labels node)
  (let ((args (call-args (lambda-body ((call-arg 2) node)))))
    (generate-let&labels node (car args) (cdr args))))

;;; GENERATE-LET&LABELS Divide up the procedures depending on whether they
;;; need to be closed or can be jumped to.

(define (generate-let&labels node body exprs)
  (cond ((eq? (lambda-strategy (car exprs)) 'strategy/stack)
         (generate-join-let node body (car exprs)))
        (else
         (iterate loop ((exprs exprs) (jumps '()) (closures '()))
           (cond ((null? exprs)
                  (really-generate-let&labels node body jumps closures))
                 ((eq? (lambda-strategy (car exprs)) 'strategy/label)
                  (loop (cdr exprs) (cons (car exprs) jumps) closures))
                 ((eq? (environment-closure (lambda-env (car exprs))) *unit*)
                  (loop (cdr exprs) jumps closures))
                 (else
                  (loop (cdr exprs) jumps (cons (car exprs) closures))))))))


(define (generate-join-let node body cont)
  (make-stack-closure node cont)
  (allocate-call (lambda-body body)))


;;; REALLY-GENERATE-LET&LABELS If necessary, cons a closure and make the
;;; closures internal to it available in the current environment by putting
;;; them in registers if they are referenced in the body (really we should
;;; look to see where they are being used as in RESULT-WANT-LOC).
;;; For jump lambdas, establish the current environment as the one they
;;; run in.  Then generate-code for the body.

(define (really-generate-let&labels node body jumps closures)
  (if closures
      (let ((closure (environment-closure (lambda-env (car closures)))))
        (make-heap-closure node closure)
        (lock AN)
        (walk (lambda (var)
                (let ((reg (get-register 'pointer node '*))
                      (offset (cdr (assq var (closure-env closure)))))
                  (generate-move-address node (reg-offset AN offset) reg)
                  (mark var reg)))
              (filter (lambda (closure)
                        (memq? closure (lambda-live body)))
                      (cdr (closure-members closure))))
        (if (memq? (car (closure-members closure)) (lambda-live body))
            (mark (car (closure-members closure)) AN))))
  (allocate-call (lambda-body body)))


(define (get-or-set-join-state node lamb)
  (let ((join (lambda-env lamb)))
    (if (eq? (join-point-arg-specs join) 'not-yet-determined)
        (set-join-state node join lamb))
    join))

;;; SET-JOIN-STATE The first jump (compile time) is about to be made to this
;;; point.  We must set up places for the free variables to go.  For now,
;;; put one in a register and the rest in temporaries. Move them there.

(define (set-join-state node join lamb)
  (lambda-queue lamb)
  (let ((args (length (cdr (lambda-variables lamb))))
        (env (join-point-env join)))
    (if env
        (push (join-point-global-registers join)
              (cons (fx+ A1 args) (car env))))
    (walk (lambda (var)
            (cond ((temp-loc var)
                   => (lambda (temp)
                        (push (join-point-global-registers join)
                              (cons temp var))))
                  (else
                   (let* ((access (access-value node var))
                          (temp (get-temp var 'pointer)))
                       (set (temp-loc var) temp)
                       (cond ((register-loc var)
                              => (lambda (reg)
                                   (set (reg-node reg) nil)
                                   (set (register-loc var) nil))))
                       (push (join-point-global-registers join)
                             (cons temp var))
                       (generate-move node access temp)))))
          (cdr env))
    (cond ((join-point-contour join)
           => (lambda (contour)
                (push (join-point-global-registers join)
                      (cons P contour)))))
    (set (join-point-arg-specs join) (reg-positions args nil))))



(define (first-n l n)
  (do ((i 0 (fx+ i 1))
       (l l (cdr l))
       (result '() (cons (car l) result)))
      ((fx>= i n) (reverse! result))))

(define (at-top-level? node)
  (eq? (environment-closure (lambda-env node)) *unit*))


    
;;; Call-Xenoid
;;; ---------------------------------------------------------------------

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
         (lock AN)
         (generate-move node AN
                     (access-locative node location))
         (unlock AN))
        (else
         (let ((access (access-value node value)))
           (protect-access access)
           (generate-move node access (access-locative node location))
           (release-access access)))))

;;; Fields within structured types

(define (generate-%set-accessor node)
  (let ((offset (leaf-via (call-proc node)))
        (value ((call-arg 3) node))
        (loc (leaf-value ((call-arg 2) node))))
    (cond ((lambda-node? value)
           (make-closure node value)
           (lock AN)
           (let ((reg (->register 'pointer node loc '*)))
             (generate-move node AN
                   (reg-offset reg offset))
             (unlock AN)))
          (else
           (let ((access (access-value node (leaf-value value))))
             (protect-access access)
             (let ((loc-reg (->register 'pointer node loc '*)))
               (generate-move node access (reg-offset loc-reg offset))
               (release-access access)))))))

(define (generate-accessor node offset)
  (destructure (((cont arg) (call-args node)))
    (let* ((rwl (result-want-loc cont))
           (var (leaf-value arg))
           (dest (cond ((register? rwl) rwl)
                        (else
                         (get-register rwl node '*)))))
      (cond ((dying? var node)
             (->register 'pointer node var dest)
             (kill var)
             (generate-move node (reg-offset dest offset) dest))
            (else
             (lock dest)
             (let ((reg (->register 'pointer node var '*)))
               (unlock dest)
               (free-register node dest)
               (generate-move node (reg-offset reg offset) dest))))
      (mark-continuation node dest))))


(define (access-locative node ref)
  (access-value node (leaf-value ref)))
                    

;;; Comparisons
;;; ---------------------------------------------------------------------

(define (primitive-comparator node inst)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let ((access (access-value node (leaf-value ref2))))
      (protect-access access)
      (emit vax/cmpl (access-value node (leaf-value ref1)) access)
      (emit-jump inst else then)
      (release-access access))))


;;; Arithmetic
;;; ---------------------------------------------------------------------

(define (mark-continuation node reg)
  (let ((cont (car (call-args node))))
    (if (lambda-node? cont)
        (if (not (n-ary? cont))
            (mark (car (lambda-variables cont)) reg))
        (generate-move node reg A1))))

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
             (let ((access (access-value node var)))
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
             (let ((access (access-value node rvar)))
               (unlock dest)
               (kill lvar)
               (values access dest dest)))
            ((and (eq? (register-loc rvar) dest) (dying? rvar node))
             (lock dest)
             (let ((access (access-value node lvar)))
               (unlock dest)
               (kill rvar)
               (values access dest dest)))
            (else
             (let ((l-acc (access-value node lvar)))
               (protect-access l-acc)
               (let ((r-acc (access-value node rvar)))
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
             (let ((access (access-value node lvar)))
               (unlock dest)
               (kill rvar)
               (values access dest dest)))
            ((and (eq? (register-loc lvar) dest) (dying? lvar node))
             (lock dest)
             (let ((access (access-value node rvar)))
               (unlock dest)
               (kill lvar)
               (values dest access dest)))
            (else
             (let ((l-acc (access-value node lvar)))
               (protect-access l-acc)
               (let ((r-acc (access-value node rvar)))
                 (release-access l-acc)
                 (free-register node dest)
                 (values l-acc r-acc dest))))))))


(define (generate-nonstandard-call node arg-specs used return)
  (destructure (((primop cont . args) (call-proc+args node)))
    (walk (lambda (reg)
            (free-register node reg))
          used)
    (parallel-assign node args arg-specs nil `(,(cons AN-1 (primop-value primop))))
    (generate-jump-to-subroutine node)
    (free-register node return)
    (mark-continuation node return)))


(define (generate-call-with-continuation node)
  (destructure (((cont proc) (call-args node)))
    (free-register node AN)
    (generate-move node
                   (access-name 'global/escape-procedure-template)
                   AN)
    (generate-extend node ESCAPE-PROCEDURE-SIZE)
    (fetch-continuation-from-stack node (leaf-value cont))
    (generate-move node SP (reg-offset A5 escape-closure/cont))
    (generate-move node (access-name 'global/dynamic-state)
                        (reg-offset A5 escape-closure/state))
    (mark (cadr (lambda-variables proc)) A5)
    (allocate-call (lambda-body proc))))
