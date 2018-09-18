(herald assign
    (syntax-table *orbit-syntax-table*))

;;; Deal with implicit cell dereferencing.
;;;============================================================================
;;;  Local variables that are set are changed to locatives.

(define (assignment-analyze node)
  (cond ((lambda-node? node)
         (let ((assigned-vars (filter variable-assigned?
                                      (lambda-rest+variables node))))
           (if (not (null? assigned-vars))
               (introduce-cells node assigned-vars))))
        ((call-node? node)
         (let ((proc (call-proc node)))
           (cond ((primop-ref? proc primop/let-reference)
                  (replace proc (create-primop-node primop/call))
                  (let ((vars (lambda-variables (car (call-args node)))))
                    (hack-references vars vars (map false vars))))))))
  (walk assignment-analyze (node-children node)))

;;; Returns true if there are any SET's or LOCATIVE's to var.

(define (variable-assigned? var)
  (and (used? var)
       (any? (lambda (ref)
               (and (eq? (node-role ref) (call-arg 2))
                    (let ((proc (call-proc (node-parent ref))))
                      (and (primop-node? proc)
                           (or (eq? (primop-value proc) primop/*set-var)
                               (eq? (primop-value proc) primop/*locative))))))
             (variable-refs var))))

;;; (lambda (x) ... x ... (set x ...) ...)
;;;   ==>  (lambda (x)
;;;          (let ((x' (make-cell x)))
;;;             ... (contents x') ... (set-contents x' ...) ...))

(define (introduce-cells node vars)          
  (let ((new-vars (map (lambda (var)
                         (create-variable (variable-name var)))
                       vars))
        (args (map (lambda (var)
                     (let ((mnode (create-call-node 3 1)))
                       (relate call-proc mnode 
                               (create-primop-node primop/make-locative))
                       (relate (call-arg 1) mnode
                               (create-primop-node primop/continuation))
                       (relate (call-arg 2) mnode
                               (create-reference-node var))
                        mnode))
                    vars)))  
    (move (lambda-body node)
          (lambda (body)
            (let ((lnode (create-lambda-node
                           'p
                           `(,nil ,(create-variable 'k) ,@new-vars)))
                  (cnode (create-call-node (fx+ (length vars) 2) 1)))
              (relate call-proc cnode lnode)
              (relate-call-args cnode
                 (cons (create-primop-node primop/continuation) args))
              (relate lambda-body lnode body)
              cnode)))
    (hack-references vars new-vars args)))
    

;;; Replace references to variables with indirections through locatives.
;;;   x              ==>  (contents x')
;;;   (set x y)      ==>  (set-contents x' y)
;;;   (locative x)   ==>  x'

(define (hack-references vars new-vars locs) 
  (walk (lambda (var new-var loc)
          (walk (lambda (ref)           
                  (cond ((neq? (node-role ref) (call-arg 2))
                         (replace ref (dereferencer new-var)))
                        (else
                         (hack-reference ref new-var loc))))
                (variable-refs var)))
        vars
        new-vars
        locs))  


(define (hack-reference ref new-var loc)
  (let* ((parent (node-parent ref))
         (proc (call-proc parent)))
    (cond ((not (primop-node? proc))
           (replace ref (dereferencer new-var)))
          ((eq? (primop-value proc) primop/*set-var)
           (assigner (create-reference-node new-var) parent))
          ((eq? (primop-value proc) primop/*locative)
           (erase-all parent)
           (replace ref (create-reference-node new-var)))
          ((eq? parent loc)  ;; this was introduced
           nil)
          (else
           (replace ref (dereferencer new-var))))))

;;;  x ==> (value x')

(define (dereferencer new-ref)
  (let ((node (create-call-node 3 1)))
    (relate call-proc node
            (create-primop-node primop/contents))
    (relate (call-arg 1) node (create-primop-node primop/continuation))
    (relate (call-arg 2) node new-ref)
    node))

;;;  (set x y) ==> (set-contents x' y)

(define (assigner new-ref parent)
  (set (primop-value (call-proc parent))
       primop/set-contents)
  (replace ((call-arg 2) parent) new-ref))
