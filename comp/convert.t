(herald convert
    (syntax-table *orbit-syntax-table*))
;;; ---------------------------------------------------------------------
;;; CPS conversion

;;; These routines are all destructive; none return an interesting value.

(define (convert node)
  (cond ((lambda-node? node)
         (convert-lambda node (lambda-cont-var node)))))

;;; (lambda () x)  ==>  (clambda (c) (c x))

(define (convert-lambda node cvar)
  (let ((body (lambda-body node)))
    (cond ((call-node? body)
           (convert-call body cvar))
          (else
           (convert body)
           (move body
                 (lambda (body)
                   (let ((new-body (create-call-node 2 0)))
                     (relate call-proc new-body
                             (create-reference-node cvar))
                     (relate (call-arg 1) new-body body)
                     new-body)))))))

;;; Eliminate composition by creating a continuation for each argument
;;; which is a call.
;;; Also, propagate our continuation into the call's exit arguments.
;;; "Cont" is either a variable structure or a LAMBDA-node.  Kinda kludgey.

;;; YUCK.  What a mess.

(define (convert-call node cont)
  (cond ((and (not (variable? cont))
              (fx> (call-exits node) 1))
         (let* ((jvar (create-variable 'j))         ;join point
                (lnode (create-lambda-node 'p (list nil jvar)))
                (cnode (create-call-node 2 1)))
           (relate call-proc cnode lnode)       ;((lambda (j) node) cont)
           (relate (call-arg 1) cnode cont)     ;what J gets bound to
           (move node
                 (lambda (node)
                   (relate lambda-body lnode node)
                   cnode))
           (really-convert-call node jvar)))
        (else
         (really-convert-call node cont))))

;;; (f *cont* (g *cont* ...) ...) => (g (lambda (val) (f *cont* val ...)) ...)

(define (really-convert-call node cont)
  (walk (lambda (arg)
          (cond ((call-node? arg)
                 (let* ((var (create-variable 'v))
                        (cont (create-lambda-node 'c (list nil var))))
                   (move arg   ;Replace the sub-call with result var ref.
                         (lambda (old)
                           (ignore old)
                           (create-reference-node var)))
                   (move node  ;Invert child and parent in tree.
                         (lambda (node)
                            (relate lambda-body cont node)
                            arg))
                   (convert-call arg cont)))
                ((primop-ref? arg primop/continuation)
                 (replace arg (if (variable? cont)
                                  (create-reference-node cont)
                                  cont)))
                ((lambda-node? arg)
                 (let ((n (call-arg-number (node-role arg))))
                   (cond ((and (fx> n 0)
                               (fx<= n (call-exits node)))
                          (convert-lambda arg cont))  ;propagate cont into body
                         (else
                          (convert arg)))))))
          (sort (call-proc+args node)
                (lambda (node1 node2)
                  (fx< (node-rank node1)
                       (node-rank node2)))))
  (cond ((and (not (variable? cont))
              (empty? (node-parent cont)))
         (erase-all cont))))

;;; Evaluate the most complex arguments first.

(define (node-rank node)
  (cond ((call-node? node)
         (if *left-to-right?*
             (call-arg-number (node-role node)))
             (fx- 0 (call-complexity node)))
        (else 0)))

(lset *left-to-right?* nil)

;;; Complexity analysis.

(define (complexity-analyze node)
  (cond ((reference-node? node) 1)
        (else
         (do ((q 0 (fx+ q (complexity-analyze (car l))))
              (l (node-children node) (cdr l)))
             ((null? l)
              (if (call-node? node)
                  (set (call-complexity node) q))
              q)))))
