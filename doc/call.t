

More calling sequence documentation:

Pseudo-source-code definitions for protocol checking,
n-ary procedures, operation dispatch, and so forth.




  m = the number of argument registers.
  (This is inconsistent with call.doc!)

  Too many arguments in a call:

    (f a1 a2 ... am ... an)
      ==>  (call-n-ary f n a1 a2 ... am-1 (%list am am+1 ... an))

  Too many formals:

    (lambda (a1 a2 ... am ... an) body)
      ==>  (n-ary (lambda (nargs a1 a2 ... am-1 rest)
                    (if (not (= nargs n))
                        ($)
                        (let ((am   (nth rest 0))
                              (am+1 (nth rest 1))
                              ...
                              (an   (nth rest n-m)))
                          body))))

  N-ary setup - too few formals:

    (n-ary (lambda (nargs a1 a2 ... an rest) body))
      ==>
    (n-ary (lambda (nargs a1 a2 ... an ... am-1 rest)
             (let* ((cn   (lambda (rest) body))
                    (cn+1 (lambda (rest) (cn (%cons an+1 rest))))
                    ...
                    (cm-1 (lambda (rest) (cm-2 (%cons am-1 rest)))))
                    (cm   (lambda (rest) (cm-1 (%cons rest rest)))))
               (cond ((< nargs n) ($))
                     ((> nargs m) (cn rest))
                     (else
                      (xcase nargs
                        ((n)   (cn   '()))
                        ((n+1) (cn+1 '()))
                        ...
                        ((m-1) (cm-1 '()))
                        ((m)   (cm   '())) ))))))

  N-ary setup - argument list escapes upwards (or downwards!)

    (n-ary (lambda (nargs a1 a2 ... an)
             (let ((an (copy-list an)))
               ... body ...)))

  APPLY:

  (define apply-internal
    (n-ary (lambda (nargs a1 a2 ... am-2 p am)
             (cond ((> nargs m)
                    ;; A rest-arg will always have length >= 2.
                    (let* ((j (- nargs m+1))
                           (z (%nthcdr am (- j 1)))
                           (l (cadr z)))
                      (set (cdr z) (%copy-list l)))
                    (call-n-ary p (+ j (length l)) a1 a2 ... am-1 am))
                   (else
                    (let* ((cm   (lambda (p a1 a2 ... am l)
                                   (if (null? l)
                                       (p a1 a2 ... am)
                                       (call-n-ary p (+ m (length l))
                                                     a1 a2 ...
                                                     (%cons am (%copy-list l))))))
                           (cm-1 (lambda (p a1 a2 ... am-1 l)
                                   (if (null? l)
                                       (p a1 a2 ... am-1)
                                       (cm p a1 a2 ... am-1 (car l) (cdr l)))))
                           ...
                           (c1   (lambda (p a1 l)
                                   (if (null? l)
                                       (p)
                                       (c1 p a1 (car l) (cdr l)))))
                           (c0   (lambda (p l)
                                   (if (null? l)
                                       (p)
                                       (c1 p (car l) (cdr l))))))
                      (xcase nargs
                             ((0) (c0 (car l) (cdr l)))
                             ((1) (c0 p a1))
                             ((2) (c1 p a1 a2))
                             ...
                             ((m-1) (cm-2 p a1 a2 ... am-1))
                             ((m)   (cm-1 p a1 a2 ... am-1 am)))))))))

  (define apply
    (n-ary (lambda (nargs p a1 a2 ... am-2 rest)
             (call-nary nargs apply-internal a1 a2 ... am-2 p rest))))

  ;; Left shift calling first argument

  (define call
    (n-ary (lambda (nargs+1 p a1 a2 ... am-2 rest)
             (let ((nargs (- nargs+1 1)))
               (xcond ((> nargs m-1)
                       (call-n-ary nargs p a1 a2 ... am-2 (car rest) (cdr rest)))
                      ((= nargs m-1)
                       (p a1 a2 ... am-2 rest))
                      ((< nargs m-1)
                       (call-n-ary nargs p a1 a2 ... am-2)))))))

  (define (call-internal p)
    (n-ary (lambda (nargs a1 a2 ... am-2 am-1 am)
             (let ((p (proclaim (protocol nargs) p)))
               (call-n-ary nargs p a1 a2 ... am-2 am-1 am)))))

  (define (protocol nargs)      ;e.g. ((protocol 2) cons) => t
    (lambda (p)
      (cond ((and (closure? p)
                  (let ((want (closure-nargs p)))
                    (or (= want -1)
                        (= want nargs))))

  ;;; Right shift calling a different procedure

  (define (trampoline p)
    (call-n-ary (+ nargs 1) p
                p a1 a2 ... am-2
                (xcond ((< nargs m) am-1)
                       ((= nargs m) (%list am-1 am))
                       ((> nargs m) (%cons am-1 am)))))

  (operate obj next op a1 a2 ... am-3 am-2)
    ==>
  (call-n-ary nargs|operating (get-handler obj) a1 a2 ... am-3
                                                obj next op am-2)

  (define (get-handler obj)
    (cond ((and (closure? obj) (willing? obj)) obj)
          (else handle-unwilling)))

  (define (make-operation default)
    (Y (lambda (op)
         (n-ary (lambda (nargs a1 a2 ... am-3 am-2 am-1 am)
                  (operate a1 terminus op a1 a2 ... am-3
                           (xcond ((< nargs m-1) am-2)
                                  ((= nargs m-1) (%list am-2 am-1))
                                  ((= nargs m) (%list am-2 am-1 am))
                                  ((> nargs m) (%cons* am-2 am-1 am)))))))))

  (method (a1 a2 ... am) . body)
    ==>
  (n-ary (lambda (nargs) ...))

  (Y (lambda (obj)
       (obtain-env-from 1 obj (lambda (obj) x))))  =>  foo

  (foo foo) => x

  (%object proc
           (lambda (op) (eq? op op0)))

    (n-ary (lambda (nargs a1 a2 ... am-3 obj next op am-2)
             (cond ((not (funny-nargs? nargs))
                    (call-n-ary proc nargs
                       a1 a2 ... am-3 obj next op am-2))
                   ((matches? op)
                    => (lambda (op)
                         ((call-internal meth) nargs&~funny
                             a1 a2 ... next ...)))
                   (else
                    (call-n-ary next nargs
                       a1 a2 ... am-3 obj terminus op am-2)))))

    (define-operation (op-table obj) nil)

    (define (joiner obj-1 obj-2)
      (lambda (obj-1 obj-2)
        (let ((table-1 (op-table obj-1))
              (table-2 (op-table obj-2)))
          (cond ((and table-1 table-2)
                 (let ((table (merge-tables table-1 table-2)))
                   (lambda (obj-1 obj-2)
                     (%object obj-1 table))))
                (else join)))))

    (define (join obj-1 obj-2)
      (cond ((joined? obj-1)
             (join-1 (left obj-1)
                     (join (right obj-1) obj-2)))
            (else
             (join-1 obj-1 obj-2)))))


;;; Must flavors form a DAG?  What does it mean for there to be a loop?

(define (flavor instantiator . combiners)
  (let ((own-methods '())
        (dependents '())  ; list of flavors in whose combiners list we appear
        (hash-table nil)
        (valid? nil)
        (all-combiners (apply union (map get-combiners combiners))))
    (let ((self
    (object (lambda (op)
              (cond ((not valid?)
                     (set hash-table (make-hash-table))
                     (walk (lambda (c)
                             (propagate-methods c table n))
                           combiners)
                     (propagate-methods self table 0)))
              (cond ((table-entry hash-table op)
                     => (lambda (z) (values (cadr z) (cddr z))))
                    (else nil)))
            ((set-method self op method)
             (push own-methods
                   (cons (cons op method)))
             (invalidate self))
            ;; delete-method
            ((merge-method self op method n)
             (set (hash-table-entry self op)
                  (cons (cond ((hash-table-entry self op)
                               => (lambda (z)
                                    (method-join method (cadr z) (cddr z))))
                              (else method))
                        n)))
            ((invalidate self)
             (cond (valid?
                    (set-invalid self)
                    (walk set-invalid dependents))))
            ((set-invalid self) (set valid? nil))
            ((set-dependent self table)
             (push dependents table))
            ((get-combiners self)
             (cons self all-combiners))
            ((propagate-methods self table n)
             (walk (lambda (z)
                     (merge-method table (car z) (cdr z) n))
                   own-methods
                   (gen-integers 1)))
            ((instantiate-component self) (instantiator))
            ((instantiate self)
             (let ((components (cons (instantiator)
                                     (map instantiate-component dependents))))
               (*object nil
                        (lambda (op)
                          (receive (method n) (flavor op)
                            (values method
                                    (nth components n))))))))))
    (walk (lambda (c) (set-dependent c self)) all-combiners))))

(define (method-join m1 m2 n)   ; n is m2's position in instance
  (lambda (obj ...)
    ... invoke m1 with next component = (nth self n2) ...))

(method (op component punt self args ...)
  (receive (a b c)
           (divulge-f0-variables component)
    ... body ...))




(flavor (lambda (init-info)
          (let ((a ...)
                (b ...))
            (object nil
              ((divulge-f0-variables self) (values a b c))
              ((op1 self ...) ... a ...))))
        f1 f2 f3 ... fn)
                        
(define make-foo
   (let ((table
         `((,op1 1)
           (,op2 2))))
      (object
        (lambda (a b c)
          ... init...
            (*object nil
              (lambda (op)
                (let ((m1 (lambda (self ...) ...))
                      (m2 (lambda (self ...) ...)))
                      ...)
                (select op
                  ((invoke-method n . args)
                   (apply (xcase n
                            ((1) m1)
                            ((2) m2)
                            ...)
                          args))
                  ((origin self) make-foo)
                  (else (invoke-method self (cadr (assq op table))))))))
        ((dispatch-table self) table))))
                     
(define foo (make-foo a b c))

(op1 foo ...)
  ==
(invoke-method foo (cadr (assq op1 (dispatch-table make-foo))) ...)


(define (make-foo-bar a b c d e)
  (join (make-foo a b c) (make-bar d e)))

(define make-foo-bar
  (combine make-foo make-bar))

(define foo-bar (make-foo-bar a b c d e))

(op1 foo-bar ...)
