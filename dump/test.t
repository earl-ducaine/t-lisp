(define (d file exp . rest)
  (destructure (((keys accs) rest))
    (let ((filename (filename-with-type (->filename file) 'dmp)))
      (with-open-streams ((o (if rest
                                 (open-dump filename keys accs)
                                 (open-dump filename))))
        (write o exp)))))

(define (r file . rest)
  (destructure (((makers accs) rest))
    (let ((filename (filename-with-type (->filename file) 'dmp)))
      (with-open-streams ((o (if rest
                                 (open-retrieve filename makers accs)
                                 (open-retrieve filename))))
        (read o)))))

(define-structure-type frog
  id            ; Unique numeric identifier
  binder        ; LAMBDA node which binds this variable
  number        ; K: var = (nth (lambda-variables (variable-binder var)) K)
  )

(define-methods handle-frog
  ((print self stream)
   (format stream "#{Frog~_~S~_~S~_~S}"
           (frog-id self)
           (frog-binder self)
           (frog-number self))))

(define (create-frog i b n)
  (let ((frog (make-frog)))
    (set (frog-id frog)     i)
    (set (frog-binder frog) b)
    (set (frog-number frog) n)
    frog))

(define (keys obj)
  (cond ((frog? obj)
         `frog-record)
        (else
         nil)))

(define (accessors key)
  (cond ((eq? key 'frog-record)
         (stype-selectors frog-stype))
        (else
         nil)))

(define (makers key)
  (cond ((eq? key 'frog-record)
         (stype-constructor frog-stype))
        (else
         nil)))
