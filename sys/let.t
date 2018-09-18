(herald (tsys let t 30)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Binding macros

;;; Macros for doing bindings of all sorts.

(define (valid-spec? spec)
  (and (pair? spec)
       (list? (cdr spec))
       (null? (cddr spec))))  ;(cdr '()) => ()

(define-syntax (let specs . body)
  (cond ((every? valid-spec? specs)
         `((,(t-syntax 'lambda) ,(map car specs) ,@body)
           ,@(map (lambda (x)
                    (cond ((atom? (cdr x)) '**let-missing-initializer**)
                          (else (cadr x))))
                  specs)))
        (else
         (syntax-error "illegal spec~%  ~S" `(let ,specs . ,body)))))

(define **let-missing-initializer** '**let-missing-initializer**)

(define-syntax (bind specs . body)
  (cond ((every? valid-spec? specs)
         (let ((temps  (map (lambda (binding)
                              (ignore binding)
                              (generate-symbol 'bind))
                            specs))
               (places (map car  specs))
               (vals   (map cadr specs))
               (handler (generate-symbol 'wind))
               (cell    (generate-symbol 'cell)))
           `((,(t-syntax 'lambda)
              ,temps
              ((,(t-syntax 'lambda)
                (,handler)
                (bind-handler ,handler
                              (,(t-syntax 'lambda) () . ,body)
                              ,handler))
               (,(t-syntax 'lambda)
                ()
                ,@(map (lambda (place temp)
                         `(,(t-syntax 'let) ((,cell ,place))
                                            (,(t-syntax 'set) ,place ,temp)
                                            (,(t-syntax 'set) ,temp ,cell)))
                       places temps))))
             ,@vals)))
        (else
         (syntax-error "illegal spec~%  ~S" `(bind ,specs ,@body)))))

(define-syntax (destructure specs . body)
  (expand-destructure specs body))

;;; Note that EXPAND-DESTRUCTURE is called from other places.
;;; Difficult to write this without side-effects.  Try it sometime.

(define (expand-destructure specs body)
  (let ((a '()) (b '()))
    (walk (lambda (spec)
            (let ((foo (lambda (vars z val)
                         (cond ((null? vars))
                               ((atom? vars)
                                (push a `(,vars (,z ,val))))
                               (else
                                (let ((temp (generate-symbol z)))
                                  (push a `(,temp (,z ,val)))
                                  (push b `(,vars ,temp))))))))  
              (let ((vars (car spec)) (val (cadr spec)))
                (cond ((atom? vars)
                       ;; No destructuring called for; just do as for LET.
                       (push a spec))
                      ((pair? val)
                       ;; RHS is a call or special form; need to stow value.
                       (let ((temp (generate-symbol 'temp)))
                         (push a `(,temp ,val))
                         (push b `(,vars ,temp))))
                      (else
                       ;; RHS is a variable, LHS is pattern; take apart value.
                       (foo (car vars) 'car val)
                       (foo (cdr vars) 'cdr val))))))
          specs)
    `(,(t-syntax 'let) ,(reverse! a)
       ,(cond ((null? b) (blockify body))
              (else (expand-destructure (reverse! b) body))))))

;(define-syntax (let-destructured . rest)
;  `(,(t-syntax 'destructure) . ,rest))               ; change later

;;; What about BIND-DESTRUCTURED, LET*-DESTRUCTURED, and BIND*-DESTRUCTURED?

(define-syntax (let* specs . rest)
  (expand-star-macro specs rest (t-syntax 'let)))

(define-syntax (destructure* specs . rest)
  (expand-star-macro specs rest (t-syntax 'destructure)))

(define-syntax (bind* specs . rest)
  (expand-star-macro specs rest (t-syntax 'bind)))

(define (expand-star-macro specs rest mac)
  (cond ((null? (cdr specs))
         `(,mac ,specs . ,rest))
        (else `(,mac (,(car specs))
                     ,(expand-star-macro (cdr specs) rest mac)))))

;;; Bogus version of RECEIVE for T 2.

(define-syntax (receive vars form . body)
  (cond ((not (proper-list? vars))
         `(receive-values (lambda ,vars ,@body) (lambda () ,form)))
        (else
         (let ((nvars (length vars))
               (temp (generate-symbol 'receive)))
           (cond ((fx> nvars *values-vector-length*)
                  (syntax-error "too many variables~%  ~S"
                                `(receive ,vars ,form . ,body)))
                 ((fx= nvars 1)
                  `(let ((,(car vars) ,form))
                     ,@body))
                 (else
                  `(let ((,temp ,form))
                     (if (fxn= (if (pair? ,temp) (car ,temp) 1) ,nvars)
                         (losing-receive ,temp ,nvars))
                     (let ,(do ((v vars (cdr v))
                                (i 0 (fx+ i 1))
                                (s '()
                                   (cons `(,(car v) (vref *values-vector* ,i))
                                         s)))
                               ((null? v) (reverse! s)))
                       ,@body))))))))
