(herald syntax)

;;; Macros for compiler.

;;; (values a b c)
;;;     Returns a, b, and c as its three values.
;;;
;;; (receive (x y z) form . body)           Special form
;;;     Binds variables x, y, z to the values returned by form, and
;;;     evaluates body in that scope.
;;;
;;; These will be primitive in T version 3.  In version 2, they are simulated
;;; by the following awful macros.

(define *value-regs* '(**a1** **a2** **a3** **a4** **a5**))

(define-syntax (receive vars call . body)
  (let ((nvars (length vars)))
    (if (fx> nvars (length *value-regs*))
        (syntax-error "too many variables~%  ~S"
                      `(receive ,vars ,call . ,body)))
    `(let ((**nvals** (car ,call)))
        (if (fxn= **nvals** ,nvars)
            (losing-receive **nvals** ,nvars))
        (let ,(map list vars *value-regs*)
          ,@body))))

(define-syntax (values . vals)
  (let ((nvals (length vals))
        (temps (map (lambda (val) (ignore val) (generate-symbol 'values))
                    vals)))
    (if (fx> nvals (length *value-regs*))
        (syntax-error "too many values~%  ~S"
                      `(values . ,vals)))
    `(let ,(map list temps vals)
       ,@(map (lambda (reg temp) `(set ,reg ,temp))
              *value-regs*
              temps)
       '(,nvals . **multiple-values**))))

;;; The following have reduced utility now that there are only four node
;;; types.  Perhaps they should be phased out.

;;; (define-generalist name dispatch
;;;   (lambda (node arg) ((dispatch node) node arg)))
;;;
;;; NAME gets bound to the value of the LAMBDA, but the LAMBDA is
;;; closed in an environment in which DISPATCH is bound to a procedure
;;; which will select an appropriate specialist procedure.

(define-syntax (define-generalist name dispatch proc)
  `(define ,name
     (let ((,dispatch (make-dispatcher ',name)))
       (object ,proc
               ((get-dispatcher self) ,dispatch)
               ((identification self) ',name)))))

;;; (define-specialist ((phase type) . args) . body)

(define-syntax (define-specialist pat . body)
  (let ((spec (car pat)))
    `(set-node-dispatch ,(car spec)
                         ',(cadr spec)
                         (object (lambda ,(cdr pat)
                                   (ignorable ,@(cdr pat))      ; hack
                                   . ,body)
                                 ((identification self) ',spec)))))

;;; Kludges

(*define *standard-env* '*orbit-syntax-table*
  (env-syntax-table (the-environment)))

(define-syntax primop nil)

(define-syntax (emit op . args)
  `(icreate ',op ,@args))
