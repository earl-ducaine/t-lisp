(herald (tsys object t 88)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; OBJECT and related macros

(define-syntax (%method pat . body)
  (destructure (((op . args) pat))
    (cond ((symbol? op)
	   `(,(t-syntax 'named-lambda) ,op (#f #f #f . ,args)
	      (,(t-syntax 'ignorable) ,(car args))
	      . ,body))
	  (else
	   `(,(t-syntax 'lambda) (#f #f #f . ,args)
	      (,(t-syntax 'ignorable) ,(car args))
	      . ,body)))))

(define-syntax (%handler var . clauses)
  (cond ((pair? var)
	 ;; Obsolete syntax.
	 `(,(t-syntax '%handler) #f ,var . ,clauses))
	(else
	 (let ((obj   (generate-symbol 'object))
	       (state (generate-symbol 'state)))
	   `(,(t-syntax 'lambda) (,obj ,state)
	      (,(t-syntax 'select) (%state-op ,state)
		      ,@(map (lambda (clause)
			       (construct-method-clause clause var obj state))
			     clauses)
		      ,@(if (not (assq '=> clauses))
			    `((else (%dispatch-next ,obj ,state)))
			    '())))))))

;;; Auxiliary for above

(define (construct-method-clause clause var obj state)
  (let ((lose (lambda ()
		(syntax-error "bad method clause syntax~%  ~S"
			      clause)
		'(() nil))))
    (cond ((not (pair? clause)) (lose))
	  ((eq? (car clause) '=>)
	   `(else (,(cadr clause) ,obj ,state)))
	  ((pair? (car clause))
	   (destructure ((((op . args) . body) clause))
	     `((,op) (,(t-syntax 'lambda)
		      (,var #f #f . ,args)
		      (,(t-syntax 'ignorable) ,(car args))
		      . ,body))))
	  (else (lose)))))

(define-syntax (object proc . clauses)
  `(%object ,proc (,(t-syntax '%handler) #f . ,clauses)))

(define-syntax (operation default . clauses)
  `(%operation (%massage-default ,default)
	       '(1 . t)
	       nil
	       (,(t-syntax '%handler) #f
			 ,@clauses
			 ,@(if (assq '=> clauses)
			       '()
			       '((=> handle-operation))))))

(define-syntax (define-operation pat . body)
  (let ((pat (check-arg pair? pat 'define-operation)))  ; ugh
    `(define ,(car pat)
       (%operation ,(cond ((atom? body) 'nil)
			  (else `(%method ,pat . ,body)))
		   ',((*value *t-implementation-env*
			      'arglist->argspectrum)
		      (cdr pat))
		   ',(car pat)
		   handle-operation))))

(define-syntax (define-settable-operation pat . body)
  (let ((pat (check-arg pair? pat 'define-settable-operation))) ; ugh
    `(define ,(car pat)
       (%settable-operation ,(cond ((atom? body) 'nil)
				   (else
				    `(%method ,pat . ,body)))
			    ',((*value *t-implementation-env*
				       'arglist->argspectrum)
			       (cdr pat))
			    ',(car pat)))))

(define-syntax (define-predicate id . rest)
; (ignore rest)  bug
  `(define ,id (%predicate ',id)))  ; Hair this up later

;;; E.g. (DEFINE-METHODS HANDLE-BITV
;;;        ((PRINT BITV STREAM) ...))
;;; This is implemented WRONG at the moment.  What to do.  It'll do for now.

(define-syntax (define-methods handlr var . clauses)
  (cond ((pair? var)
	 ;; Obsolete syntax.
	 `(,(t-syntax 'define-methods) ,handlr #f ,var . ,clauses))
	(else
	 (let ((g (generate-symbol 'handler)))
	   `(let ((,g ,handlr))
	      ,@(map (lambda (clause)
		       (let ((pat (car clause)))
		         (cond ((pair? pat)
			        `(*define-method ,g
						 ,(car pat)
						 (lambda (,var #f #f
							       ,@(cdr pat))
						   ,@(cdr clause))))
			       ((eq? pat '=>)
				`(set-default-handler ,g ,(cadr clause)))
			       (else
				(syntax-error "bad clause~%  (~S ...~_~S~_...)"
					      'define-methods
					      clause)))))
		       clauses))))))

