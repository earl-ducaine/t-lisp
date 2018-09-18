(herald (tsys syntax t 121)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Syntax tables & syntax descriptors

(define-integrable syntax-table-entry
  (object (lambda (table sym) (table sym))
          ((setter self) set-syntax-table-entry)))

(define-operation (set-syntax-table-entry table sym descr))

(define (make-syntax-table super . maybe-id)
  (let* ((id (car maybe-id))		; (car '()) => #f
	 (table (make-table id))
	 (env nil)
	 (atomex nil))
    (object (lambda (sym)
              (let ((probe (table-entry table sym)))
                (cond (probe (if (eq? probe '*filtered*) nil probe))
                      (super (super sym))
                      (else nil))))
            ((set-syntax-table-entry self sym descr)
             (cond ((not *print-env-warnings?*))
                   ((table-entry table sym)
                    (env-warn "Redefining syntax" sym))
                   ((and super (super sym))
                    (env-warn "Shadowing syntax" sym)))
             (set (table-entry table sym)
                  (if descr
                      (check-arg syntax-descriptor?
                                 descr
                                 set-syntax-table-entry)
                      '*filtered*)))
            ((env-for-syntax-definition self)
             (cond (env)
                   (super (env-for-syntax-definition super))
                   (else (make-locale *standard-env*
				      'env-for-syntax-definition))))
            (((setter env-for-syntax-definition) self new-env)
             (set env (check-arg environment?
				 new-env
				 env-for-syntax-definition)))
	    ((atom-expander self)
	     (cond (atomex)
		   (super (atom-expander super))
		   (else default-atom-expander)))
	    (((setter atom-expander) self new-atomex)
	     (set atomex (check-arg procedure? new-atomex atom-expander)))
            ((identification self) id)
	    ((set-identification self val)
	     (if (not id) (set id val)))
            ((print-type-string self) "Syntax-table"))))

(define-settable-operation (env-for-syntax-definition table))
(define-settable-operation (atom-expander table))

(define (default-atom-expander atom)
  (cond ((symbol? atom)
	 `(,(t-syntax 'variable-value) atom))	;randomness
	((self-evaluating? atom)
	 `(,(t-syntax 'quote) ,atom))		;more randomness
	((null? atom)
	 (format (error-output)
		 "~&** Warning: ~S in evaluated position~%" atom)
	 `(,(t-syntax 'quote) ,atom))
	(else
	 (syntax-error "unevaluable datum - ~S" atom))))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)))

(define (make-empty-syntax-table id)
  (make-syntax-table nil id))

;;; Used only by EVAL and ALPHATIZE!  Creates primitive syntax
;;; descriptors on demand.  Eventually, this must be phased out; the
;;; list of primitive syntax descriptors should be standardized and
;;; their creation should be localized to this file.

(define (obtain-syntax-table-entry table symbol spect)
  (or (syntax-table-entry table symbol)
      (set (syntax-table-entry table symbol)
           (make-syntax-descriptor symbol spect))))

;;; Descriptor creation and manipulation.

(define (make-syntax-descriptor id spect)
  (object nil
	  ((syntax-descriptor? self) t)
	  ((identification self) id)
	  ((print self stream)
	   (print-syntax-descriptor self stream))
	  ((syntax-argspectrum self) spect)))

(define (print-syntax-descriptor self stream)
  (let ((id (identification self)))
    (cond ((eq? self (syntax-table-entry *standard-syntax-table* id))
           (format stream "#[Syntax~_~S]" id))
          ;; Temporary kludge - no JOIN.
          ((or (eq? self *backquote*)
               (eq? self *comma*)
               (eq? self *comma-atsign*))
           (format stream "#[Internal-syntax~_~S]" id))
          (else
           (format stream "#{Syntax~_~S~_~S}"
                   (object-hash self) id)))))

(define-operation (syntax-descriptor? obj) (macro-expander? obj))
(define-predicate macro-expander?)
(define-operation (expand-macro-form desc exp table))
(define-operation (syntax-argspectrum obj) '(0 . t))        ; foo

;;; Called from expansion of MACRO-EXPANDER.
;;; EXPANDER is a procedure of one argument.
;;; Someday allow for tracing macro expansions?

(define (make-macro-descriptor expander spect id)
  (object nil
	  ((expand-macro-form self exp table)
	   (ignore table)
	   (expander exp))
	  ((macro-expander? self) t)
	  ((syntax-argspectrum self) spect)
	  ((print self stream)
	   (print-syntax-descriptor self stream))
	  ((identification self) id)
	  ((disclose self)
	   (disclose-macro-expander expander))
	  ((get-loaded-file self)
	   (get-loaded-file expander))))

;;; Sample nonstandard macro expander:
;;;   (OBJECT NIL
;;;           ((EXPAND-MACRO-FORM SELF EXP TABLE)
;;;            ... use TABLE ...)
;;;           ((MACRO-EXPANDER? SELF) T))

;;; Basic syntax check for special forms.

(define (check-special-form-syntax desc exp)
  (cond ((compatible-with-argspectrum? (cdr exp) (syntax-argspectrum desc))
         exp)
        (else (check-special-form-syntax desc
                                         (syntax-error
                                          "bad syntax for special form~%  ~S"
                                          exp)))))

;;; ---------- end of important stuff.

;;; Random debugging thing called by CRAWL.

(define (macro-expand form table)
  (cond ((atom? form) form)
        ((symbol? (car form))
         (let ((probe (syntax-table-entry table (car form))))
           (if (and probe (macro-expander? probe))
               (expand-macro-form probe form table)
               form)))
        ((macro-expander? (car form))
         (expand-macro-form (car form) form table))
        (else form)))

;;; Incredibly kludgey procedure.  Assumes that the form of DISCLOSE of the
;;; expansion procedure is of the form
;;;  (NAMED-LAMBDA ,SYMBOL (,Z)
;;;    (DESTRUCTURE (((() . ,ARGS) ,Z))
;;;      . ,REST))

(define (disclose-macro-expander proc)
  (let ((f (lambda (name body)
             (cond ((and (pair? body) (eq? (car body) (t-syntax 'destructure)))
                    (destructure ((   (#f (((#f . args) #f)) . body)   body))
                      `(macro-expander (,name . ,args) . ,body)))
                   (else nil)))))
    (cond ((disclose proc)
            => (lambda (lexp)
                 (cond ((pair? lexp)
                        (case (car lexp)
                          ((named-lambda) (f (cadr lexp) (cadddr lexp)))
                          ((lambda)       (f nil         (caddr lexp)))
                          (else nil)))
                       (else nil))))
          (else nil))))

(define (t-syntax name)		;Handy abbreviation
  (syntax-table-entry *standard-syntax-table* name))
