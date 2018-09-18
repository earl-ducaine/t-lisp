(herald foreign
        (support (make-empty-support-env 'nil) vaxbase))
  
;;; The syntax to define a foreign procedure is
;;; (define-foreign (name (IN     rep0 name0)
;;;                       (IN/OUT rep1 name1)
;;;                       (OUT    rep2 name2)
;;;                       ...)
;;;   return-rep)


(define-local-syntax (define-foreign name-params value)
  `(define-wired ,(car name-params)
     (foreign ',(car name-params) ',(reverse! (cdr name-params)) ',value)))

;;; primop for creating calls to foreign code


(define-wired (foreign name params value)
  (primop foreign (name params value)
    ((primop.foreign self)
     (car (primop.arglist self)))
    ((primop.arg-specs self) '())
    ((primop.generate self node)
     (generate-foreign-call node
                            self
                            (cadr (primop.arglist self))
                            (caddr (primop.arglist self))))))




(define-foreign (readc (in rep/raw-pointer))
  rep/raw-char)


(define-foreign (writec (in rep/raw-pointer) (in rep/raw-char))
  rep/undefined)
