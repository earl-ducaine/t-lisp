(herald (tsys misc t 114)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Miscellaneous utilities

(define *true-object*
  (object nil
          ((print self stream) (format stream "#T"))
          ((identification self) '*true-object*)))

(define (relocate-other unit type index env)
  (ignore unit env)
  (xcase type
	 ((%%true-rel-type) *true-object*)))

(define (make-token id)                 ; see READ
  (object nil
          ((print-type-string self) "Token")
          ((identification self) id)))

(define (arglist->argspectrum z)        ; see EVAL, OBJECT
  (iterate loop ((z z)
                 (n 0))
    (cond ((atom? z) (cons n (not (null? z))))
          (else (loop (cdr z) (fx+ n 1))))))

(define (compatible-with-argspectrum? l spect)
  (iterate loop ((l l) (n 0))
    (cond ((fx>= n (car spect))
           (cond ((null? (cdr spect))
                  (if (null? l) n nil))		;Must match exactly
                 ((fixnum? (cdr spect))
                  (iterate loop ((l l) (n n))
                    (cond ((or (fx>= n (cdr spect)) (atom? l))
                           (if (null? l) n nil))
                          (else (loop (cdr l) (fx+ n 1))))))
                 (else n)))	;Enough args, and no limit
          ((atom? l) nil)	;Too few args
          (else (loop (cdr l) (fx+ n 1))))))

;;; Symbol, combinator, locative, etc. stuff.

(define (symbol->string symbol)
  (copy-string (symbol-pname (check-arg symbol? symbol symbol->string))))

(define (walk-symbol-table proc symbol-table)
  (walk-vector (lambda (bucket) (walk proc bucket)) symbol-table))

(define (walk-symbols proc)
  (walk-symbol-table proc *the-symbol-table*))

(define (call-with-current-continuation proc)
  (catch tag (proc tag)))               ; Only works when compiled.

(define call-with-continuation call-with-current-continuation)

(define (map! proc l)
  (do ((z l (cdr z)))
      ((null? z) l)
    (set (car z) (proc (car z)))))

;;; Horrible cons-intensive definition.  Fix later.  There should be an
;;;  APPLY-MAP.

(define (map proc l . lists)
  (cond ((null? lists)
         (map1 proc l))
        (else
         (do ((l l (cdr l))
              (result '() (block0 (cons (apply proc (car l) (map1 car lists))
                                        result)
                                  (map! cdr lists))))
             ((or (null-list? l)
                  (any? null-list? lists))
              (reverse! result))))))

(define (mapcdr proc l . lists)
  (do ((l l (cdr l))
       (result '() (block0 (cons (apply proc l lists) result)
                           (map! cdr lists))))
      ((or (null-list? l)
           (any? null-list? lists))
       (reverse! result))))

(define (walk proc l . lists)
  (cond ((null? lists)
         (walk1 proc l))
        (else
         (do ((l l (cdr l)))
             ((or (null-list? l)
                  (any? null-list? lists))
              '*value-of-walk*)
           (apply proc (car l) (map1 car lists))
           (map! cdr lists)))))

(define (walkcdr proc l . lists)
  (do ((l l (cdr l)))
      ((or (null-list? l)
           (any? null-list? lists))
       '*value-of-walkcdr*)
    (apply proc l lists)
    (map! cdr lists)))

(define (map-append proc l . lists)
  (apply append (apply map proc l lists)))

(define (map-append! proc l . lists)
  (apply append! (apply map proc l lists)))



;;; Logical Connectives

;;; These should be rewritten later to offer some sense of efficiency. 
;;; What we have here is extremely elegant if a bit silly. Will work for
;;; the time being... 
;;;
;;; Perhaps set operations could be regular operations, with these 
;;;  definitions as default handlers... however this loses because, like
;;;  arithmetic, they want to be unbiased with respect to particular args...
;;;  sigh. Can win with negation anyway, since it is unary...

(define (disjoin . fns)
  (lambda arglist (any?   (lambda (fn) (apply fn arglist)) fns)))

(define (conjoin . fns)
  (lambda arglist (every? (lambda (fn) (apply fn arglist)) fns)))

(define (complement fn)
  (lambda arglist (not (apply fn arglist))))

;;; As an operation, the above would look like:
;;;
;;; (define-operation (complement fn)
;;;   (lambda arglist (not (apply fn arglist))))
;;;

;;; (COMPOSE f g ...)
;;;
;;; Composes procedures of 1 argument.  What would it mean to compose procedures
;;;  of more than 1 argument?  Returns a procedure which works like the 
;;;  composition of these.  Note, 
;;;
;;;     ((COMPOSE x1 x2 ... xN) y)  <=>  (x1 (x2 ... (xN y) ...))
;;;
;;;  so, for example,
;;;
;;;             CADR   is like  (COMPOSE CAR CDR)
;;;             CDDAR  is like  (COMPOSE CDR CDR CAR)
;;;
;;; Compiler should know about COMPOSE.  We wouldn't want ((COMPOSE ...) ...)
;;; to cons a closure.
;;;
;;; Last may a procedure of more than one argument.

(define (compose . procs)
  (cond ((null? procs) proj0)
        ((null? (cdr procs)) (car procs))
        (else (let ((prelude (apply compose (cdr procs)))
                    (finally (car procs)))
                (lambda args
                  (finally (apply prelude args)))))))


;;; Locative stuff goes here because there's so little of it.

;;; Operations on locatives

(define-settable-operation (contents loc))
(define set-contents (setter contents))

(define-operation (define-contents loc value))

(define-predicate locative?)

;;; Other locatives are made using the MAKE-LOCATIVE operation.

(define-operation (make-locative proc . args)
  (object nil
          ((contents self) (apply proc args))
          ((set-contents self val)
           (apply (setter proc) (append args (list val))))      ; choke!
          ((locative? self) t)
          ((print-type-string self) "Locative")))

;;; ---------- Miscellaneous.

;;; Assertion crud.  See also OPEN.
;;; CHECK-ARG now lives in SYSTEM.

(define (*check-arg predicate expression where)
  (if (predicate expression) expression
    (*check-arg predicate
                (error "some argument didn't answer true to ~S as expected~
                     ~%  (~S ... ~S ...)"
                       (or (identification predicate) predicate)
                       (or (identification where) where)
                       expression)
                where)))

(define (*enforce predicate expression)
  (if (predicate expression) expression
    (*enforce predicate
              (error "(~S ~S ~S) failed"
                     'enforce
                     (or (identification predicate) predicate)
                     expression))))
(define enforce *enforce)

(define (*assert something)
  (cond ((not something)
         (error "assertion failed")))
  '**value-of-assert**)
(define assert *assert)

(define (undefined-value . stuff)
  (cond ((null? stuff)
         ;; Don't close over STUFF
         (object nil
                 ((print self stream)
                  (format stream "#{Undefined-value~_~S}"
                          (object-hash self)))))
        (else
         (object nil
                 ((print self stream)
                  (format stream "#{Undefined-value~_~S"
                          (object-hash self))
		  (walk (lambda (x) (format stream "~_~s" x))
			stuff)
		  (writec stream #\}))))))

(define (undefined-effect . stuff)
  (error "call to ~S~%  ~S" 'undefined-effect `(undefined-effect . ,stuff)))

;;; Delays

(define (make-delay proc)
  (let ((value (value->nonvalue proc)))
    (let ((get-the-value 
           (lambda ()
             (cond ((nonvalue? value)
                    (set value
                         (let ((v ((nonvalue->value value))))
                           (cond ((nonvalue? v)
                                  (delay-lossage v))
                                 (else v)))))
                   (else value)))))
      (object (lambda () (get-the-value))
              ((force self) (get-the-value))
              ((delay? self) t)
              ((print-type-string self)
               (if (nonvalue? value) "Delayed" "Forced"))))))

(define (delay-lossage v)
  (let ((v (error "forcing a delay yielded nonvalue ~S" v)))
    (if (nonvalue? v) (delay-lossage v) v)))

(define-predicate delay?)
(define-operation (force obj) obj)

;;; Temporary definitions of VALUES and RECEIVE-VALUES for T 2.

(define *values-vector-length* 20)

(define *values-vector* (make-vector *values-vector-length*))

(define (multiple-values? x)
  (and (pair? x) (eq? (cdr x) '**multiple-values**)))

(define (receive-values recipient thunk)
  (let ((info (thunk)))
    (cond ((multiple-values? info)
	   (case (car info)
		 ;; Pessimizations
		 ((0) (recipient))
		 ((2) (recipient (vref *values-vector* 0)
				 (vref *values-vector* 1)))
		 ((3) (recipient (vref *values-vector* 0)
				 (vref *values-vector* 1)
				 (vref *values-vector* 2)))
		 (else
		  (do ((i (fx- (car info) 1) (fx- i 1))
		       (l '() (cons (vref *values-vector* i) l)))
		      ((fx< i 0)
		       (apply recipient l))))))
	  (else
	   (recipient info)))))

(define (values . vals)
  (cond ((null? vals) '(0 . **multiple-values**))
	((null? (cdr vals)) (car vals))
	(else
	 (do ((l vals (cdr l))
	      (i 0 (fx+ i 1)))
	     ((null? l)
	      (cons i '**multiple-values**))
	   (set (vref *values-vector* i) (car l))))))

(define (losing-receive **info** nvars)
  (error "wrong number of return values - expected ~s, received ~s~%  ~s"
         nvars
	 (if (multiple-values? **info**) (car **info**) 1)
	 `(values ,@(receive-values list (lambda () **info**)))))
