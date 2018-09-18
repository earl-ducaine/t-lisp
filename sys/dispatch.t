(herald (tsys dispatch t 7)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;; Specific routines generation

;;; Mung a random piece of s-expression into a bunch of SET-DISPATCHES
;;; that define all the routines for generic arithmetic.

(define-local-syntax (define-specific-ops . data)
  (locale ()

(define (process-one-specific-entry entry)
  (let ((types (car entry))
        (opspecs (cdr entry)))
    `(set-dispatches ,(concatenate-symbol '%% (car types) '-number-type)
                     ,(concatenate-symbol '%% (cadr types) '-number-type)
                     ,@(process-subentries opspecs))))

(define (process-subentries specs)
  (let ((probe (apply append (map car specs))))
    (cond ((not (alikev? probe
                         '(add subtract multiply divide div less? equal?)))
           (error "bad clause order in subentry~%  ~S"
                  probe))))
  (apply append! (map process-subentry specs)))

(define (process-subentry subentry)
  (map (lambda (op) (hacked-eval (cadr subentry) op))
       (car subentry)))

(define (hacked-eval form opval)
  (cond ((eq? form 'op)     opval)
        ((eq? form 'conc)   concatenate-symbol)
        ((eq? form 'specop) specop)
        ((pair? form)
         (cond ((eq? (car form) 'quote) (cadr form))
               (else
                (apply (hacked-eval (car form) opval)
                       (map (lambda (a) (hacked-eval a opval)) (cdr form))))))
        (else (error "cannot HACKED-EVAL ~S" form))))

;;; Construct code for specific routines to {add, subtract, multiply, divide}
;;; two numbers, one of type OPD1 and the other of type OPD2.
;;; The routine will coerce OPD1 and OPD2 to OPR-TYPE and then apply
;;; the appropriate type-specific arithmetic routine.

(define (specop opr opd1 opd2 opr-type)
  `(lambda (x y) (,(concatenate-symbol opr-type '- opr)
                  ,(coerced-operand 'x opd1 opr-type)
                  ,(coerced-operand 'y opd2 opr-type))))

(define (coerced-operand operand opd-type opr-type)
  (cond ((neq? opd-type opr-type)
         `(,(concatenate-symbol opd-type '-> opr-type) ,operand))
        (else operand)))

;;; This is the last form in the initial (locale () ...).  It yields the form
;;; which will actually be compiled.  (All the above code is ephemeral -
;;; it exists only at compile time.)

`(block ,@(map process-one-specific-entry data))))


;;; Order of ops is significant!

(define-specific-ops

  ((fixnum fixnum)
   ((add subtract multiply divide) (conc 'fixnum- op '-carefully))
   ((div) 'fixnum-divide)
   ((less? equal?) (conc 'fixnum- op)))

  ((fixnum flonum)
   ((add subtract multiply divide) 
    (specop op 'fixnum 'flonum 'flonum))
   ((div)
    '(lambda (x y) (flonum-divide (fixnum->flonum x) y)))
   ((less? equal?)
    (specop op 'fixnum 'flonum 'flonum)))

  ((fixnum bignum)
   ((add subtract multiply) (specop op 'fixnum 'bignum 'bignum))
   ((divide) 'ratio) 
   ((div)
    '(lambda (x y) (ignore x y)
       (if (and (fx= x *min-fixnum*)        ;Thanks to Joe Stoy!
                (= y (- *min-fixnum*)))
           -1
           0)))
   ((less?)  '(lambda (x y) (ignore x) (bignum-positive? y)))
   ((equal?) 'false))

  ((fixnum ratio)
   ((add subtract multiply divide div less?) (conc 'rational- op))
   ((equal?) 'false))

  ((flonum fixnum)
   ((add subtract multiply divide) (specop op 'flonum 'fixnum 'flonum))
   ((div) '(lambda (x y) (flonum-divide x (fixnum->flonum y))))
   ((less? equal?) (specop op 'flonum 'fixnum 'flonum)))

  ((flonum flonum)
   ((add subtract multiply divide)  (conc 'flonum- op))
   ((div) 'flonum-divide)
   ((less? equal?) (conc 'flonum- op)))

  ((flonum bignum)
   ((add subtract multiply divide) (specop op 'flonum 'bignum 'flonum))
   ((div) '(lambda (x y) (flonum-divide x (bignum->flonum y))))
   ((less? equal?) (specop op 'flonum 'bignum 'flonum)))

  ((flonum ratio)
   ((add subtract multiply divide) (specop op 'flonum 'ratio 'flonum))
   ((div) '(lambda (x y) (flonum-divide x (ratio->flonum y))))
   ((less? equal?) (specop op 'flonum 'ratio 'flonum)))

  ((ratio fixnum)
   ((add subtract multiply divide div less?) (conc 'rational- op))
   ((equal?) 'false))

  ((ratio flonum)
   ((add subtract multiply divide) (specop op 'ratio 'flonum 'flonum))
   ((div) '(lambda (x y) (flonum-divide x (ratio->flonum y))))
   ((less? equal?) (specop op 'ratio 'flonum 'flonum)))

  ((ratio bignum)
   ((add subtract multiply divide div less?) (conc 'rational- op))
   ((equal?) 'false))

  ((ratio ratio)
   ((add subtract multiply divide div less? equal?) (conc 'rational- op)))

  ((bignum fixnum)
   ((add subtract multiply) (specop op 'bignum 'fixnum 'bignum))
   ((divide) 'ratio)
   ((div)    'b-f-divide)
   ((less?)  '(lambda (x y) (ignore y) (not (bignum-positive? x))))
   ((equal?) 'false))

  ((bignum flonum)
   ((add subtract multiply divide) (specop op 'bignum 'flonum 'flonum))
   ((div) '(lambda (x y) (flonum-divide (bignum->flonum x) y)))
   ((less? equal?) (specop op 'bignum 'flonum 'flonum)))
    
  ((bignum bignum)
   ((add subtract multiply) (conc 'bignum- op))
   ((divide) 'ratio)
   ((div) 'bignum-divide)
   ((less? equal?) (conc 'bignum- op)))
  
  ((bignum ratio)
   ((add subtract multiply divide div less?) (conc 'rational- op))
   ((equal?) 'false))
  )
