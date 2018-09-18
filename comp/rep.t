(herald rep)

(lset *reps* '(rep/raw-char
               rep/raw-pointer
               rep/raw-integer
               rep/pointer))

(lset *rep-converter-table* (make-table 'reps))

(walk (lambda (rep)
        (set (table-entry *rep-converter-table* rep) (make-table 'reps)))
      *reps*)

(define-local-syntax (define-rep-converter from to proc)
  `(set (table-entry (table-entry *rep-converter-table* ',to) ',from)
        ,proc))

(define-local-syntax (define-shift-converter from to amount)
  `(define-rep-converter ,from ,to (shift-converter ,amount)))

(define (shift-converter amount)
  (lambda (from to)
    (emit vax/ashl (machine-num amount) from to)))



(define-shift-converter rep/pointer rep/raw-char -8)
(define-shift-converter rep/pointer rep/raw-integer -2)
(define-shift-converter rep/raw-integer rep/pointer 2)

(define-rep-converter rep/raw-char rep/pointer
  (lambda (from to)
    (emit vax/ashl (machine-num 8) from to)
    (emit vax/movb (machine-num header/char) to)))

(define-rep-converter rep/pointer rep/raw-pointer
  (lambda (from to)
    (emit vax/addl3 from (machine-num 2) to)))

(define (really-rep-convert from from-rep to to-rep)
  ((table-entry (table-entry  *rep-converter-table* to-rep) from-rep) from to))


(define (rep-push value to-rep)
  (cond ((immediate? value)
         (emit vax/pushl (value-with-rep value to-rep)))
        (else
         (let ((access (access-value nil value))
               (from-rep (rep value)))
           (cond ((eq? from-rep to-rep)
                  (emit vax/pushl access))
                 (else
                  (really-rep-convert access from-rep (@-r nil SP) to-rep))))))
  (increment-stack))

(define (value-with-rep value rep)
  (xcond ((char? value)
          (xcond ((eq? rep 'rep/raw-char)
                  (machine-num (char->ascii value)))
                 ((eq? rep 'rep/pointer)
                  (machine-num (fixnum-logior (fixnum-ashl value 8)
                                              header/char)))))
         ((fixnum? value)
          (xcond ((eq? rep 'rep/raw-integer)
                  (machine-num value))
                 ((eq? rep 'rep/pointer)
                  (lit value))))))

(define (rep value)
  'rep/pointer)
