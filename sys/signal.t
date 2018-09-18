(herald (tsys signal t 129)
        (pre-cook)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Signals and errors

(define (make-condition-type default string id)
  (let ((h default))
    (object nil
            ((instantiate-condition self args) ;originally had . ARGS; TC lost
             (object nil
                     ((process-signal self)
                      (h self))
                     ((print-signal self stream)
                      (format stream "~&** ~A: " string)
                      (apply format stream args)
                      (fresh-line stream))
                     ((print-type-string self) string)))
            ((condition-handler self) h)
            ((set-condition-handler self val) (set h val))
            ((identification self) id)
            ((print-type-string self) "Condition"))))

;;; Operations on conditions

(define-operation (instantiate-condition condition args))

(define-settable-operation (condition-handler condition-type))
(define set-condition-handler (setter condition-handler))

;;; Operation on condition instance

(define-operation (process-signal instance))
(define-operation (print-signal instance stream))

(define (signal condition . args)
  (process-signal (instantiate-condition condition args)))

;;; Utility for CONDITION-BIND (?)

(define (cons-condition-handler proc type)
  (let ((proc (check-arg procedure? proc cons-condition-handler))
        (punt (condition-handler type)))
    (lambda (err)
      (proc err punt))))

;;; ---------- Error conditions.

;;; Error conditions in general

(define (make-error-type string id)
  (make-condition-type *standard-error-handler* string id))

(define (*standard-error-handler* err)
  (catch error-point
    (bind ((*the-error* (cons err error-point)))
      (catch abort
        (bind ((*reporting-error?* t)
               (*abort-error-report* abort))
          (let ((out (error-output)))
            (format out "~&~%")         ; don't use ~2& - ZFORMAT can't cope
            (print-signal err out))))
      (error-breakpoint))))

(lset *the-error* nil)
(lset *reporting-error?* nil)
(lset *abort-error-report* nil)

;;; Particular error conditions.

(define (error f-string . f-args)
  (signal-error *unspecific-error-type* f-string f-args))

(define *unspecific-error-type*
  (make-error-type "Error" '*unspecific-error-type*))

(define (syntax-error f-string . f-args)
  (signal-error *syntax-error-type* f-string f-args))

(define *syntax-error-type*
  (make-error-type "Syntax error" '*syntax-error-type*))

(define (read-error stream f-string . f-args)
  (clear-input stream)
  (signal-error *read-error-type*
                (append (cond ((pair? f-string) f-string)
                              (else (list f-string)))
                        '("~&  (line ~S of stream ~S)"))
                (append f-args (list (vpos stream) stream))))

(define *read-error-type*
  (make-error-type "Read error" '*read-error-type*))

(define (signal-error error-type f-string f-args)
  (cond (*z?*
         (apply zerror f-string f-args))
        ((eq? *reporting-error?* '*reporting-error?*)
         (set *z?* t)
         (apply zerror f-string f-args))
        (*reporting-error?*
         (bind ((*reporting-error?* '*reporting-error?*)) 
           (format (error-output)
                   "~&** Error while reporting error!~%")
           (*abort-error-report* nil)))
        (else
         (apply signal error-type f-string f-args))))

;;; ---------- Other stuff.

(define (illegal-call proc args)
  (let ((punt (lambda ()
                (error "attempt to call a non-procedure~%  ~S"
                       (cons proc args)))))
    (cond ((not (reasonable? proc))
           (error "attempt to call a corrupt datum~%  ~S"
                  (cons proc args)))
          ((symbol? proc)               ; Cater to the confused
           (error "attempt to call a symbol~%  ~S"
                  (cons proc args)))
          ((nonvalue? proc)
           (let ((thing (nonvalue->value proc)))
             (cond ((and (vcell? thing)
                         (eq? (vcell-contents thing) proc))
                    (error "attempt to call an undefined procedure~%  ~S"
                           (cons (vcell-id thing) args)))
                   (else (punt)))))
          (else (punt)))))
