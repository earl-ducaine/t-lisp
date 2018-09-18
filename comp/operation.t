(herald operation)

;;; Enough stuff to define SETTER and *THE-BUCK-STOPS-HERE* which are needed to
;;; define and integrate CAR etc.

(define (*operation default spect id h)
 (labels ((op (object (lambda (obj . args)
                        (apply operate op obj *the-buck-stops-here* obj args))
                      ((argspectrum self) spect)
                      ((identification self) id)
                      ((default-method self) default)
                      ((operation? self) t)
                      (=> h))))
   op))

(define (operate obj op next self . args)
  (receive (method obj op next)
           (perform-dispatch obj op next)
    (apply method obj op next args)))

(define *the-buck-stops-here*
  (*object nil
           (lambda (obj state)
             (ignore obj)
             (let ((op (%state-op state)))
               (cond ((operation? op)
                      (%operation-default op))
                     (else                ;For trace package!
                      (default-method op)))))))

(define-wired setter
  (*operation nil '(1) 'setter handle-operation))
