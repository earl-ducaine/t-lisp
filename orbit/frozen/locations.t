(herald locations)

(define-local-syntax (define-location name loc)
  `(define-wired ,name
     (object (lambda (x)
               (contents (,loc x)))
       ((setter self)
        (lambda (x v)
          (set-contents (,loc x) v)))
       ((locatizer self)
        (lambda (x)
          (,loc x))))))

(define-location xenoid-pointer xenoid-pointer-loc)

(define-location extend-template extend-template-loc)

(define-location string-text string-text-loc)

(define-location string-offset string-offset-loc)

(define-location car car-loc)

(define-location cdr cdr-loc)
