(herald aliases
        (support (make-empty-support-env 'nil) vaxbase vaxprimops open))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Standard name abbreviations

;;; Handy abbreviations for element accessors.
;(define-integrable vref vector-elt)
;(define-integrable vset set-vector-elt)

;(define-integrable char       string-head)
;(define-integrable chdr       string-tail)
;(define-integrable chdr!      string-tail!)
;(define-integrable nthchdr    string-nthtail)
;(define-integrable nthchdr!   string-nthtail!)
;(define-integrable nthchar    string-elt)

(define-integrable fx+  fixnum-add)
(define-integrable fx-  fixnum-subtract)
(define-integrable fx*  fixnum-multiply)
(define-integrable fx/  fixnum-divide)
(define-integrable fx=  fixnum-equal?)
(define-integrable fx<  fixnum-less?)
(define-integrable fx>  fixnum-greater?)
(define-integrable fxn= fixnum-not-equal?)
(define-integrable fx>= fixnum-not-less?)
(define-integrable fx<= fixnum-not-greater?)

;(define-integrable fl+  flonum-add)
;(define-integrable fl-  flonum-subtract)
;(define-integrable fl*  flonum-multiply)
;(define-integrable fl/  flonum-divide)
;(define-integrable fl=  flonum-equal?)
;(define-integrable fl<  flonum-less?)
;(define-integrable fl>  flonum-greater?)
;(define-integrable fln= flonum-not-equal?)
;(define-integrable fl>= flonum-not-less?)
;(define-integrable fl<= flonum-not-greater?)

;(define-integrable bref         bytev-elt)
;(define-integrable bref-8       bytev-elt-8)
;(define-integrable bref-16      bytev-elt-16)
;(define-integrable bref-32      bytev-elt-32)
;(define-integrable bref-pointer bytev-elt-pointer)
