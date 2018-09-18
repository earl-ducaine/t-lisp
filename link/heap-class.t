(herald heap-class)

;;; Classify objects into the heap they should be relocated into.
;;; We use several sources of info here:
;;;     A linker supplied table mapping objects to heaps
;;;     A default map

;;; In the case of a system suspend, the default map checks which segment
;;; the object comes from. In the case of a system build, the default map
;;; is data type driven.

;;; The function reloc-heap returns one of the three dynamically bound
;;; vars, *pi*, *pd*, *im* for pure-position-independent,
;;; pure-position-dependent, and impure, respectively.

;;; Here is the classifier for the system build case:

(define (reloc-heap object heap-table)
  (case (table-entry heap-table object)
    ((pi) *pi*)
    ((pd) *pd*)
    ((im) *im*)
    ((pure)
     (if (or (immediate? object)    (symbol? object)
             (bytev? object)        (bignum? object)
             (bitv? object))
         *pi*
         *pd*))
    (else
     (cond ((or (immediate? object) (symbol? object)
                (bignum? object)    (template-desc? object))
            *pi*)
           (else
            *im*))))

;;; impure things: unit-store? closure-desc? refs-list?
;;;                pair? vector? bytev? bitv?
