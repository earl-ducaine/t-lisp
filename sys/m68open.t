(herald m68open
        (env () (tsys aliases)))

;;; 68000-specific integrable procedure definitions.

;;; STRING-TEXT

(define-integrable string-text string-pointer)

;;; Shifting.

(define-integrable fixnum-ashl pointer-ashl)

(define-integrable (pointer-ash ptr i)
  (if (fx< i 0)
      (pointer-ashr ptr (fixnum-abs i))
      (pointer-ashl ptr i)))

(define-integrable (fixnum-ash f i)
  (if (fx< i 0)
      (fixnum-ashr f (fixnum-abs i))
      (fixnum-ashl f i)))

;;; Byte vector lossage

(define-integrable bytev-elt
  (if-integrated (lambda (bv i)
                   (fixnum-logand (bytev-elt-8 bv i) 255.))
                 (make-accessor (lambda (vec idx) 
                                  (bytev-elt vec idx))
                                set-bytev-elt-8)))
  
(declare-setter bytev-elt set-bytev-elt-8)
