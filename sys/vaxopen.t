(herald (tsys vaxopen t 12)
        (env () (tsys aliases)))

;;; Copyright (c) 1983, 1984 Yale University

;;; VAX-specific integrable procedures.

(define-integrable (string-text string)
  (pointer-subtract (string-pointer string)
	  	    (fixnum->pointer (string-base string))))

(define-integrable template-guts? rel-item?)

;;; Shift right & left.

(define-integrable pointer-ashl pointer-ash)
(define-integrable fixnum-ashl  fixnum-ash)
;(define-integrable fixnum-ashl  pointer-ashl)

(define-integrable (fixnum-ashr x y)
  (fixnum-ash x (fx- 0 y)))

(define-integrable (pointer-ashr x y)
  (pointer-ash x (fx- 0 y)))

;;; BYTEV accessors.

;;; Byte vector lossage

;;; Weird inconsistency.  In the 68000 code generator, rep BYTE means
;;; a signed byte, whereas on the VAX it means an unsigned byte.
;;; So the primitive bytev accessor we get for the VAX is BREF (= MREF-8-U),
;;; and on the 68000 it's BREF-8 (= MREF-8).

(define-integrable bytev-elt mref-8-u)

(define-integrable bytev-elt-8  ; = mref-8
  (object (lambda (bv i)
            (let ((z (mref-8-u bv i)))
              (if (fx>= z 128) (fx- z 256) z)))
          ((setter self)
           ((no-op setter) mref-8-u))))

(declare-setter bytev-elt-8 (setter mref-8-u))

(define-integrable bytev-elt-16
  (object (lambda (bv i) (mref-16 bv (fixnum-ashr i 1)))
          ((setter self)
           (lambda (bv i val)
             (set (mref-16 bv (fixnum-ashr i 1)) val)))))

(define-integrable bytev-elt-32
  (object (lambda (bv i) (mref-32 bv (fixnum-ashr i 2)))
          ((setter self)
           (lambda (bv i val)
             (set (mref-32 bv (fixnum-ashr i 2)) val)))))

(define-integrable bytev-elt-pointer
  (object (lambda (bv i) (mref bv (fixnum-ashr i 2)))
          ((setter self)
           (lambda (bv i val)
             (set (mref bv (fixnum-ashr i 2)) val)))))

;;; Losing code generator.

(define-integrable (fixnum-remainder x y)
  (fixnum-subtract x (fixnum-multiply (fixnum-divide x y) y)))

;;; Silly VAX doesn't have full complement of logical ops.

(define-integrable (fixnum-logand x y)
  (if-integrated (fixnum-logandc2 x (fixnum-lognot y))
		 (let ((x (check-arg fixnum? x fixnum-logand))
		       (y (check-arg fixnum? y fixnum-logand)))
		   (fixnum-logandc2 x (fixnum-lognot y)))))

(define-integrable (pointer-logand x y)
  (pointer-logandc2 x (pointer-lognot y)))
