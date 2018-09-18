(herald codes
        (env t))

;;;
;;; codes used for encoding and decoding
;;;

;;; Unstructured codes
(define-constant dump/null               0)       ;;; the empty list
(define-constant dump/char               1)       ;;; character
(define-constant dump/true               2)       ;;; '#t

;;; Codes that are shared or unshared
;;;   Low order bit zero = 0, not shared
;;;                      = 1, shared
(define-constant dump/pair               4)       ;;; cons pair
(define-constant dump/float              6)       ;;; floating point number
(define-constant dump/coded             72)       ;;; Coded by external proc.

;;; Codes that have size bytes
;;;   Number of bytes = (+ 1 (remainder code 4))
(define-constant dump/object-ref         8)       ;;; an already stored object
(define-constant dump/string-ref        12)       ;;; an already stored string
(define-constant dump/positive-fixnum   16)       ;;; positive fixnum
(define-constant dump/negative-fixnum   20)       ;;; negative fixnum

;;; Codes that are shared or unshared and have size bytes
;;;   Low order bit zero = 0, not shared
;;;                      = 1, shared
;;;   Number of bytes = (+ 1 (div (remainder code 8) 2))
(define-constant dump/string            24)       ;;; string
(define-constant dump/symbol            32)       ;;; symbol
(define-constant dump/vector            40)       ;;; vector
(define-constant dump/byte-vector       48)       ;;; byte vector
(define-constant dump/positive-bignum   56)       ;;; positive bignum
(define-constant dump/negative-bignum   64)       ;;; negative bignum
