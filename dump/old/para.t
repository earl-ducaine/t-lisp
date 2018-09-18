(Herald Para (env t))

;;;
;;; parameter definitions for the encoding and decoding
;;;

(define-constant %-null               0)       ;;; () type
(define-constant %-char               1)       ;;; character
(define-constant %-pair               2)       ;;; cons pair
(define-constant %-pair-shared        3)       ;;; shared cons pair

(define-constant %-fixnum             4)       ;;; fixed number
(define-constant %-old-pointer        8)       ;;; an old pointer
(define-constant %-pair-number       16)       ;;; number of shared pairs
(define-constant %-symbol            20)       ;;; symbol
(define-constant %-symbol-shared     24)       ;;; shared symbol
(define-constant %-old-symbol        28)       ;;; an old symbol
(define-constant %-symbol-number     32)       ;;; number of shared symbols
(define-constant %-string            36)       ;;; string
(define-constant %-string-shared     40)       ;;; shared string
(define-constant %-old-string        44)       ;;; an old string
(define-constant %-string-number     48)       ;;; number of shared strings
(define-constant %-vector            52)       ;;; vector
(define-constant %-vector-shared     56)       ;;; shared vector
(define-constant %-byte-vector       60)       ;;; byte vector
(define-constant %-bytev-shared      64)       ;;; shared byte vector

(define-constant %-bignum-p          65)       ;;; positive big number
(define-constant %-bignum-p-shared   66)       ;;; shared positive big number
(define-constant %-bignum-n          67)       ;;; negative big number
(define-constant %-bignum-n-shared   68)       ;;; shared negative big number
(define-constant %-big-flo           69)       ;;; big floating point
(define-constant %-big-flo-shared    70)       ;;; shared big floating point


;;; map size
;;;

(define-constant %-ptr-map-size 1111)        ;;; for the pair map
(define-constant %-map-size     1111)        ;;; for the symbol and string map
