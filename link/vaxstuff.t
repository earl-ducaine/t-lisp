;;; Utilities

(define (write-int stream int)
  (if *debug?* (format t "    Wrote ~s = #x~x~%" int int))
  (write-half stream int)
  (let ((int (ash int -16)))
    (write-halfe stream int)))

(define (write-half stream int)
  (write-byte stream int)
  (let ((int (ash int -8)))
    (write-byte stream int)))

(define (write-byte stream n)
  (write-char stream (ascii->char (logand n 255))))

(define (write-bytev-image stream bytev)
  (let ((len (bytev-length bytev)))
    (write-int stream (logior (ash len 8)
                              (logior (ash tag2/bytev 2)
                                      tag1/imm)))
    (if *debug?* (format t "    Byte vector (~s bytes)~%" len))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i len)
         (do ((i i (fx+ i 1)))
             ((fx= (logand i 3) 0) 'done)
           (write-byte stream 0)))
      (write-byte stream (bref bytev i)))))

(define (align n m)
  (let ((2^m-1 (- (ash 1 m) 1)))
    (logand (+ n 2^m-1) (lognot 2^m-1))))

;;; Sample compiled expression.

(import *t-implementation-env* make-bytev bref bytev-length)

;;; (define comexes (list (make-sample)))

(define (make-sample)
  (let ((comex (make-comex))
        (code (make-bytev 25))
        (v (make-vector 1))
        (w (make-bytev 1)))
    (do ((i 0 (fx+ i 1)))
        ((= i (bytev-length code)))
      (set (bref code i) i))
    (set (comex-code         comex) code)
    (set (vref v 0) "_foo")
    (set (bref w 0) op/foreign)
    (set (comex-objects      comex) v)
    (set (comex-opcodes      comex) w)
    comex))

;;; Target parameters

(define-constant *cell* 4)

(define-constant tag/fixnum 0)
(define-constant tag/imm    1)
(define-constant tag/stored 2)
(define-constant tag/pair   3)

(define-constant header/char            1)
(define-constant header/bitv            9)
(define-constant header/text           17)
(define-constant header/general-vector 25)
(define-constant header/bytev          34)
(define-constant header/bignum         37)
(define-constant header/slice          41)
(define-constant header/string         41)
(define-constant header/symbol         49)
