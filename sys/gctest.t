

(define (test heap)
  (util-init)
  (gc-init)
  (let* ((old (heap))
         (new (gc old)))
    (util-init)
    (heap)
    (compare-n (make-descriptor 'esc old)
               (make-descriptor 'esc new)
               (gc-size (make-descriptor 'esc old)))))

;;; (1 2 3 4 5)

(define (test1)
  (do ((loc 2 (+ loc 2))
       (i 4 (-1+ i))
       (desc (make-pair 0 (make-fix 5) nil-desc) desc))
      ((>= 0 i))
      (set desc (make-pair loc (make-fix i) desc)))

  (make-general-vector 10 (make-descriptor 'pair 8))

  10)

;;; Vector(#\U #\g #\h)

(define (test2)
  (make-general-vector 0 (make-char #\U) (make-char #\g) (make-char #\h))

  (make-general-vector 10 (make-descriptor 'esc 0))

  10)


;;; Bit vector

(define (test3)
  (make-bit-vector 24 10)

  (make-general-vector 10 (make-descriptor 'esc 24))

  10)


;;; Other vector

(define (test4)
  (make-other-vector 24 10)

  (make-general-vector 10 (make-descriptor 'esc 24))

  10)


;;; Template inside bit vector

(define (test5)
  (make-bit-vector 24 10)

  (make-template 27 'heap 3 3 2)

  (make-general-vector 10 (make-descriptor 'esc 27))

  10)


;;; Heaped closure

(define (test6)
  (make-bit-vector 24 10)

  (make-template 27 'heap 3 3 2)

  (make-closure 1 27 (make-fix 5) (make-fix 6) (make-fix 7))

  (make-general-vector 10 (make-descriptor 'esc 1))

  10)


;;; Closure internal closure

(define (test7)
  (make-bit-vector 24 10)

  (make-template 27 'heap 3 5 1)
  (make-template 29 'closure 5 0 4)

  (make-closure 40 27 (make-char #\X) (make-char #\Y) (make-char #\Z)
                      (make-fix 111) (make-char #\A))
  (make-closure 44 29)

  (make-general-vector 10 (make-descriptor 'esc 44))

  10)


;;; Closure internal template

(define (test8)
  (make-bit-vector 24 10)

  (make-template 27 'heap 3 6 1)
  (make-template 32 'heap 8 0 0)   ;;; Aux template for clos. internal template

  (make-closure 40 27 (make-char #\X) (make-char #\Y) (make-char #\Z)
                      (make-template-1 5 3 2)
                      (make-template-2 'heap)
                      (make-descriptor 'esc 32))

  (make-closure 64 45 (make-descriptor 'esc 40)
                      (make-fix 1) (make-fix 2))

  (make-general-vector 10 (make-descriptor 'esc 64))

  10)


;;; Vectors

(define (test9)
  (make-general-vector  0 (make-fix 1) (make-descriptor 'esc 20) (make-fix 2))
  (make-general-vector 20 (make-fix 3) (make-descriptor 'esc 30) (make-fix 4))
  (make-general-vector 30 (make-fix 5) (make-descriptor 'esc  0) (make-fix 6))

  (make-general-vector 10 (make-descriptor 'esc 0))

  10)
