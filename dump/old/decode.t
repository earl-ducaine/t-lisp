(Herald Decode (env t ))

(lset *func-vec* (vector-fill (make-vector 256) '()))

;;; (READ2C dump-stream)
;;; ==================================================================
;;; Read in 2 bytes, lower byte first and return an integer.
;;; Assuming there is no EOF when calling.
;;;

(define-operation (read2c dump-stream))


;;; (READ3C dump-stream)
;;; ==================================================================
;;; Read in 3 bytes, lower byte first and return an integer.
;;; Assuming there is no EOF when calling.
;;;

(define-operation (read3c dump-stream))

;;; (READ4C dump-stream)
;;; ==================================================================
;;; Read in 4 bytes, lower byte first and return an integer.
;;; Assuming there is no EOF when calling.
;;;

(define-operation (read4c dump-stream))

;;; (READ-STRING dump-stream n)
;;; ==================================================================
;;; Read in n bytes, lower byte first,  and return a T string.
;;; Assuming there is no EOF when calling.
;;;

(define-operation (read-string dump-stream n))

;;; (GET-FIXNUM dump-stream type start-value)
;;;======================================================================
;;; Read in 1, 2, 3 or 4 bytes and concatnate them together according to
;;; the real type.  Our coding rule is:
;;;     start-value:     1 byte
;;;     start-value + 1: 2 bytes
;;;     start-value + 2: 3 bytes
;;;     start-value + 3: 4 bytes
;;; Assuming that calling has no error for efficience.
;;;

(define-operation (get-fixnum dump-stream type start-value))

;;; (GET-DUMP dump-stream)
;;;========================================================================
;;; Return the next s-expression you dumped to the file, if there is an
;;; dumped s-express; otherwise, return *eof* object.
;;;

(define-operation (get-dump dump-stream))


;;;
;;; The following are the access methods used in the function-vector
;;; for different data types
;;;
;;;


(define-operation (do-char dump-stream))
(define-operation (do-pair dump-stream))
(define-operation (do-pair-shared dump-stream))
(define-operation (do-fixnum dump-stream))
(define-operation (do-old-pointer dump-stream))
(define-operation (do-pointer-number dump-stream))

(define-operation (do-symbol dump-stream))
(define-operation (do-symbol-shared dump-stream))
(define-operation (do-old-symbol dump-stream))
(define-operation (do-symbol-number dump-stream))

(define-operation (do-string dump-stream))
(define-operation (do-string-shared dump-stream))
(define-operation (do-old-string dump-stream))
(define-operation (do-string-number dump-stream))

(define-operation (do-vector dump-stream))
(define-operation (do-vector-shared dump-stream))
(define-operation (do-byte-vector dump-stream))
(define-operation (do-bytev-shared dump-stream))

(define-operation (do-bignum-p dump-stream))
(define-operation (do-bignum-p-shared dump-stream))
(define-operation (do-bignum-n dump-stream))
(define-operation (do-bignum-n-shared dump-stream))
(define-operation (do-big-flo dump-stream))
(define-operation (do-big-flo-shared dump-stream))

(define-operation (prd dump-stream))        ;;; for debugging


;;; (CLOSE-DUMPED dump-stream)
;;;=======================================================================
;;; Close the dumped file you just opened.
;;;

(define-operation (close-dumped dump-stream))


(define (open-dumped file)
    (let* ((string-vec '())
           (symbol-vec '())
           (pointer-vec '())
           (string-id 0)
           (symbol-id 0)
           (pointer-id 0)                     
           (type 0)
           (stream (open file '(in)))
           (read1c (pre-dispatch readc stream)))

      (object nil

        ((get-dump self)
         (let ((token-char (read1c stream)))
           (cond ((eof? token-char)
                  token-char)
                 (else
                  (set type (char->ascii token-char))
                  ((vref *func-vec* type) self)))))

        ((read2c self)
         (let ((value (char->ascii (read1c stream))))
           (fixnum-logior (fixnum-ashl (char->ascii (read1c stream))
                                       8) value)))

        ((read3c self)
         (let ((value (char->ascii (read1c stream))))
           (set value
                (fixnum-logior (fixnum-ashl (char->ascii (read1c stream))
                                            8) value))
           (fixnum-logior (fixnum-ashl (char->ascii (read1c stream))
                                       16) value)))

        ((read4c self)
         (let ((value (char->ascii (read1c stream))))
           (set value
                (fixnum-logior (fixnum-ashl (char->ascii (read1c stream))
                                        8) value))
           (set value
                (fixnum-logior (fixnum-ashl (char->ascii (read1c stream))
                                        16) value))
           (fixnum-logior (fixnum-ashl (char->ascii (read1c stream))
                                   24) value)))

        ((read-string self n)
         (let ((buf (get-buffer)))
           (loop (incr i from 0 to (fx- n 1))
                 (do (buffer-writec buf (read1c stream))))
           (block0 buf
                   (release-buffer buf))))

        ((get-fixnum self type start-value)
         (cond ((fx= type start-value)
                (char->ascii (read1c stream)))
               ((fx= type (fx+ start-value 1))
                (read2c self))
               ((fx= type (fx+ start-value 2))
                (read3c self))
               (else
                (read4c self))))

        ((prd self)
         (format t "~&~s" symbol-vec)
         (format t "~&~s" string-vec)
         (format t "~&~s" pointer-vec)
         (format t "~&ptr ~d str ~d sym ~d" pointer-id string-id symbol-id))

        ((do-char self)
         (read1c stream))

        ((do-fixnum self)
         (get-fixnum self type %-fixnum))

        ((do-pair self)
         (cons (get-dump self)
               (get-dump self)))

        ((do-pair-shared self)
         (let ((cell (cons 0 0)))
           (set (vref pointer-vec (set pointer-id (fx+ pointer-id 1))) cell)
           (set (car cell) (get-dump self))
           (set (cdr cell) (get-dump self))
           cell))

        ((do-old-pointer self)
         (vref pointer-vec (get-fixnum self type %-old-pointer)))

        ((do-pointer-number self)
         (set pointer-vec
              (vector-fill
                   (make-vector (get-fixnum self type %-pair-number)) '()))
         (get-dump self))

        ((do-symbol self)
         (string->symbol (read-string self
                                  (get-fixnum self type %-symbol))))

        ((do-symbol-shared self)
         (set (vref symbol-vec (set symbol-id (fx+ symbol-id 1)))
              (string->symbol (read-string self
                                       (get-fixnum self
                                                   type %-symbol-shared)))))

        ((do-old-symbol self)
         (vref symbol-vec (get-fixnum self type %-old-symbol)))

        ((do-symbol-number self)
         (set symbol-vec
              (vector-fill (make-vector (get-fixnum self
                                                    type %-symbol-number)) '()))
         (get-dump self))

        ((do-string self)
         (copy-string (read-string self (get-fixnum self type %-string))))

        ((do-string-shared self)
         (set (vref string-vec (set string-id (fx+ string-id 1)))
              (copy-string
                  (read-string self (get-fixnum self type %-string-shared)))))

        ((do-old-string self)
         (vref string-vec (get-fixnum self type %-old-string)))

        ((do-string-number self)
         (set string-vec
              (vector-fill (make-vector (get-fixnum self
                                                    type
                                                    %-string-number)) '()))
         (get-dump self))

        ((do-vector self)
         (let* ((vec-len (get-fixnum self type %-vector))
                (vec (vector-fill (make-vector vec-len) 0)))
           (loop (incr i from 0 to (fx- vec-len 1))
                 (do (set (vref vec i) (get-dump self))))
           vec))

        ((do-vector-shared self)
         (let* ((vec-len (get-fixnum self type %-vector-shared))
                (vec (vector-fill (make-vector vec-len) 0)))
           (set (vref pointer-vec (set pointer-id (fx+ pointer-id 1))) vec)
           (loop (incr i from 0 to (fx- vec-len 1))
                 (do (set (vref vec i) (get-dump self))))
           vec))

        ((do-byte-vector self)
         (let* ((vec-len (get-fixnum self type %-byte-vector))
                (vec (make-bytev vec-len)))
           (loop (incr i from 0 to (fx- vec-len 1))
                 (do (set (bref-8 vec i) (char->ascii (read1c stream)))))
           vec))

        ((do-bytev-shared self)
         (let* ((vec-len (get-fixnum self type %-bytev-shared))
                (vec (make-bytev vec-len)))
           (set (vref pointer-vec (set pointer-id (fx+ pointer-id 1))) vec)
           (loop (incr i from 0 to (fx- vec-len 1))
                 (do (set (bref-8 vec i) (char->ascii (read1c stream)))))
           vec))

        ((do-big-flo self)
         (let* ((p1 (read2c self))
                (p2 (read2c self))
                (p3 (read2c self))
                (p4 (read2c self)))
           (fixnums-replace-flonum (fl+ 0.0 (no-op 0.0)) p1 p2 p3 p4)))

        ((do-big-flo-shared self)
         (let* ((p1 (read2c self))
                (p2 (read2c self))
                (p3 (read2c self))
                (p4 (read2c self)))

           (set (vref pointer-vec (set pointer-id (fx+ pointer-id 1)))
                (fixnums-replace-flonum (fl+ 0.0 (no-op 0.0)) p1 p2 p3 p4))))

        ((do-bignum-p self)
         (let ((len (char->ascii (read1c stream)))
               (fi 0)
               (value 0))
           (loop (incr i from 2 to len)
                 (do (set fi (read2c self))
                     (set value (* (+ value fi) 65536))))
           (+ value (read2c self))))

        ((do-bignum-p-shared self)
         (let ((len (char->ascii (read1c stream)))
               (fi 0)
               (value 0))
           (loop (incr i from 2 to len)
                 (do (set fi (read2c self))
                     (set value (* (+ value fi) 65536))))
           (set (vref pointer-vec (set pointer-id (fx+ pointer-id 1)))
                (+ value (read2c self)))))

        ((do-bignum-n self)
         (- (do-bignum-n self)))

        ((do-bignum-n-shared self)
         (- (do-bignum-n-shared self)))

        ((close-dumped self)
         (vector-fill string-vec 0)      ;;; for garbage collection
         (vector-fill symbol-vec 0)
         (vector-fill pointer-vec 0)
         (close stream)))))


;;;
;;; The following is the binding of different do functions for different
;;; data types, so that we can switch function by using the *func-vec*.
;;;


(loop (incr i from 0 to 255)
      (do (set (vref *func-vec* i)
               (lambda (x) '()))))              ;;; do nothing

(define (binfour i func)
    (set (vref *func-vec* i) func)
    (set (vref *func-vec* (fx+ i 1)) func)
    (set (vref *func-vec* (fx+ i 2)) func)
    (set (vref *func-vec* (fx+ i 3)) func))

(set (vref *func-vec* %-char) do-char)          ;;; char
(set (vref *func-vec* %-pair) do-pair)          ;;; pair
(set (vref *func-vec* %-pair-shared)
     do-pair-shared)                            ;;; shared pair

(binfour %-fixnum do-fixnum)                    ;;; fixnum

(binfour %-old-pointer do-old-pointer)          ;;; old pair
(binfour %-pair-number do-pointer-number)       ;;; num of shared ptrs

(binfour %-symbol do-symbol)                    ;;; symbol
(binfour %-symbol-shared do-symbol-shared)      ;;; shared symbol
(binfour %-old-symbol do-old-symbol)            ;;; old symbols
(binfour %-symbol-number do-symbol-number)      ;;; number of shared symbols

(binfour %-string do-string)                    ;;; string
(binfour %-string-shared do-string-shared)      ;;; shared string
(binfour %-old-string do-old-string)            ;;; old string
(binfour %-string-number do-string-number)      ;;; number of shared strings

(binfour %-vector do-vector)                    ;;; vector
(binfour %-vector-shared do-vector-shared)      ;;; shared vector

(binfour %-byte-vector do-byte-vector)          ;;; byte vector
(binfour %-bytev-shared do-bytev-shared)        ;;; shared byte vector

(set (vref *func-vec* %-big-flo) do-big-flo)    ;;; bigflo
(set (vref *func-vec* %-big-flo-shared)
     do-big-flo-shared)                         ;;; shared bigflo

(set (vref *func-vec* %-bignum-p) do-bignum-p)  ;;; positive bignum
(set (vref *func-vec* %-bignum-p-shared)
     do-bignum-p-shared)                        ;;; shared positive bignum

(set (vref *func-vec* %-bignum-n) do-bignum-n)  ;;; nagetive bignum
(set (vref *func-vec* %-bignum-n-shared)
     do-bignum-n-shared)                        ;;; shared nagetive bignum
