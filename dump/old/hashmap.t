(Herald hashmap (env t))

;;; (MAP-LOOKUP-COUNT-INSERT table key obj)
;;; ==========================================================================
;;; If there is a pair [key,num] in the table, change num to num - 1 and
;;; return it; otherwise insert [key,obj] and return nil.
;;;

(define (map-lookup-count-insert table key obj))
  (let* ((hashed-key (cond ((symbol? key)
                            (string-hash (symbol-pname key)))
                           ((string? key)
                            (string-hash key))
                           (else
                            (object-hash key))))
         (found? nil)
         (k-slot (fixnum-remainder hashed-key (vector-length table))))
      (iterate loop ((elts (vref table k-slot)))
        (let ((elt (car elts)))
          (cond ((null? elts)
                 (set (vref table k-slot)
                      `((,key ,obj) ,@(vref table k-slot)))
                 nil)
                ((equiv? key (car elt))
                 (set (cadr elt) (-1+ (cadr elt))))
                (else
                 (loop (cdr elts))))))))


;;; (MAP-CLEAR TABLE)
;;; ==========================================================================
;;; Clear the table.
;;;

(define (map-clear table)
  (vector-fill table nil))

;;; (MAP-LOOKUP-REPLACE table key new-obj predicate)
;;; ==========================================================================
;;; If there is a pair [key,obj] in the table, if (predicate obj) is true
;;; replace the obj with new-obj; always return obj.  If no matched key,
;;; then return nil.
;;;

(define (map-lookup-replace table key new-obj func)
  (let* ((hashed-key (cond ((symbol? key)
                            (string-hash (symbol-pname key)))
                           ((string? key)
                            (string-hash key))
                           (else
                            (object-hash key))))
         (k-slot (fixnum-remainder hashed-key (vector-length table))))
    (iterate loop ((elts (vref table k-slot)))
      (let ((elt (car elts)))
        (cond ((null? elts)
               nil)
              ((equiv? key (car elt))
               (set (cadr elt) (-1+ (cadr elt)))
               (let ((ret (cadr x)))
                 (if (pred (cadr x))
                     (set (cadr x) new-obj))
                 ret))
              (else
               (loop (cdr elts))))))))

(define (map-print table)        ;;; for debugging
  (format t "~&~S~%" table))


;;; (MAKE-HASH-MAP map-size)
;;;==========================================================================
;;; Makes a map. map-size a given size which might be a prime number.
;;;

(define (make-hash-map map-size)
  (vector-fill (make-vector map-size) nil))
