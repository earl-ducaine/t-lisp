(herald (tsys hash t 34)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Weak pointers

;;; Thanks to Gerry Sussman, Carl Hewitt, and Multics Maclisp

;;; (object-hash obj)  =>  fixnum
;;;    Generates a unique numeric id for obj.
;;;
;;; (object-unhash fixnum)  =>  obj
;;;    Returns the object which has given id, if the object hasn't
;;;    been deleted by the GC.  Returns #F if object no longer exists.
;;;
;;; (eq? a b) if and only if (= (hash a) (hash b)).
;;;
;;; Numbers returned by "object-hash" really are unique - even if
;;; the object goes away, the number won't be recycled.

(define (make-vector@ n)
  (let ((v (make-vector n)))
    (vector-fill v '())
    (change-tag v %%extend-tag %%fixnum-tag)))

(define-integrable (vector@ v)
  (change-tag v %%fixnum-tag %%extend-tag))

(block (lset   *hash-table* (make-vector@ 1021))
       (lset *unhash-table* (make-vector@ 1021))
       (lset *hash-gennum* 0)
       (lset *hash-gennum-last-gc* *hash-gennum*)
       (lset *hash-active-last-gc* 0))

(define (hashed-object-count)
  (fx+ *hash-active-last-gc* (fx- *hash-gennum* *hash-gennum-last-gc*)))

(define-integrable (pointer-hash p)
  (fixnum-abs (pointer->fixnum p)))

;;; Should keep separate hash tables for gc-copyable vs. static objects.
;;; Stars and planets?

(define (object-hash obj)
  (gc-defer
   (let* ((table (vector@ *hash-table*))
          (index (fxrem (pointer-hash obj)
                        (vector-length table)))
          (buckets (vref table index)))
     (iterate loop ((l buckets))
       (cond ((null? l)
              (let ((untable (vector@ *unhash-table*))
                    (n *hash-gennum*))
                (set *hash-gennum* (fx+ n 1))
                (let ((z (cons obj n))
                      (undex (fxrem n (vector-length untable))))
                  (set (vref table index)
                       (cons z buckets))
                  (set (vref untable undex)
                       (cons z (vref untable undex)))
                  n)))
             (else
              (let ((z (car l)))
                (cond ((eq? obj (car z))
                       (cdr z))
                      (else (loop (cdr l)))))))))))

(define *hash-nil* (object-hash nil))

(define (object-unhash n)
  (let ((n (check-arg nonnegative-fixnum? n object-unhash)))
    (gc-defer
     (let* ((untable (vector@ *unhash-table*))
            (buckets (vref untable
                           (fxrem n (vector-length untable)))))
       (iterate loop ((l buckets))
         (cond ((null? l) nil)
               (else
                (let ((z (car l)))
                  (cond ((fx= n (cdr z))
                         (car z))
                        (else (loop (cdr l))))))))))))

(define (necessarily-unhash n)
  (cond ((fx= n *hash-nil*) nil)
        ((object-unhash n))
        (else (error "(~S ~S) lost - object doesn't exist"
                     'necessarily-unhash n))))

(define (rehash-post-gc-hook)
  (let ((  old (vector@   *hash-table*))
        (unold (vector@ *unhash-table*)))
    (let ((  size (vector-length   old))
          (unsize (vector-length unold)))
      (set   *hash-table* (make-vector@   size))
      (set *unhash-table* (make-vector@ unsize))
      (let ((  new (vector@   *hash-table*))
            (unnew (vector@ *unhash-table*))
            (count 0))
        ;; Iterate through bucket lists in unhash table.
        (do ((undex 0 (fx+ undex 1)))
            ((fx= undex unsize)
             (set *hash-gennum-last-gc* *hash-gennum*)
             (set *hash-active-last-gc* count))
          ;; Rebuild one bucket list.
          (iterate loop ((old-l (vref unold undex))
                         (new-l '()))
            (cond ((null? old-l)
                   (set (vref unnew undex) new-l))
                  (else
                   (let* ((z (car old-l))
                          (obj (car z)))
                     (cond ((alive-after-gc? obj)
                            (set count (fx+ count 1))
                            (let ((new-z (new-cell)))
                              (set (cdr new-z) (cdr z))
                              (gc-copy (%loc car new-z) (car z)) ; get new addr
                              (push (vref new (fxrem (pointer-hash (car new-z))
                                                     size))
                                    new-z)
                              (loop (cdr old-l)
                                    (cons new-z new-l))))
                           (else
                            (loop (cdr old-l) new-l))))))))))))

;;; Make sure this agendum is performed first!

(push *post-gc-agenda*
      (cons 'rehash-post-gc-hook rehash-post-gc-hook))
