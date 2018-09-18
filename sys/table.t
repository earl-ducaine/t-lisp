(herald (tsys table t 33)
        (env tsys (tsys hash)))     ; for fxrem, pointer-hash

;;; Copyright (c) 1984 Yale University

;;;; Hash tables

;(define-integrable fxrem fixnum-remainder)

;(define-integrable (pointer-hash p)
;  (fixnum-abs (pointer->fixnum p)))

(define-structure-type xtable
  id            ; Identification
  count         ; Number of entries
  entries       ; Vector or list of entries
  stamp         ; GC timestamp
  limit         ; Expand and rehash when this limit is reached
  )

(define-methods handle-xtable
  ((print table stream)
   (format stream "#{Table~_~S~_~S}"
           (object-hash table)
           (xtable-id table)))
  ((identification table) (xtable-id table))
  ((set-identification table id)
   (if (not (xtable-id table))
       (set (xtable-id table) id))))

(let ((table (stype-master xtable-stype)))
  (set (xtable-id table) nil)
  (set (xtable-count table) 0)
  (set (xtable-entries table) '()))

(define-constant *table-vector-threshold* 10)

(define (make-table . maybe-id)
  (let ((table (make-xtable)))
    (if (not (null? maybe-id))
	(set (xtable-id table) (car maybe-id)))
    table))

(define table? xtable?)

(define table-entry
  (object (lambda (table obj)
            (let ((e (xtable-entries table)))
              (cond ((list? e)
                     (cond ((%assq obj e) => cdr)
                           (else nil)))
                    ;; e is a vector.
                    ((vector? e)
                     (let ((e (cond ((fxn= (xtable-stamp table) *gc-count*)
                                     (table-rehash table)
                                     (xtable-entries table))
                                    (else e))))
                       (cond ((%assq obj
                                     (vref e (fxrem (pointer-hash obj)
                                                    (vector-length e))))
                              => cdr)
                             (else nil))))
		    ((table? table)
		     (error "corrupt table~%  ~S"
			    `(table-entry ,table ,obj)))
		    (else
		     (table-entry (*check-arg table? table table-entry)
				  obj)))))
          ((setter self) set-table-entry)))

(define (set-table-entry table obj value)
  (cond (value
         (really-set-table-entry table obj value))
        (else
         (remove-table-entry table obj))))

(define (really-set-table-entry table obj value)
  (let ((e (xtable-entries table)))
    (cond ((list? e)
           (cond ((%assq obj e)
                  => (lambda (z) (set (cdr z) value)))
                 (else
                  (cond ((fx= (xtable-count table)
                              *table-vector-threshold*)
                         (table-expand-from-list table)
                         (really-set-table-entry table obj value))
                        (else
                         (modify (xtable-count table)
                                 (lambda (c) (fx+ c 1)))
                         (push (xtable-entries table)
                               (cons obj value))
                         value)))))
          ((fxn= (xtable-stamp table) *gc-count*)
           (table-rehash table)
           (really-set-table-entry table obj value))
          (else
           (let* ((index (fxrem (pointer-hash obj) (vector-length e)))
                  (l (vref e index)))
             (cond ((%assq obj l)
                    => (lambda (z) (set (cdr z) value)))
                   ((fx= (xtable-count table)
                         (xtable-limit table))
                    (table-rehash table)
                    (really-set-table-entry table obj value))
                   (else
                    (modify (xtable-count table)
                            (lambda (c) (fx+ c 1)))
                    (set (vref e index)
                         (cons (cons obj value) l))
                    value)))))))

;;; Ought to be decrementing the table count if there was an old entry.
;;; Easier not to; doesn't matter that much anyhow, for now.

(define (remove-table-entry table obj)
  (cond ((table-entry table obj)
         (modify (xtable-count table) (lambda (c) (fx- c 1)))
         (let ((e (xtable-entries table)))
           ;; Delete
           (cond ((list? e)
                  (set (xtable-entries table)
                       (del! (lambda (x y) (eq? x (car y))) obj e)))
                 (else
                  (modify (vref e (fxrem (pointer-hash obj) (vector-length e)))
                          (lambda (l)
                            (del! (lambda (x y) (eq? x (car y))) obj l)))))))))

;;; Change representation from list to vector.

(define (table-expand-from-list table)
  (let* ((e (xtable-entries table))
         (new-size 11)      ; arbitrary
         (new (make-vector new-size)))
    (vector-fill new '())
    (set (xtable-entries table) new)
    (set (xtable-limit table) (fixnum-ashl new-size 2))
    (set (xtable-stamp table) *gc-count*)
    (do ((l e (cdr l)))
        ((null? l))
      (let ((z (car l)))
        (push (vref new (fxrem (pointer-hash (car z)) new-size))
              z)))))

;;; Rehash everything, either because of a GC or because of a change
;;; in the size of the vector.

(define (table-rehash table)
  (let* ((table (check-arg table? table table-rehash))
	 (e (xtable-entries table))
         (size (vector-length e))
         ;; Increase table size, if necessary.
         (new-size (if (fx>= (xtable-count table) (xtable-limit table))
                       (fx- (fixnum-ashl size 1) 1)
                       size))
         (new (make-vector new-size)))
    (vector-fill new '())
    (set (xtable-entries table) new)
    (set (xtable-limit table) (fixnum-ashl new-size 2)) ;ave chain length = 4?
    (set (xtable-stamp table) *gc-count*)
    (do ((i 0 (fx+ i 1)))
        ((fx>= i size) table)
      (do ((l (vref e i) (cdr l)))
          ((null? l))
        (let ((z (car l)))
          (push (vref new (fxrem (pointer-hash (car z)) new-size))
                z))))))

;;; Apply proc to every (key, value) pair in table.

(define (table-walk table proc)
  (let ((e (xtable-entries table)))
    (cond ((list? e)
           (walk (lambda (z) (proc (car z) (cdr z))) e))
          ((fxn= (xtable-stamp table) *gc-count*)
           (table-rehash table)
           (walk-table table proc))
          (else
           (walk-vector (lambda (l)
                          (walk (lambda (z) (proc (car z) (cdr z))) l))
                        e)))))

(define walk-table table-walk)

;;; Return first value in table which satisfies the predicate.

(define (find-table-entry table pred)
  (let ((e (xtable-entries table)))
    (cond ((list? e)
           (iterate loop ((l e))
             (cond ((null? l) nil)
                   ((pred (caar l)) (cdar l))
                   (else (loop (cdr l))))))
          ((fxn= (xtable-stamp table) *gc-count*)
           (table-rehash table)
           (find-table-entry table pred))
          (else
           (let ((len (vector-length e)))
             (iterate outer-loop ((i 0))
               (cond ((fx>= i len) nil)
                     (else
                      (iterate loop ((l (vref e i)))
                        (cond ((null? l) (outer-loop (fx+ i 1)))
                              ((pred (caar l)) (cdar l))
                              (else (loop (cdr l)))))))))))))
