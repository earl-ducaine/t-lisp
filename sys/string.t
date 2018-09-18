(herald (tsys string t 34)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; String manipulation

;;; Because of problems with bootstrap order, the routine COPY-STRING
;;; is defined in EARLY.  The "buffer" stuff is defined in POOL.

(define (make-string size)
  (let ((n (check-arg (lambda (x)
			(and (fx>= x 0) (fx< x *string-length-limit*)))
                      size
                      make-string)))
    (%make-string n)))

(define (string-equal? s1 s2)
  (%string-equal? (check-arg string? s1 string-equal?)
                  (check-arg string? s2 string-equal?)))

(define (list->string l)
  (let ((l (check-arg list? l list->string)))
    (let ((len (length l)))
      (let ((str (make-string len)))
        (do ((i 0 (fx+ i 1))
             (l l (cdr l)))
            ((fx= i len) str)
          (set (nthchar str i) (car l)))))))

(define (string->list s)
  (let ((s (check-arg string? s string->list)))
    (do ((i (fx- (string-length s) 1) (fx- i 1))
         (l '() (cons (nthchar s i) l)))
        ((fx< i 0) l))))

(define (string-append . strings)
  (do ((l strings (cdr l))
       (n 0 (fx+ n (string-length (check-arg string? (car l) string-append)))))
      ((null? l)
       (let ((newstring (make-string n)))
         (do ((l strings (cdr l))
              (n (chopy newstring) (nthchdr! n (string-length (car l)))))
             ((null? l) newstring)
           (string-replace n (car l) (string-length (car l))))))))

(define (string-slice string start count)
  (let ((string (check-arg string? string string-slice))
        (start  (check-arg nonnegative-fixnum? start string-slice))
        (count  (check-arg nonnegative-fixnum? count string-slice)))
    (let ((new-string (nthchdr string start)))
      (cond ((fx>= (string-length new-string) count)
             (set (string-length new-string) count)
             new-string)
            (else
             (error "inconsistent arguments~
                   ~%  (~S ~S ~S ~S)"
                    'string-slice string start count))))))

;;; Does one more cons than it ought to.  Fix sometime.

(define (substring string start count)
  (copy-string (string-slice string start count)))

;;; Mappers.

(define (walk-string fn string)         ; Cf. WALK-VECTOR
  (let ((string (check-arg string? string walk-string)))
    (let ((limit (fx- (string-length string) 1)))
      (cond ((fx>= limit 0)
             (iterate loop ((i 0))
               (cond ((fx>= i limit) 
                      (fn (nthchar string i)))
                     (else
                      (fn (nthchar string i))
                      (loop (fx+ i 1))))))))))

(define (map-string proc string)
  (let ((string (check-arg string? string map-string)))
    (let ((len (string-length string)))
      (let ((new-string (%make-string len)))
        (do ((i 0 (fx+ i 1)))           ; avoid chonsing
            ((fx>= i len) new-string)
          (set (nthchar new-string i) (proc (nthchar string i))))))))

(define (map-string! fn string)
  (let ((string (check-arg string? string map-string!)))
    (let ((len (string-length string)))
      (do ((i 0 (fx+ i 1)))
          ((fx>= i len) string)
        (set (nthchar string i) (fn (nthchar string i)))))))

;;; Case stuff

(define (string-upcase string)
  (map-string char-upcase string))

(define (string-downcase string)
  (map-string char-downcase string))

(define (string-upcase! string)
  (map-string! char-upcase string))

(define (string-downcase! string)
  (map-string! char-downcase string))

;;;

(define (string-fill string ch)
  (let ((string (check-arg string? string string-fill))
        (ch (check-arg char? ch string-fill)))
    (let ((size (string-length string)))
      (do ((i 0 (fx+ i 1)))
          ((fx>= i size) string)
        (set (nthchar string i) ch)))))

(define (char->string ch)
  (let ((s (%make-string 1)))
    (set (char s) (check-arg char? ch char->string))
    s))

(define (string-posq ch string)
  (let ((ch (check-arg char? ch string-posq))
        (string (check-arg string? string string-posq)))
    (%string-posq ch string)))
