(herald (dump dump)
        (syntax-table (block (require (hax receive))
                             &&syntax-table&&))
        (env t (dump codes)))

;;; A T-object dumper.  Li Kai's algorithm implemented by Richard Kelsey

;;; (OPEN-DUMP filespec)
;;;========================================================================
;;;   Returns an object that handle the operations WRITE and CLOSE.  The
;;; idea is to make a dump file look like any other to the user.  Dumped
;;; objects are just stored in a list until the dump is closed.  Then they
;;; are written into the file.

(define (open-dump filespec . encoder)
  (let ((filename (->filename filespec))
        (things '())
        (keys (if encoder (car encoder) false))
        (accessors (if encoder (cadr encoder) false)))
    (object nil
      ((print self stream)
       (format stream "#{Dumper~_~S~_~S}" (object-hash self) filename))
      ((write self thing)
       (push things thing)
       t)
      ((close self)
       (dump filename (reverse! things) keys accessors)))))


;;; These structures are used to hold a couple of indices.  It's just a
;;; double-decker locative.

(define-structure-type dump-status
  object-id    ; Number of shared objects found so far
  string-id    ; Number of shared strings found so far
  )

(let ((ds (stype-master dump-status-stype)))
  (set (dump-status-object-id ds) 0)
  (set (dump-status-string-id ds) 0))

(define (reset-status ds)
  (set (dump-status-object-id ds) 0)
  (set (dump-status-string-id ds) 0)
  t)

;;; The top level procedure.  Walks the dumped objects first to find any
;;; that occur more than once.  The numbers of duplicated objects are written
;;; as the first two objects in the file. The only reason for the first pass
;;; is so that the retriever can make the right sized vectors to hold the
;;; duplicated objects.  Finally, the objects are actually dumped.

(define (dump filename things keys accessors)
  (with-open-streams ((out (open filename '(out))))
    (let ((status (make-dump-status))
          (objects (make-table 'dumped-objects))
          (strings (make-string-table 'dumped-strings)))
      (walk (lambda (thing)
               (find-duplicates thing status objects strings keys accessors))
            things)
      (dump-small-object (dump-status-object-id status) out)
      (dump-small-object (dump-status-string-id status) out)
      (reset-status status)
      (walk (lambda (thing)
               (dump-out thing out status objects strings keys accessors))
            things)
      t)))

;;; Put all non-immediate objects into tables so that duplicates can be found.
;;; When a duplicate is found the current index of the proper type is stored
;;; in the table to be used as the encoding of the copy.  The index is stored
;;; as a negative integer.

(define (find-duplicates thing status objects strings keys accessors)
  (iterate loop ((thing thing))
    (cond ((small-object? thing)
           nil)
          ((string? thing)
           (let ((tag (string-table-entry strings thing)))
             (cond ((null? tag)
                    (set (string-table-entry strings thing) 'one))
                   ((eq? tag 'one)
                    (set (string-table-entry strings thing) 'many)
                    (increment (dump-status-string-id status))))))
          ((pair? thing)
           (cond ((add-to-table thing status objects)
                  (loop (car thing))
                  (loop (cdr thing)))))
          ((vector? thing)
           (cond ((add-to-table thing status objects)
                  (do ((i 0 (fx+ 1 i)))
                      ((fx>= i (vector-length thing)))
                      (loop (vref thing i))))))
          ((or (symbol? thing)
               (bytev? thing)
               (float? thing)
               (bignum? thing))
           (add-to-table thing status objects))
          ((keys thing)
           =>(lambda (key)
                (cond ((add-to-table thing status objects)
                       (loop key)
                       (walk (lambda (proc)
                               (loop (proc thing)))
                             (accessors key))))))
          (else
           (error "don't know how to dump ~S" thing)))))

(define (add-to-table thing status table)
  (let ((tag (table-entry table thing)))
    (cond ((null? tag)
           (set (table-entry table thing) 'one)
           t)
          ((eq? tag 'one)
           (set (table-entry table thing) 'many)
           (increment (dump-status-object-id status))
           nil)
          (else
           nil))))

;;; Dumps the objects into the file by calling the appropriate routine for
;;; each.

(define (dump-out thing out status objects strings keys accessors)
  (iterate loop ((thing thing))
    (cond ((small-object? thing)
           (dump-small-object thing out))
          ((string? thing)
           (dump-string thing out status strings))
          ((pair? thing)
           (cond ((dump-object thing out status objects)
                  (loop (car thing))
                  (loop (cdr thing)))))
          ((vector? thing)
           (cond ((dump-object thing out status objects)
                  (do ((i 0 (fx+ 1 i)))
                      ((fx>= i (vector-length thing)))
                    (loop (vref thing i))))))
          ((or (symbol? thing)
               (bytev? thing)
               (float? thing)
               (bignum? thing))
           (dump-object thing out status objects))
          ((keys thing)
           =>(lambda (key)
                (cond ((dump-object thing out status objects)
                       (loop key)
                       (walk (lambda (proc)
                               (loop (proc thing)))
                             (accessors key))))))
          (else
           (error "don't know how to dump ~S" thing)))))

;;; The immediate types of object.

(define (small-object? thing)
  (or (null? thing)
      (fixnum? thing)
      (char? thing)
      (eq? thing '#t)))

(define (dump-small-object thing out)
  (cond ((null? thing)
         (dump-byte out dump/null))
        ((char? thing)
         (dump-byte out dump/char)
         (dump-byte out (char->ascii thing)))
        ((eq? thing '#t)
         (dump-byte out dump/true))
        ((not (fixnum? thing))
         (error "dump internal error, ~S is not a small object" thing))
        ((fx> 0 thing)
         (dump-unshared-code&size out dump/negative-fixnum (fx- 0 thing)))
        (else
         (dump-unshared-code&size out dump/positive-fixnum thing))))

;;; Dumping strings.  A table entry of 'ONE means that their is only one copy
;;; of the string and it can just be dumped.  'MANY means that this
;;; is the first of several copies.  The string is dumped with the 'shared'
;;; bit set and the table entry is set to the next string index.  Any other
;;; entry is an index which is dumped with the STRING-REF type.

(define (dump-string string out status strings)
  (let ((tag (string-table-entry strings string)))
    (cond ((eq? tag 'one)
           (dump-code&size out dump/string nil (string-length string))
           (writes out string))
          ((eq? tag 'many)
           (let ((index (increment (dump-status-string-id status))))
             (set (string-table-entry strings string) index)
             (dump-code&size out dump/string t (string-length string))
             (writes out string)))
          (else
           (dump-unshared-code&size out dump/string-ref tag)))))

;;; This is the same as for strings.

(define (dump-object thing out status objects)
  (let ((tag (table-entry objects thing)))
    (cond ((eq? tag 'one)
           (dump-data out thing nil)
           t)
          ((eq? tag 'many)
           (let ((index (increment (dump-status-object-id status))))
             (set (table-entry objects thing) index)
             (dump-data out thing t)
             t))
          (else
           (dump-unshared-code&size out dump/object-ref tag)
           nil))))

;;; Dump whatever non-pointer data an object may have.

(define (dump-data out thing shared)
  (cond ((symbol? thing)
         (let ((str (symbol-pname thing)))
           (dump-code&size out dump/symbol shared (string-length str))
           (writes out str)))
        ((pair? thing)
         (dump-code out dump/pair shared))
        ((vector? thing)
         (dump-code&size out dump/vector shared (vector-length thing)))
        ((bytev? thing)
         (dump-code&size out dump/byte-vector shared (bytev-length thing))
         (do ((i 0 (fx+ 1 i)))
             ((fx>= i (bytev-length thing)))
           (dump-byte out (bref-8 thing i))))
        ((integer? thing)
         (let ((num-list (do ((val (abs thing) (div val 65536))
                              (res '() (cons (remainder val 65536) res)))
                             ((=0? val)
                              res)))
               (code (if (> 0 thing)
                         dump/negative-bignum
                         dump/positive-bignum)))
           (dump-code&size out code shared (length num-list))
           (walk (lambda (num)
                   (dump-bytes out num 2))
                 num-list)))
        ((float? thing)
         (dump-code out dump/float shared)
         (flonum-guts thing
                      (lambda (p0 p1 p2 p3)
                        (dump-bytes out p0 2)
                        (dump-bytes out p1 2)
                        (dump-bytes out p2 2)
                        (dump-bytes out p3 2))))
        (else
         (dump-code out dump/coded shared))))

;;; Write out a code that has no size field.

(define (dump-code out code shared)
  (dump-byte out (if shared (fx+ 1 code) code)))

;;; Write a code that has a size but no 'shared' field.

(define (dump-unshared-code&size out code num)
  (cond ((fx< num 0)
         (error "dump internal error, can't encode negative fixnum ~S" num))
        ((fx< num 256)
         (dump-byte out code)
         (dump-byte out num))
        ((fx< num 65536)
         (dump-byte out (fx+ 1 code))
         (dump-bytes out num 2))
        ((fx< num 16777216)
         (dump-byte out (fx+ 2 code))
         (dump-bytes out num 3))
        (else                           ; Should check here.
         (dump-byte out (fx+ 3 code))
         (dump-bytes out num 4))))

;;; Write a code that both size and 'shared' fields.

(define (dump-code&size out code shared num)
  (let ((code (if shared (fx+ 1 code) code)))
    (cond ((fx< num 0)
           (error "dump internal error, can't encode negative fixnum ~S" num))
          ((fx< num 256)
           (dump-byte out code)
           (dump-byte out num))
          ((fx< num 65536)
           (dump-byte out (fx+ 2 code))
           (dump-bytes out num 2))
          ((fx< num 16777216)
           (dump-byte out (fx+ 4 code))
           (dump-bytes out num 3))
          (else                           ; Should check here.
           (dump-byte out (fx+ 6 code))
           (dump-bytes out num 4)))))

;;; Write out various numbers of bytes.

(define (dump-byte out byte)
  (writec out (ascii->char (fixnum-logand byte #xFF))))

(define (dump-bytes out num count)
  (do ((i 0 (fx+ 8 i)))
      ((fx>= i (fx* count 8)))
    (dump-byte out (fixnum-ashr num i))))
