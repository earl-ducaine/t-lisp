(herald (dump retrieve)
	(syntax-table (block (require (hax receive))
			     &&syntax-table&&))
	(env t (dump codes)))

;;; Retrieving dumped objects.
;;;   See dump.doc, codes.t, dump.t as well.

;;; This is a vector containing a procedure to decode each possible type byte.
;;; Initially it is filled with an error routine.

(block
  (define *decode-dispatch-vec* (make-vector 256))

  (vector-fill *decode-dispatch-vec*
               (lambda (code in)
                 (ignore in)
                 (error "retrieve got an unknown type code ~S" code)))
  t)

;;; Puts the proper procedure into the dispatch vector.  There are four
;;; decoding procedures.

(define (add-dispatch code shared? data?)
  (receive (proc count)
           (cond ((and shared? data?)
                  (values decode-shared&data 8))
                 (data?
                  (values decode-data 4))
                 (shared?
                  (values decode-shared 2))
                 (else
                  (values decode-plain 1)))
    (do ((i code (fx+ 1 i)))
        ((fx>= i (fx+ code count)))
      (set (vref *decode-dispatch-vec* i) proc))))

;;; These are the four type-byte decoders.  They just extract the proper fields
;;; and return them along with the normalized type code.

(define (decode-plain code in)
  (ignore in)
  (values code nil nil))

(define (decode-shared code in)
  (ignore in)
  (values (fixnum-logand code #xFE) (fixnum-odd? code) nil))

(define (decode-data code in)
  (let ((data (fixnum-logand code #x3)))
    (values (fixnum-logand code #xFC) nil (get-bytes in (fx+ 1 data)))))

(define (decode-shared&data code in)
  (let ((data (fixnum-ashr (fixnum-logand code #x6) 1)))
    (values (fixnum-logand code #xF8)
            (fixnum-odd? code)
            (get-bytes in (fx+ 1 data)))))

;;; All of the handled types.
;;;             TYPE             SHARED?    SIZE FIELD?
(add-dispatch dump/null            nil         nil)
(add-dispatch dump/char            nil         nil)
(add-dispatch dump/true            nil         nil)
(add-dispatch dump/pair            t           nil)
(add-dispatch dump/float           t           nil)
(add-dispatch dump/coded           t           nil)
(add-dispatch dump/object-ref      nil         t)
(add-dispatch dump/string-ref      nil         t)
(add-dispatch dump/positive-fixnum nil         t)
(add-dispatch dump/negative-fixnum nil         t)
(add-dispatch dump/string          t           t)
(add-dispatch dump/symbol          t           t)
(add-dispatch dump/vector          t           t)
(add-dispatch dump/byte-vector     t           t)
(add-dispatch dump/positive-bignum t           t)
(add-dispatch dump/negative-bignum t           t)

;;; Opens a dumped file, reads in the counts of shared objects, makes vectors
;;; to hold the shared objects, and returns an object for the stream.

(define (open-retrieve filespec . decoder)
  (let* ((filename (->filename filespec))
         (status (make-dump-status))
         (in (open filename '(in)))
         (objects
	  (make-vector (fx+ 1 (retrieve-object in nil nil nil nil nil))))
         (strings
	  (make-vector (fx+ 1 (retrieve-object in nil nil nil nil nil))))
         (makers (if decoder (car decoder) false))
         (accessors (if decoder (cadr decoder) false)))
    (object nil
      ((print self stream)
       (format stream "#{Retriever~_~S~_~S}" (object-hash self) filename))
      ((read self)
       (retrieve-object in status objects strings makers accessors))
      ((close self)
       (close in)))))

;;; Read in and decode the next type byte.  Checks for end-of-file.

(define (get-next-code in)
  (let ((char (readc in)))
    (if (eof? char)
        (values *eof* nil nil)
        ((vref *decode-dispatch-vec* (char->ascii char))
           (char->ascii char)
           in))))

;;; Retrieves the next object.  This routine does *EOF* checking and adds
;;; shared objects to the vectors.  Pairs and vectors must be checked for
;;; sharing before their fields are retrieved so that circular ones will
;;; be reconstructed properly.

(define (retrieve-object in status objects strings makers accessors)
  (iterate next ()
    (receive (code shared? data)
             (get-next-code in)
      (if (eof? code)
          *eof*
          (receive (thing key)
                   (select code
                     ((dump/object-ref) (values (vref objects data) nil))
                     ((dump/string-ref) (values (vref strings data) nil))
                     ((dump/coded) (let* ((key (next))
                                          (maker (makers key)))
                                     (if (not maker)
                                         (error "no maker for key ~S" key)
                                         (values (maker) key))))
                     (else
                      (values (get-next-object in code data) nil)))
            (cond ((and shared? (fx= code dump/string))
                   (vset strings
                         (modify (dump-status-string-id status)
				 (lambda (x) (fx+ x 1)))
                         thing))
                  (shared?
                   (vset objects
                         (modify (dump-status-object-id status)
				 (lambda (x) (fx+ x 1)))
                         thing)))
            (select code
              ((dump/pair)
               (set (car thing) (next))
               (set (cdr thing) (next)))
              ((dump/vector)
               (do ((i 0 (fx+ 1 i)))
                   ((fx>= i data))
                 (set (vref thing i) (next))))
              ((dump/coded)
               (let ((procs (accessors key)))
                 (if (not procs)
                     (error "no accessors for key ~S" key)
                     (walk (lambda (proc)
                             (set (proc thing) (next)))
                           procs)))))
            thing)))))

;;; Actually retrieves the next object.  Dispatches on the type code.

(define (get-next-object in code data)
  (select code
    ((dump/null)            nil)
    ((dump/true)            '#t)
    ((dump/char)            (readc in))
    ((dump/positive-fixnum) data)
    ((dump/negative-fixnum) (fx- 0 data))
    ((dump/pair)            (cons nil nil))
    ((dump/vector)          (make-vector data))
    ((dump/string)          (get-string in data))
    ((dump/symbol)          (string->symbol (get-string in data)))
    ((dump/float)           (get-float in))
    ((dump/byte-vector)     (get-bytev in data))
    ((dump/positive-bignum) (get-bignum in data))
    ((dump/negative-bignum) (- (get-bignum in data)))
    (else
     (error "retrieve got an unknown type code ~S" code))))

;;; Routines to reconstruct the various types.

(define (get-string in size)
  (map-string! (lambda (ch)
                 (ignore ch)
                 (readc in))
               (make-string size)))

(define (get-bytev in size)
  (let ((bytev (make-bytev size)))
    (do ((i 0 (fx+ 1 i)))
        ((fx>= i size) bytev)
      (set (bref-8 bytev i) (get-byte in)))))

(define (get-float in)
  (let* ((p1 (get-two-bytes in))
         (p2 (get-two-bytes in))
         (p3 (get-two-bytes in))
         (p4 (get-two-bytes in)))
    (fixnums-replace-flonum (fl+ 0.0 (no-op 0.0)) p1 p2 p3 p4)))

(define (get-bignum in size)
  (do ((num 0 (+ (get-two-bytes in)
                 (* num 65536)))
       (i 0 (fx+ 1 i)))
      ((fx>= i size)
       num)))

;;; Read in various numbers of bytes.

(define (get-byte in)
  (char->ascii (readc in)))

(define (get-two-bytes in)
  (let ((value (get-byte in)))
    (fixnum-logior (fixnum-ashl (get-byte in) 8)
                   value)))

(define (get-bytes in count)
  (let ((end (fx* 8 count)))
    (do ((i 0 (fx+ 8 i))
         (val 0 (fixnum-logior (fixnum-ashl (get-byte in) i)
                               val)))
        ((fx>= i end)
         val))))
