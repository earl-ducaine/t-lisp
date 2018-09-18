(HERALD VMSLOAD
        (ENV TSYS (TSYS VMSIO)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Object file loader

;;; This is the machine-dependent portion of FASLOAD, for VAX/VMS.
;;; Mostly this just punts to appropriate BLISS code; see assist module(s).

;;; Bogus version of OBJECT-FILE? predicate.  Just looks at filename.
;;; Needs improvement. What about generation numbers?  Should use LIB$FILE_SCAN.

(define (object-file? spec)
  (let* ((filename (->filename spec))
         (type (filename-type filename))
         (name (filename-name filename)))
    (or (eq? type 'bin)
        (and (string? type)
             (string-equal? (string-upcase type) "bin"))
        (and (null? type)
             (string? name)
             (fx>= (string-length name) 4)
             (let ((s (nthchdr name
                               (fx- (string-length name) 4))))
               (string-equal? (string-upcase s) ".bin"))))))

(define (object-file-stream? stream)
  (let ((fn (stream-filename stream)))
    (and fn (object-file? fn))))

(define (load-space-exhausted? code-size unit-size)
  (let ((margin (fx+ code-size (fx+ unit-size (fx/ unit-size 4)))))
    (not (ensure-heap-space? margin))))


;;; Given open stream, return unit.
(define (load-raw-unit stream)
  (let ((chan    (stream->channel stream))
        (head    (make-vector 3)))
      (vms-i/o *channel-load-xenoid* (chan-fab chan) 12 head)
      (let ((code-size (vref head 1))
            (unit-size (pointer-ashl (vref head 2) 1)))
        (cond ((fx= (vref head 0) %%precooked)
               (error "you're trying to fasload a precooked file"))
              ((not (fx= (vref head 0) %%correct-fasl))
               (error "this doesn't look like a fasl file to me"))
              ((load-space-exhausted? code-size unit-size))
              (else
               ;; there should be a better way to do raw allocation
               (let ((code (make-code code-size))
                     (unit (make-extend-n 0 unit-size)))
                 (vms-i/o *channel-load-xenoid* (chan-fab chan) code-size code)
                 (vms-i/o *channel-load-xenoid*
                          (chan-fab chan) unit-size (pointer-address unit))
                 (set-extend-template unit *unit-template*)
                 (set-unit-code unit code)
                 unit))))))
