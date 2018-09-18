(Herald Encode (env t ))

;;; (WRITE2C dump-stream word)
;;;=========================================================================
;;; Write the lowest byte first, then the second lowest byte.
;;;

(define-operation (write2c dump-stream word))


;;; (WRITE3C dump-stream word)
;;;=========================================================================
;;; Write the lowest byte first, ...
;;;

(define-operation (write3c dump-stream word))


;;; (WRITE4C dump-stream word)
;;;========================================================================
;;; Write the lowest byte first, ...
;;;

(define-operation (write4c dump-stream word))

;;; (write-token-word dump-stream token-type token-id)
;;;========================================================================
;;; This is a for writing old token id to refer to the previous token.
;;; The coding of the token-type makes the function general for three types.
;;; Probably more later.
;;; If the token-id < 1 byte, then <token-type> and <token-id> (1byte)
;;; If the token-id < 2 bytes, then <token-type> and <token-id> (2byte)
;;; If the token-id < 3 bytes, then <token-type> and <token-id> (3byte)
;;; If the token-id < 4 bytes, then <token-type> and <token-id> (4byte)
;;;

(define-operation (write-token-word dump-stream token-type token-id))


;;; (BUILD-HASH-MAPS dump-stream s-expr)
;;;=========================================================================
;;; This function takes any kind of s-expression and find all the shared
;;; things like shared-pairs, shared-symbols, and shared-strings.
;;; It build a hash map for all the shared things.  Right now there are
;;; three maps:
;;;
;;;     pair-map   = { <integer, frequence-count> }
;;;     symbol-map = { <string, frequence-count> }
;;;     string-map = { <string, frequence-count> }
;;;
;;; where the frequence-count has a negative value.
;;;

(define-operation (build-hash-maps dump-stream s-expr))

(define-operation (dump dump-stream s-expr))


;;; (ENCODE dump-stream s-expr)
;;;========================================================================
;;; This function will dump an s-expression into a stream in
;;; in the following encoding format:
;;;
;;;    Token    -- The smallest encoding and decoding unit.
;;;                It always starts with a token-type which needs 1 byte.
;;;                The data following the token-type needs 0 or many bytes.
;;;
;;;    Token-id -- The token serial number which identifies token in both
;;;                encoding and decoding.
;;;                This needs 32 bits fixed number, i.e., 4 bytes.
;;;
;;;    Format of the token for each data type:
;;;
;;;        ():     <token-type>
;;;
;;;        char:  <token-type> <char>
;;;                where <char> == a character in ASCII code
;;;
;;;        fixnum: <token-type> <value>
;;;                where <value> == maximum 29 bits number representation
;;;                      first byte is the lowest byte
;;;
;;;        string: <token-type> <length> <characters>
;;;                where <length> is a two byte long unsigned integer
;;;                      <characters> is a sequence of the characters in
;;;                      the string in ASCII code
;;;
;;;        symbol: <token-type> <length> <characters>
;;;                where <length> is a two byte long unsigned integer
;;;                      <characters> is a sequence of the characters in
;;;                      the string in ASCII code and always in uppercase
;;;
;;;        pair:   <token-type> <car-data> <cdr-data>
;;;                where <car-data> and <cdr-data> can be either
;;;                         . a token transmitted before; the format is
;;;                           <token-type> <token-id>
;;;                       or
;;;                         . any data token
;;;
;;;                Note that this scheme allows the transmission of circular
;;;                lists.
;;;
;;;        vector: <token-type> <length> elment0 elment1 ...
;;;                where elementi can be anything defined
;;;
;;;        flonum: <token-type> <value>
;;;                where <value> has four 2-byte fixnums.
;;;
;;;        bignum: <token-type> k, fk-1, ... , f1, f0
;;;                where
;;;                     real-value = f0 + f1 * b + ... + fk-1 * b^k-1
;;;
;;;                and b = 65536, the reverse order is for efficiency
;;;
;;;        bytev:  <token-type> N, byte1, ... , byteN
;;;
;;;

(define-operation (encode dump-stream s-expr))


(define-operation (pr dump-stream))             ;;; for debugging


;;; (CLOSE-DUMP dump-stream)
;;;=========================================================================
;;; This function does the actual encoding and writing of all
;;; the s-expressions we dumped in turn.
;;; On the decoding side, we will have to follow the dumping order to
;;; assign the s-expressions to some variables
;;;

(define-operation (close-dump dump-stream))


(define (open-dump file)
    (let* ((string-map (make-hash-map %-map-size))
           (symbol-map (make-hash-map %-map-size))
           (pointer-map (make-hash-map %-ptr-map-size))
           (string-id 0)
           (symbol-id 0)
           (pointer-id 0)
           (dumps '())
           (stream (open file '(out)))
           (write1c (pre-dispatch writec stream))
           (write1s (pre-dispatch writes stream))
           (id 0))

      (object nil

        ((write2c self word)
         (write1c stream word)
         (write1c stream (fixnum-ashr word 8)))

        ((write3c self word)
         (write1c stream word)
         (write1c stream (fixnum-ashr word 8))
         (write1c stream (fixnum-ashr word 16)))

        ((write4c self word)
         (write1c stream word)
         (write1c stream (fixnum-ashr word 8))
         (write1c stream (fixnum-ashr word 16))
         (write1c stream (fixnum-ashr word 24)))

        ((write-token-word self token-type token-id)
         (cond ((fx= (fixnum-logand (fixnum-ashr token-id 8) #x1fffff) 0)
                (write1c stream token-type)
                (write1c stream token-id))
               ((fx= (fixnum-logand (fixnum-ashr token-id 16) #x1fff) 0)
                (write1c stream (fx+ token-type 1))
                (write2c self token-id))
               ((fx= (fixnum-logand (fixnum-ashr token-id 24) #x1f) 0)
                (write1c stream (fx+ token-type 2))
                (write3c self token-id))
               (else
                (write1c stream (fx+ token-type 3))
                (write4c self token-id))))

        ((build-hash-maps self s-expr)
         (cond
          ((symbol? s-expr)                            ;;; symbol
           (set id (map-lookup-count-insert symbol-map s-expr 0))
           (if (fixnum? id)
               (if (fx< id 0)                          ;;; shared symbol
                   (set symbol-id (fx+ symbol-id 1))))
           s-expr)

          ((pair? s-expr)
           (set id (map-lookup-count-insert pointer-map s-expr 0))
           (if (fixnum? id)
               (if (fx< id 0)                          ;;; shared pair
                   (set pointer-id (fx+ pointer-id 1))))
           (build-hash-maps self (car s-expr))
           (build-hash-maps self (cdr s-expr)))

          ((string? s-expr)                            ;;; string
           (set id (map-lookup-count-insert string-map s-expr 0))
           (if (fixnum? id)
               (if (fx< id 0)                          ;;; shared string
                   (set string-id (fx+ string-id 1))))
           s-expr)

          ((or (bytev? s-expr)
               (float? s-expr)
               (bignum? s-expr))
           (set id (map-lookup-count-insert pointer-map s-expr 0))
           (if (fixnum? id)
               (if (fx< id 0)                          ;;; shared ptr
                   (set pointer-id (fx+ pointer-id 1))))
           s-expr)

          ((vector? s-expr)
           (set id (map-lookup-count-insert pointer-map s-expr 0))
           (if (fixnum? id)
               (if (fx< id 0)                          ;;; shared ptr
                   (set pointer-id (fx+ pointer-id 1))))

           (loop (incr i from 0 to (fx- (vector-length s-expr) 1))
                 (do (build-hash-maps self (vref s-expr i)))))

          (else                                        ;;; other types
           s-expr)))

        ((dump self s-expr)
         (build-hash-maps self s-expr)
         (set dumps (cons s-expr dumps)))

        ((pr self)
         (map-print symbol-map)
         (map-print string-map)
         (map-print pointer-map)
         (format t "~&ptr ~d sym ~d str ~d" pointer-id symbol-id string-id))

        ((encode self s-expr)
         (cond
          ((null? s-expr)                     ;;; null expression
           (write1c stream %-null))

          ((char? s-expr)                     ;;; character
           (write1c stream %-char)
           (write1c stream s-expr))

          ((fixnum? s-expr)                   ;;; fixnum
           (write-token-word self %-fixnum s-expr))

          ((pair? s-expr)                             ;;; pair
           (set id (map-lookup-replace pointer-map
                                       s-expr
                                       (fx+ pointer-id 1)
                                       (lambda (x) (fx< x 0))))
           (cond ((fx= id 0)                     ;;; non-shared pair
                  (write1c stream %-pair)
                  (encode self (car s-expr))
                  (encode self (cdr s-expr)))

                 ((fx< id 0)                     ;;; new shared pair
                  (set pointer-id (fx+ pointer-id 1))
                  (write1c stream %-pair-shared)
                  (encode self (car s-expr))
                  (encode self (cdr s-expr)))

                 (else                                ;;; old shared
                  (write-token-word self %-old-pointer id))))

          ((string? s-expr)                   ;;; string
           (set id (map-lookup-replace string-map
                                       s-expr
                                       (fx+ string-id 1)
                                       (lambda (x) (fx< x 0))))
           (cond ((fx= id 0)                  ;;; non-shared symbol
                  (write-token-word self
                                    %-string
                                    (string-length s-expr))
                  (write1s stream s-expr))

                 ((fx< id 0)                     ;;; new shared symbol
                  (set string-id (fx+ string-id 1))
                  (write-token-word self
                                    %-string-shared
                                    (string-length s-expr))
                  (write1s stream s-expr))

                 (else
                  (write-token-word self %-old-string id))))

          ((symbol? s-expr)                   ;;; symbol
           (let ((temp-str (symbol-pname s-expr)))
             (set id (map-lookup-replace symbol-map
                                         s-expr
                                         (fx+ symbol-id 1)
                                         (lambda (x) (fx< x 0))))
             (cond ((fx= id 0)                  ;;; non-shared symbol
                    (write-token-word self
                                      %-symbol
                                      (string-length temp-str))
                    (write1s stream temp-str))

                   ((fx< id 0)                     ;;; new shared symbol
                    (set symbol-id (fx+ symbol-id 1))
                    (write-token-word self
                                      %-symbol-shared
                                      (string-length temp-str))
                    (write1s stream temp-str))

                   (else
                    (write-token-word self %-old-symbol id)))))

          ((vector? s-expr)                       ;;; vector
           (let ((len (vector-length s-expr)))
             (set id (map-lookup-replace pointer-map
                                         s-expr
                                         (fx+ pointer-id 1)
                                         (lambda (x) (fx< x 0))))
             (cond ((fx= id 0)                      ;;; non-shared
                    (write-token-word self %-vector len)
                    (loop (incr i from 0 to (fx- len 1))
                          (do (encode self (vref s-expr i)))))

                   ((fx< id 0)                     ;;; new shared vec
                    (set pointer-id (fx+ pointer-id 1))
                    (write-token-word self %-vector-shared len)
                    (loop (incr i from 0 to (fx- len 1))
                          (do (encode self (vref s-expr i)))))

                   (else                                ;;; old shared
                    (write-token-word self %-old-pointer id)))))

          ((bytev? s-expr)                        ;;; byte vector
           (let ((len (bytev-length s-expr)))
             (set id (map-lookup-replace pointer-map
                                         s-expr
                                         (fx+ pointer-id 1)
                                         (lambda (x) (fx< x 0))))
             (cond ((fx= id 0)                      ;;; non-shared
                    (write-token-word self %-byte-vector len)
                    (loop (incr i from 0 to (fx- len 1))
                          (do (write1c stream (bref-8 s-expr i)))))

                   ((fx< id 0)                     ;;; new shared bv
                    (set pointer-id (fx+ pointer-id 1))
                    (write-token-word self %-bytev-shared len)
                    (loop (incr i from 0 to (fx- len 1))
                          (do (write1c stream (bref-8 s-expr i)))))

                   (else                                ;;; old shared
                    (write-token-word self %-old-pointer id)))))


          ((bignum? s-expr)                       ;;; bignum
           (let ((num-list '()))
             (loop (initial (value (abs s-expr))
                            (quo 0))
                   (do (set quo (div value 65536))
                       (push num-list (- value (* quo 65536)))
                       (set value quo))
                   (until (=0? quo)))

             (set id (map-lookup-replace pointer-map
                                         s-expr
                                         (fx+ pointer-id 1)
                                         (lambda (x) (fx< x 0))))
             (cond ((fx= id 0)                     ;;; non-shared
                    (if (positive? s-expr)
                        (write1c stream %-bignum-p)
                        (write1c stream %-bignum-n))
                    (write1c stream (length num-list))

                    (loop (while num-list)
                          (do (write2c self (car num-list))
                              (set num-list (cdr num-list)))))

                   ((fx< id 0)                       ;;; new shared
                    (set pointer-id (fx+ pointer-id 1))
                    (if (positive? s-expr)
                        (write1c stream %-bignum-p-shared)
                        (write1c stream %-bignum-n-shared))
                    (write1c stream (length num-list))

                    (loop (while num-list)
                          (do (write2c self (car num-list))
                              (set num-list (cdr num-list)))))

                   (else
                    (write-token-word self %-old-pointer id)))))

          ((float? s-expr)                        ;;; flonum
           (set id (map-lookup-replace pointer-map
                                       s-expr
                                       (fx+ pointer-id 1)
                                       (lambda (x) (fx< x 0))))
           (cond ((fx= id 0)                     ;;; non-shared
                  (write1c stream %-big-flo)
                  (flonum-guts s-expr
                           (lambda (p1 p2 p3 p4)
                             (write2c self p1)
                             (write2c self p2)
                             (write2c self p3)
                             (write2c self p4))))

                 ((fx< id 0)                       ;;; new shared
                  (set pointer-id (fx+ pointer-id 1))
                  (write1c stream %-big-flo-shared)
                  (flonum-guts s-expr
                           (lambda (p1 p2 p3 p4)
                             (write2c self p1)
                             (write2c self p2)
                             (write2c self p3)
                             (write2c self p4))))

                 (else
                  (write-token-word self %-old-pointer id))))

          (else
           (format t "~&tag: ~d" (pointer-tag s-expr))
           (format t "~&This type has not been implemented yet."))))

        ((close-dump self)
         (write-token-word self
                           %-pair-number (fx+ pointer-id 1))
         (write-token-word self
                           %-string-number (fx+ string-id 1))
         (write-token-word self
                           %-symbol-number (fx+ symbol-id 1))

         (set pointer-id 0)
         (set string-id 0)
         (set symbol-id 0)

         (set dumps (reverse! dumps))
         (for (x in dumps)
              (do (encode self x)))

         (map-clear symbol-map)        ;;; These are for garbage collection
         (map-clear string-map)
         (map-clear pointer-map)

         (close stream)))))
