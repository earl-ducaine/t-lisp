
(define mem-size 1024)

(block (define memory (make-vector mem-size))
       t)

(define (get-tag num)
  (case (bit-field num 0 2)
    ((0) 'fix)
    ((1) 'imm)
    ((2) 'esc)
    ((3) 'pair)))

(define (change-tag num tag)
  (set-bit-field num 0 2 (case tag
                           ((fix)  0)
                           ((imm)  1)
                           ((esc)  2)
                           ((pair) 3)
                           (else
                            (error "Unknown tag ~S" tag)))))

(define mem-ref
  (object (lambda (ptr)
            (let ((addr (ash ptr -2)))
              (address-check addr)
              (vref memory addr)))
    ((setter self)
     (lambda (ptr val)
       (let ((addr (ash ptr -2)))
         (address-check addr)
         (set (vref memory addr) val))))))

(define (address-check addr)
  (if (or (> 0 addr)
          (<= mem-size addr))
      (error "Address out of range ~S" addr)))

(define index
  (object (lambda (loc num)
            (tag-esc-check loc)
            (mem-ref (+ (ash num 2) loc)))
    ((setter self)
     (lambda (loc num val)
       (tag-esc-check loc)
       (set (mem-ref (+ (ash num 2) loc)) val)))))

(define (tag-esc-check loc)
  (let ((tag (get-tag loc)))
    (if (eq? 'esc tag)
      t
      (error "Index with wrong tag ~S" loc))))

(define gc-cdr
  (object (lambda (loc)
            (tag-pair-check loc)
            (mem-ref loc))
    ((setter self)
     (lambda (loc val)
       (tag-pair-check loc)
       (set (mem-ref loc) val)))))

(define gc-car
  (object (lambda (loc)
            (tag-pair-check loc)
            (mem-ref (+ 4 loc)))
    ((setter self)
     (lambda (loc val)
       (tag-pair-check loc)
       (set (mem-ref (+ 4 loc)) val)))))

(define (tag-pair-check loc)
  (let ((tag (get-tag loc)))
    (if (eq? 'pair tag)
      t
      (error "Gc-car with wrong tag ~S" loc))))

(define (get-type num)
  (case (bit-field num 2 6)
    ((0) 'char)
    ((1) 'general-vector)
    ((2) 'bit-vector)
    ((3) 'other-vector)
    ((4 20 36) 'template)  ;;; Three different kinds of templates
    ((5) 'nil)
    (else
     (error "Unknown immediate type field in ~S" num))))

(define (change-type num type)
  (set-bit-field num 2 6 (case type
                           ((char)           0)
                           ((general-vector) 1)
                           ((bit-vector)     2)
                           ((other-vector)   3)
                           ((template)       4)
                           ((nil)            5)
                           (else
                            (error "Unknown type ~S" type)))))

(define (nil? desc)
  (and (eq? 'imm (get-tag desc))
       (eq? 'nil (get-type desc))))

(define (get-vector-size loc)
  (bit-field (mem-ref loc) 8 24))

(define (template-offset template)
  (bit-field (mem-ref (- template 4)) 2 14))

(define (template-scratch template)
  (bit-field (mem-ref (- template 4)) 16 8))

(define (template-pointers template)
  (bit-field (mem-ref (- template 4)) 24 8))

(define (template-closure template)
  (bit-field (mem-ref (- template 4)) 16 16))

(define (template-type template)
  (real-template-type (mem-ref template)))

(define (real-template-type template)
  (case (bit-field template 6 2)
    ((0) 'heap)
    ((1) 'stack)
    ((2) 'closure)
    ((3) (error "Unknown template type in ~S" template))))

(define (change-template-type num type)
  (set-bit-field num 6 2 (case type
                           ((heap)    0)
                           ((stack)   1)
                           ((closure) 2)
                           (else
                            (error "Unknown template type ~S" type)))))

(define (ptr+ ptr num)
  (+ ptr (* 4 num)))

(define (ptr- ptr num)
  (- ptr (* 4 num)))


(lset old-start 0)
(lset old-end 0)

(lset new-start 0)
(lset new-end 0)

(define (gc-init)
  (set old-start 0)
  (set old-end (-1+ (div mem-size 2)))
  (set new-start (1+ old-end))
  (set new-end new-start)
  (set k nil-desc)
  t)

(define (copy-chunk ptr size)
  (do ((old ptr (+ old 4))
       (new (make-descriptor 'esc new-end) (+ new 4))
       (count size (-1+ count)))
      ((= count 0)
       (set new-end (+ new-end size))
       (- new (* size 4)))
      (format t "~&~-4D " (ash old -2))
      (format t "=> ~-4D " (ash new -2))
      (print-desc (mem-ref old))
      (set (mem-ref new) (mem-ref old))
      (set (mem-ref old) new)))

(define (gc-size ptr)
  (case (get-type (mem-ref ptr))
    ((nil
      char)           1)
    ((general-vector
      bit-vector
      other-vector)   (1+ (get-vector-size ptr)))
    ((template)       (error "GC-size called on a template ~S" ptr))
    (else
     (error "Unknown type ~S" type))))

(define (new? ptr)
  (let ((addr (ash (mem-ref ptr) -2)))
    (and (<= new-start addr)
         (> new-end addr))))

(define (old? ptr)
  (let ((addr (ash (mem-ref ptr) -2)))
    (and (<= old-start addr)
         (> old-end addr))))

(define (general-vector? ptr)
  (eq? (get-type (mem-ref ptr)) 'general-vector))

(define (template? ptr)
  (and (eq? 'imm (get-tag (mem-ref ptr)))
       (eq? 'template (get-type (mem-ref ptr)))))

(define (print-desc desc)
  (xcase (get-tag desc)
    ((esc) (format t "Esc ~D " (bit-field desc 2 30)))
    ((pair) (format t "Pair ~D " (bit-field desc 2 30)))
    ((fix) (format t "Fix ~D " (bit-field desc 2 30)))
    ((imm)
     (xcase (get-type desc)
       ((nil)
        (format t "Nil "))
       ((char)
        (format t "Char ~A " (ascii->char (bit-field desc 8 8))))
       ((general-vector)
        (format t "Gen Vec ~D " (bit-field desc 8 24)))
       ((bit-vector)
        (format t "Bit Vec ~D " (bit-field desc 8 24)))
       ((other-vector)
        (format t "Other Vec ~D " (bit-field desc 8 24)))
       ((template)
        (format t "Template ~S " (real-template-type desc)))))))

(define (print-template num)
  (format t
          "Off ~D Ptr ~D Scr ~D"
          (bit-field num 2 14)
          (bit-field num 24 8)
          (bit-field num 16 8)))

(define (print-cell addr)
  (let ((loc (ash addr 2)))
    (format t "~&~-4D " addr)
    (print-desc (mem-ref loc))
    (if (template? loc)
        (print-template (mem-ref (- loc 4))))
    (format t "~%")
    t))

(define (mem-dump start . count)
  (cond ((null? count)
         (print-cell start))
        (else
         (do ((i start (1+ i)))
             ((>= i (+ start (car count))))
             (print-cell i)))))
