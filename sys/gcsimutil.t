
(define (gc addr)
  (let ((loc (make-descriptor 'esc addr))
        (new new-end))
    (move-next loc 0 loc 0)
    new))

(define (clear-mem)
  (vector-fill memory 0)
  t)

(define (dump)
  (do ((i 0 (1+ i)))
      ((>= i mem-size))
      (if (n= 0 (vref memory i))
          (mem-dump i)))
  t)

(define (make-descriptor tag field)
  (cond ((eq? tag 'imm)
         (change-tag (change-type 0 field) tag))
        (else
         (change-tag (set-bit-field 0 2 30 field) tag))))

(define (store addr desc)
  (set (mem-ref (ash addr 2)) desc))

(lset garbage-count mem-size)

(define (util-init)
  (set garbage-count mem-size))

(define (garbage)
  (make-descriptor 'esc (increment garbage-count)))

(define nil-desc
  (make-descriptor 'imm 'nil))

(define (make-fix num)
  (make-descriptor 'fix num))

(define (make-template-2 type)
  (change-tag (change-template-type (change-type 0 'template) type) 'imm))

(define (make-template-1 offset pointers scratch)
  (set-bit-field
    (set-bit-field
      (set-bit-field 0 2 14 offset) 16 8 scratch) 24 8 pointers))

(define (make-template addr type offset pointers scratch)
  (store (-1+ addr) (make-template-1 offset pointers scratch))
  (store addr (make-template-2 type)))

(define (make-general-vector start . descs)
  (let ((size (length descs)))
    (set (mem-ref (ash start 2))
         (set-bit-field (make-descriptor 'imm 'general-vector) 8 24 size))
    (do ((i (ash (1+ start) 2) (+ 4 i))
         (elts descs (cdr elts)))
        ((null? elts))
        (set (mem-ref i) (car elts)))
    (make-descriptor 'esc start)))

(define (make-closure start template . descs)
  (let ((ptrs (template-pointers (ash template 2)))
        (scrs (template-scratch  (ash template 2)))
        (type (template-type     (ash template 2))))
    (if (n= ptrs (length descs))
        (error "Wrong number of descriptors to make-closure at ~D" start))
    (set (mem-ref (ash start 2))
         (make-descriptor 'esc template))
    (do ((loc (ash (1+ start) 2) (+ 4 loc))
         (elts descs (cdr elts)))
        ((null? elts))
        (set (mem-ref loc) (car elts)))
    (if (eq? type 'heap)
        (do ((i 1 (1+ i)))
            ((> i scrs))
            (set (mem-ref (ash (+ i start ptrs) 2)) (garbage))))
    (make-descriptor 'esc start)))

(define (make-pair loc car-elt cdr-elt)
  (let ((addr (ash loc 2)))
    (set (mem-ref addr) cdr-elt)
    (set (mem-ref (+ addr 4)) car-elt)
    (make-descriptor 'pair loc)))

(define (make-char char)
  (set-bit-field (make-descriptor 'imm 'char) 8 8 (char->ascii char)))

(define (make-bit-vector start size)
  (set (mem-ref (ash start 2))
       (set-bit-field (make-descriptor 'imm 'bit-vector) 8 24 size))
  (do ((i (1+ start) (1+ i)))
      ((> i (+ start size)))
      (set (mem-ref (ash i 2)) (garbage)))
  (make-descriptor 'esc start))

(define (make-other-vector start size)
  (set (mem-ref (ash start 2))
       (set-bit-field (make-descriptor 'imm 'other-vector) 8 24 size))
  (do ((i (1+ start) (1+ i)))
      ((> i (+ start size)))
      (set (mem-ref (ash i 2)) (garbage)))
  (make-descriptor 'esc start))

(define checked (make-descriptor 'esc -1))

(define (checked? num)
  (= num checked))

(define (compare oi ni)
  (let* ((old (vref memory oi))
         (new (vref memory ni))
         (tag (get-tag old)))
    (cond ((checked? old)
           t)
          ((neq? tag (get-tag new))
           (format t "~&Tags don't match~%")
           (wrong oi ni))
          ((or (garbage? old)
               (eq? 'fix tag))
           (cond ((n= old new)
                  (format t "~&Fix or garbage not equal~%")
                  (wrong oi ni))
                 (else
                  (right oi ni))))
          ((eq? 'esc tag)
           (compare-ptr old new))
          ((eq? 'pair tag)
           (compare-n old new 2))
          ((neq? old new)
           (format t "~&Plain not equal~%")
           (wrong oi ni))
          (else
           (right oi ni)))))

(define (compare-ptr old new)
  (cond ((checked? (mem-ref old))
         t)
        ((template? old)
         (let ((offset (template-offset old)))
           (compare-ptr (ptr- old offset) (ptr- new offset))))
        ((eq? 'imm (get-tag (mem-ref old)))
         (compare-n old new (gc-size old)))
        (else
         (compare-closure old new))))

(define (compare-closure old new)
  (let* ((template (mem-ref old))
         (offset (template-closure template))
         (size   (+ 1
                    (template-pointers template)
                    (template-scratch  template)))
         (type   (template-type template)))
    (case type
      ((heap)
       (compare-n old new size))
      ((closure)
       (compare-closure (ptr- old offset) (ptr- new offset)))
      ((stack)
       (compare-stack old o-off loc l-off))))) ;;; compare-stack is not defined

(define (compare-n old new n)
  (do ((i 0 (1+ i))
       (a (ash old -2) (1+ a))
       (b (ash new -2) (1+ b)))
      ((>= i n)
       t)
      (compare a b)))

(define (garbage? num)
  (and (eq? 'esc (get-tag num))
       (<= mem-size (ash num -2))))

(define (wrong oi ni)
  (error "Difference at ~D and ~D" oi ni))

(define (right oi ni)
  (set (vref memory oi) checked)
  (format t "~&~-4D = ~-4D " oi ni)
  (print-desc (vref memory ni)))
