
(lset k nil)  ;;; List of unfinished objects, linked in old heap


;;; Copy whatever old points to, storing the value of the new address + o-off
;;; at (index loc l-off).
(define (move-next old o-off loc l-off)
  (xcase (get-tag old)
    ((fix imm)
     (pop-k-list))
    ((pair)
     (move-pair old 0 loc l-off))
    ((esc)
     (cond ((template? old)
            (let ((offset (template-offset old)))
              (move-next (ptr- old offset) offset loc l-off)))
           ((new? old)
            (set (index loc l-off) (index old 0))
            (pop-k-list))
           (else
            (move-pointer old o-off loc l-off))))))

;;; Continuation of move-next, dealing with pointers.
(define (move-pointer old o-off loc l-off)
  (case (get-tag (index old 0))
    ((imm)
     (move-immediate-object old o-off loc l-off))
    ((esc)
     (move-closure old o-off loc l-off))
    (else
     (error "Corrupt header to ~S at ~S pointed to from ~S off of ~S"
            (get-tag (index old 0))
            old
            l-off
            loc))))

(define (move-immediate-object old o-off loc l-off)
  (let ((size (gc-size old)))
    (cond ((general-vector? old)
           (move-general old 0 loc l-off size (-1+ size)))
          ((or (vector? old)
               (> size 1))                ;;; check for off = 0 & bitv
           (move-general old o-off loc l-off size 0))
          (else
           (error "Immediate ~S pointed to from ~S off of ~S"
                  old l-off loc)))))

(define (move-closure old o-off loc l-off)
  (let* ((template (index old 0))
         (offset (template-closure  template))
         (ptrs   (template-pointers template))
         (scrs   (template-scratch  template))
         (type   (template-type     template)))
    (case type
      ((heap)
       (move-general old o-off loc l-off (+ 1 ptrs scrs) ptrs))
      ((closure)                          ;;; Could check for o-off = 0
       (move-closure (ptr- old offset) offset loc l-off))
      ((stack)
       (move-stack old o-off loc l-off))))) ;;; move-stack is not defined

;;; Copy the next thing on the list of unfinished objects
(define (pop-k-list)
  (cond ((nil? k)
         t)
        (else
         (real-pop-k-list))))

(define (real-pop-k-list)
  (cond ((eq? 'pair (get-tag k))
         (let ((copy (gc-cdr k)))
           (set k (gc-car k))
           (move-next (index copy 1) 0 copy 1)))
        (else
         (let ((next (index k 2)))
           (cond ((fx> 1 next)
                  (set k (index k 1))
                  (pop-k-list))
                 (else
                  (let ((copy (index k 0)))
                    (set (index k 2) (-1+ next))
                    (move-next (index copy next) 0 copy next))))))))

(define (move-pair old o-off loc l-off)
  (let ((new (copy-chunk old 2)))
    (set (index loc l-off) (change-tag (ptr+ new o-off) 'pair))
    (set (gc-car old) k)            ;;; old car links to K-list
    (set k old)
    (move-next (index new 0) 0 new 0))) ;;; move the cdr

;;; Link the thing at old into the K list, then copy the first elt.
(define (move-multiple old new addrs)
  (set (index old 1) k)          ;;; first pointer links into K-list
  (set k old)
  (set (index old 2) addrs)                       ;;; index of next pointer
  (move-next (index new 0) 0 new 0))              ;;; move the first pointer

;;; Move the object at old
(define (move-general old o-off loc l-off size addrs)
  (let ((new (copy-chunk old size)))
    (set (index loc l-off) (ptr+ new o-off))   ;;; set pointer to new copy
    (cond ((= addrs 0)
           (pop-k-list))
          ((= addrs 1)
           (move-next (index new 1) 0 new 1))
          (else
           (move-multiple old new addrs)))))


;;; There is another version of this in the simulator.
(comment
(define (copy-chunk old size)
  (let ((start (allocate size)))
    (do ((i 0 (1+ i)))
        ((>= i size))
        (set (index start i) (index old i))
        (set (index old i) (ptr+ start i)))
    start))
)
