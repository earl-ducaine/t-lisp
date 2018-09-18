(herald (tsys ungc t 44)
        (pre-cook)
        (env tsys (tsys gc)))

;;; Copyright (c) 1983, 1984 Yale University

;;; Heap management stuff.

(define *gc-recklessness* 'high)  

(define-integrable (heap-space-remaining)
  (fx- *heap-end* (pointer-address (heap-pointer))))

(define-integrable (heap-space-used)
  (fx- (pointer-address (heap-pointer)) *heap-begin*))

;;; Heap overflow handler.  This will eventually become much hairier.
;;; E.g., some day it will be triggerred not by a poll in IRETURN, but rather
;;;  by hardware-detected references to unwritable guard pages.
;;; At the point this is called, there is still a consing margin of up to
;;;  *GC-MARGIN* quadwords remaining.

(define (handle-heap-overflow val) (gc) val)    ; for hacked gc invocation

;;;  0
;;;  *PURE-INITIAL-MEM-BEGIN*
;;;  *PURE-INITIAL-MEM-END*
;;;  *IMPURE-INITIAL-MEM-BEGIN*
;;;  *IMPURE-INITIAL-MEM-END*
;;;  *STATIC-AREA*   [maybe someday]
;;;  *AREA-1*
;;;    begin   - lowest point of allocation
;;;    pointer - next location free for consing
;;;    limit   - consing beyond this point will eventually cause a GC
;;;    end     - point beyond which one may not cons
;;;  *AREA-2*
;;;    ...

;;;  The following simply act as caches for the current area.
;;;  They are NOT write-through caches, however.
;;;    *HEAP-BEGIN*
;;;    (HEAP-POINTER)
;;;    (HEAP-LIMIT)
;;;    *HEAP-END*

;;; Area stuff.

(define-operation (swap-current-area area))
(define-operation (sync-area area))
(define-operation (reset-area area))

(define-integrable (sync-current-area) (sync-area *the-current-area*))

;;; SIZE ought to be greater than *GC-MARGIN*.

(define (make-area area-begin area-size zeroed? id)
  (let ((begin   area-begin)
        (pointer area-begin)
        (end     (pointer-add area-begin area-size)))
    (object nil
            ((swap-current-area self)
             (sync-current-area)     ; write out cache
             (set *heap-begin* begin)
             (set *heap-end*   end)
             (set-heap-pointer (make-pointer pointer %%heap-tag))
             (xset (the-slink) %%heap-limit-index (fx- end *gc-margin*))
             (swap *the-current-area* self))
            ((sync-area self)
             (if (neq? self *the-current-area*)
                 (error "(SYNC-AREA ~S): area not current" self))
             (set zeroed? nil)
             (set pointer (pointer-address (heap-pointer))))
            ((reset-area self)
             (if (eq? self *the-current-area*)
                (error "(RESET-AREA ~S): area is current" self))
             (cond ((not zeroed?)
                    (zero-mem area-begin area-size)
                    (set zeroed? t)))
             (set pointer begin))
            ((print self stream)
             (format stream "#{Area ~s}" id)))))

(lset *area-1* (make-area *heap-1-begin* *heap-size* t '*area-1*))
(lset *area-2* (make-area *heap-2-begin* *heap-size* t '*area-2*))

(lset *the-current-area* *area-1*)
(lset *the-other-area*   *area-2*)

(swap-current-area *the-current-area*)  ; set cached variables on start up
 
;;; Switch to control when the heap is cleared: before, or after, a GC.

(lset *clear-heap-after-gc?* t)           

(define clear-heap-after-gc?
  (object (lambda () *clear-heap-after-gc?*)
          ((setter self) 
           (lambda (val)   
                (let ((val (true? val)))
                  (reset-area *the-other-area*)
                  (set *clear-heap-after-gc?* val)
                  val)))))

(define (gc-flip)
  ;(disable-ctrl-c)
  (if (not *clear-heap-after-gc?*) (reset-area *the-other-area*))
  (set *oldspace-low*  *heap-begin*)
  (set *oldspace-high* (pointer-address (heap-pointer)))
  (set *the-other-area* (swap-current-area *the-other-area*))
  (set *heap-overflow-pending?* nil))

(define (gc-done)      
  (if *clear-heap-after-gc?* (reset-area *the-other-area*))
  (sync-current-area)
  ;(enable-ctrl-c)
  )
