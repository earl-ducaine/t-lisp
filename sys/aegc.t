(herald (tsys aegc t)
        (pre-cook)
        (env tsys (tsys gc)))

;;; Copyright (c) 1983, 1984 Yale University

(define *gc-recklessness* 'high)

;;;; Area stuff

(define-operation (swap-current-area area))
(define-operation (sync-area area))
(define-operation (unmap-area area))
(define-operation (reset-area area))

(define-integrable (sync-current-area) (sync-area *the-current-area*))

;;; Cons an area, given it size in fixnum number of quardwords.
;;; Areas should be a multiple of 32K, >64K.

(define (make-area size)
  (let* ((size (fixnum-logand size -4096))      ; -4096 -> 32K align
         (path (gen-pathname "/tmp/t_heap_"))
         (uid (name-cr-file path)))
    (ms-map path size 0 (lambda (at length) 
                           (make-area-1 path uid at length)))))

;;; Cons an area. BEGIN, END et al, are all machine addresses or numbers.

(define (make-area-1 path uid begin length)
  (let ((pointer begin)
        (end (fx+ begin length)))
      (object nil
              ((swap-current-area self)
               (sync-current-area)
               (set *heap-begin* begin)
               (set *heap-end*   end)
               (set *heap-path*  path)
               (set-heap-pointer (make-pointer pointer %%heap-tag))
               ;; this XSET for icall hacked invoking only
               (xset *the-slink* %%heap-limit-index (fx- end *gc-margin*))
               (swap *the-current-area* self))
              ((sync-area self)
               (if (neq? self *the-current-area*)
                   (error "(SYNC-AREA ~S): area not current" self))
               (set pointer (pointer-address (heap-pointer))))
              ((reset-area self)
               (if (eq? self *the-current-area*)
                   (error "(RESET-AREA ~S): area is current" self))
               (set pointer begin)
               (file-truncate uid 0))
              ((unmap-area self)
               (cond (path (ms-unmap begin length)
                           (name-delete-file path))))
              ((print self stream)
               (format stream "#{Area~_~S~_~S}" (object-hash self) path)
               (format stream "~%;begin: ~x~%;length: ~x~%;pointer: ~x~%" 
                               begin length (pointer-address pointer)))
              )))

;;; A few area hacking utilities.

;;; Called when T is exited.

(define (unmap-areas)
  (if *the-other-area* (unmap-area *the-other-area*))
  (call-xenoid nil nil *t_$unmap_area-xenoid*
    *heap-begin* *heap-end* #\p #\p #\s *heap-path*))

;;; Initializing initial areas

(lset *heap-1-path* 
      (apollo-string->string (make-xenoid *heap-1-path*) *heap-1-path-length*))
(lset *area-1* (make-area-1 *heap-1-path*
                            (name-resolve *heap-1-path*)
                            *heap-1-begin*
                            *heap-size*))

(lset *heap-2-path* 
      (apollo-string->string (make-xenoid *heap-2-path*) *heap-2-path-length*))
(lset *area-2* (make-area-1 *heap-2-path*
                            (name-resolve *heap-2-path*)
                            *heap-2-begin*
                            *heap-size*))

(lset *the-current-area* *area-1*)
(lset *the-other-area* *area-2*)
(lset *heap-overflow-pending?* nil)

(swap-current-area *the-current-area*)  ; set cached variables on start up

;;; Snarf initial heap file into an AREA object. Someone calls this on booting

(define (handle-heap-overflow val)
  (gc-identity val))                    ; For hacked GC invocation

;;; Switch to the given area.  Returns the previous area!

(define (gc-flip)
  (set *oldspace-low*  *heap-begin*)
  (set *oldspace-high* (pointer-address (heap-pointer)))
  (set *heap-overflow-pending?* t)
  (set *the-other-area* (swap-current-area *the-other-area*))
  (set *heap-overflow-pending?* nil))

(define (gc-done)
  (reset-area *the-other-area*)
  (sync-current-area))

;;; GC forwared flonum marker.

(define-constant *flonum-forward-marker* #x+ffff)       ; See IEEE spec

(define (gc-copy-flonum loc obj)
  (cond ((flonum-forwarded? obj)
         (%set-loc-contents loc (flonum-forwarded obj))
         (if *gc-copy-debug?* (note-gc-repeat obj)))
        (else
         (let ((new-obj (hacked-copy-flonum obj)))
           (if *gc-copy-debug?* (note-gc-copy obj new-obj))
           (%set-loc-contents loc new-obj)
           (set-flonum-forwarded obj new-obj)))))

(define-lap-procedure hacked-copy-flonum ((expr 1 0 0))
  (move.l (reg+ sp) xp)
  (adjust-tag hp val %%flonum-tag)
  (lea (reg hp 8) hp)
  (move.l (reg xp %%flonum-low-peso-offset)
          (reg val  %%flonum-low-peso-offset))
  (move.l (reg xp %%flonum-high-peso-offset)
          (reg val  %%flonum-high-peso-offset))
  (move.l (reg+ sp) tp)                 ; pop return address and
  (jmp (reg tp)))

;;; GC string stuff

;;; This is perhaps best done by LAP code.  Pretty yicky as it is.
;;; Should hack the string printer so that it can handle forwarded strings
;;; without utterly flipping its gahooey.

;;; Unfortunately, we always need to copy the string text, even if the length
;;; is zero.  (This is to accomodate the use of strings as "buffers", that is,
;;; extendable arrays with fill-pointers.)

(DEFINE-CONSTANT *STRING-FORWARD-MARKER* -1)

(DEFINE-INTEGRABLE (STRING-FORWARDED? OBJ)	;Called by ALIVE-AFTER-GC?
  (FIXNUM-EQUAL? (STRING-BASE OBJ) *STRING-FORWARD-MARKER*))

(DEFINE (GC-COPY-STRING LOC OBJ)
  (LET ((BASE (STRING-BASE OBJ)))
    (COND ((FIXNUM-EQUAL? BASE *STRING-FORWARD-MARKER*)
           (IF *GC-COPY-DEBUG?* (NOTE-GC-REPEAT OBJ))
           (%SET-LOC-CONTENTS LOC (STRING-POINTER OBJ)))     ; Yick.
          (ELSE
           (SET *GC-STRING-COUNT* (FX+ *GC-STRING-COUNT* 1))
           (LET ((NEW-OBJ (CHOPY OBJ)))
             (IF *GC-COPY-DEBUG?* (NOTE-GC-COPY OBJ NEW-OBJ))
             (%SET-LOC-CONTENTS LOC NEW-OBJ)
             (LET ((PTR (STRING-POINTER OBJ))) 
               (IF (OLDSPACE-CONTAINS? PTR)
                   (SET-STRING-POINTER NEW-OBJ (GC-COPY-STRING-TEXT PTR))))
             (SET-STRING-POINTER OBJ NEW-OBJ)
             (SET-STRING-BASE    OBJ *STRING-FORWARD-MARKER*))))))

;;; The low 3 bits of a string text pointer tend to be 010.  We sort of assume
;;;  herein that %%TEXT-LENGTH-OFFSET is -2.  Sigh.

(DEFINE-LAP-PROCEDURE GC-COPY-STRING-TEXT ((EXPR 1 0 0))
  (MOVE.L (REG+ SP) XP)                 ; Existing text in XP
  (LEA (REG XP %%TEXT-LENGTH-OFFSET) XP)        ; ...point to corner
  (MOVEQ.L (LIT -1) D7)
  (CMP.L (REG XP) D7)                   ; If already moved, then
  (BNE.S CPTXT)                         ; 0th peso is -1 and 1th
  (MOVE.L (REG XP 4) VAL)               ; peso is new address
  (MOVE.L (REG+ SP) TP)                 ; pop return address and
  (JMP (REG TP))                        ; jump to it
CPTXT
  ;; Add 2 (for length field) and round up to quadword
  (CLR.L D0)
  (MOVE.W (REG XP) D0)
  (ADD.L (LIT 9) D0)
  (ANDI.B (LIT #b11111000) D0)
  (ADJUST-TAG HP FUN 0)                 ; Cons!, new text in FUN
  (ADDA.W D0 HP)                        ; ...
  (MOVE.L XP VAL)                       ; Save pointer to text
  (MOVE.L FUN D1)                       ; Save away ptr to new text
  (ASR.W (LIT 2) D0)                    ; convert to count of longwords
  (BRA.S CPTXT-LOOP-START)
CPTXT-LOOP
    (MOVE.L (REG+ XP) (REG+ FUN))
CPTXT-LOOP-START
    (DBRA D0 CPTXT-LOOP)
  (MOVE.L D7 (REG VAL))                 ; Store forwarding ptr
  (ADDQ.L (LIT (~ %%TEXT-LENGTH-OFFSET)) D1)
  (MOVE.L D1 (REG VAL 4))               ; ...
  (MOVE.L D1 VAL)
  (MOVE.L (REG+ SP) TP)                 ; pop return address and
  (JMP (REG TP)))                       ; jump to it


;;; Random utilities.

(DEFINE-INTEGRABLE (HEAP-SPACE-REMAINING)
  (FX- *HEAP-END* (POINTER-ADDRESS (HEAP-POINTER))))

(DEFINE-INTEGRABLE (HEAP-SPACE-USED)
  (FX- (POINTER-ADDRESS (HEAP-POINTER)) *HEAP-BEGIN*))
