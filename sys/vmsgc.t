(HERALD VMSGC
        (PRE-COOK)
        (ENV TSYS (TSYS GC)))

;;; Copyright (c) 1983, 1984 Yale University

;;; Heap management stuff.

(DEFINE-INTEGRABLE (HEAP-SPACE-REMAINING)
  (FX- *HEAP-END* (POINTER-ADDRESS (HEAP-POINTER))))

(DEFINE-INTEGRABLE (HEAP-SPACE-USED)
  (FX- (POINTER-ADDRESS (HEAP-POINTER)) *HEAP-BEGIN*))


;;; (SET-HEAP-LIMIT limit) arranges for a heap-overflow fault (and subsequent
;;;  call to EXPAND-HEAP) as soon as possible after the heap pointer exceeds
;;;  the specified limit minus *GC-MARGIN*.

(DEFINE HEAP-LIMIT
  (OBJECT (LAMBDA ()
            (FX+ (XREF (THE-SLINK) %%HEAP-LIMIT-INDEX) *GC-MARGIN*))
          ((SETTER SELF) SET-HEAP-LIMIT)))

(DEFINE (SET-HEAP-LIMIT NEW-HEAP-LIMIT)
  (XSET (THE-SLINK) %%HEAP-LIMIT-INDEX (FX- NEW-HEAP-LIMIT *GC-MARGIN*)))

(LSET *COMMENT-ON-ALLOCATION-BEHAVIOR?* NIL)

;;; Heap overflow handler.  This will eventually become much hairier.
;;; E.g., some day it will be triggerred not by a poll in IRETURN, but rather
;;;  by hardware-detected references to unwritable guard pages.
;;; At the point this is called, there is still a consing margin of up to
;;;  *GC-MARGIN* quadwords remaining.

(DEFINE (HANDLE-HEAP-OVERFLOW VAL) (GC) VAL)    ; For hacked GC invocation


;;;  0
;;;  *PURE-INITIAL-MEM-BEGIN*
;;;  *PURE-INITIAL-MEM-END*
;;;  *IMPURE-INITIAL-MEM-BEGIN*
;;;  *IMPURE-INITIAL-MEM-END*
;;;  *STATIC-AREA*   [maybe someday]
;;;  *LOW-MEM-AREA*
;;;    begin   - lowest point of allocation
;;;    pointer - next location free for consing
;;;    limit   - point up to which space is allocated and zeroed
;;;    end     - point beyond which one may not cons
;;;  *HIGH-MEM-AREA*
;;;    ...

;;;  The following simply act as caches for the current area.
;;;  They are NOT write-through caches, however.
;;;    *HEAP-BEGIN*
;;;    (HEAP-POINTER)
;;;    (HEAP-LIMIT)
;;;    *HEAP-END*

(DEFINE *GC-RECKLESSNESS* 'HIGH)

;;; Area stuff.

(DEFINE CURRENT-AREA
  (OBJECT (LAMBDA () *THE-CURRENT-AREA*)
          ((SETTER SELF) SET-CURRENT-AREA)))

(DEFINE-OPERATION (SET-CURRENT-AREA AREA))
(DEFINE-OPERATION (SYNC-AREA AREA))
(DEFINE-OPERATION (RESET-AREA AREA))

(DEFINE-INTEGRABLE (SYNC-CURRENT-AREA) (SYNC-AREA *THE-CURRENT-AREA*))

;;; SIZE ought to be greater than *GC-MARGIN*.

(DEFINE (MAKE-AREA AREA-BEGIN AREA-END ID)
  (LET ((BEGIN   AREA-BEGIN)
        (POINTER AREA-BEGIN)
        (LIMIT   AREA-END)
        (END     AREA-END))
    (OBJECT NIL
            ((SET-CURRENT-AREA SELF)
             (SYNC-AREA *THE-CURRENT-AREA*)     ; write out cache
             (SET *HEAP-BEGIN* BEGIN)
             (SET-HEAP-POINTER (MAKE-POINTER POINTER %%HEAP-TAG))
             (SET-HEAP-LIMIT   LIMIT)
             (SET *HEAP-END*   END)
             (SET *THE-CURRENT-AREA* SELF))
            ((SYNC-AREA SELF)
             (IF (NEQ? SELF *THE-CURRENT-AREA*)
                 (ERROR "(SYNC-AREA ~S): area not current" SELF))
             (SET BEGIN *HEAP-BEGIN*)
             (SET LIMIT (HEAP-LIMIT))
             (SET END   *HEAP-END*)
             (SET POINTER (POINTER-ADDRESS (HEAP-POINTER))))
            ((RESET-AREA SELF)
             (IF (EQ? SELF *THE-CURRENT-AREA*)
                 (ERROR "(RESET-AREA ~S): area is current" SELF))
             (CALL-XENOID *RESET-AREA-XENOID* END BEGIN)
             (SET LIMIT   END)
             (SET POINTER BEGIN))
            ((PRINT SELF STREAM)
             (FORMAT STREAM "#{Area ~S}" ID)))))

(LSET *LOW-MEM-AREA*
      (MAKE-AREA *AREA0-BEGIN* *AREA0-END* '*LOW-MEM-AREA*))

(LSET *HIGH-MEM-AREA*
      (MAKE-AREA *AREA1-BEGIN* *AREA1-END* '*HIGH-MEM-AREA*))

(LSET *THE-CURRENT-AREA* *LOW-MEM-AREA*)

(DEFINE (GC-FLIP)
  (DISABLE-CTRL-C)
  ;; (XCALL *update-GC-stats-xenoid* *local-gc-stats*)
  (SET *OLDSPACE-LOW*  *HEAP-BEGIN*)
  (SET *OLDSPACE-HIGH* (POINTER-ADDRESS (HEAP-POINTER)))
  (SELECT *THE-CURRENT-AREA*
    ((*LOW-MEM-AREA*)
     (SET-CURRENT-AREA *HIGH-MEM-AREA*))
    ((*HIGH-MEM-AREA*)
     (SET-CURRENT-AREA *LOW-MEM-AREA*)))
  (SET *HEAP-OVERFLOW-PENDING?* NIL))

(DEFINE (GC-DONE)
  (SELECT *THE-CURRENT-AREA*
    ((*LOW-MEM-AREA*)
     (RESET-AREA *HIGH-MEM-AREA*))
    ((*HIGH-MEM-AREA*)
     (RESET-AREA *LOW-MEM-AREA*)))
  ;; (XCALL *update-GC-stats-xenoid* *local-gc-stats*)
  (ENABLE-CTRL-C))
