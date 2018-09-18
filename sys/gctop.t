(HERALD (TSYS GCTOP T 13)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;; Non-critical portion of GC: entry and exit protocol, statistics, etc.

(LSET *GC-COUNT* 0)
(LSET *DOING-GC?* NIL)
(LSET *PRINT-GC-STATS?* NIL)
(LSET *GC-MUST-RECLAIM* 1000)

(DEFINE (GC) (GC-IDENTITY T))

(DEFINE (GC-IDENTITY VAL)
  ;; Interrupts should be disabled here.
  (COND (*DOING-GC?*
         (ERROR "GC losing grossly!  *DOING-GC?* is already T!")))
  (SET *GC-LOSER* NIL)
  (SET *GC-FAULT-FRAME-LOSSAGE?* NIL)
  (SET *GC-FRAME-LOSSAGE?* NIL)
  (BIND ((*DOING-GC?* T)
         (*Z?* T)
         ((RECKLESSNESS) *GC-RECKLESSNESS*))
    (IF *GC-NOISILY?*
        (GC-FORMAT GC-OUTPUT "~&~aBeginning garbage collection.~%"
                   #\SEMICOLON))        ; U editor loses
    (WALK1 (LAMBDA (ITEM) ((CDR ITEM))) *PRE-GC-AGENDA*)
    (RESET-GC-STATS)

    (GC-FLIP)
    (GC-ROOT)
    (SET *GC-COUNT* (FX+ *GC-COUNT* 1))     ; for tables
    (WALK1 (LAMBDA (ITEM) ((CDR ITEM))) *POST-GC-AGENDA*)
    (GC-DONE)

    (SET *GC-COPY-COUNT* (FX+ *GC-COPY-COUNT* *GC-COPY-TICK*))
    (SET *HEAP-POINTER-AFTER-GC* (POINTER-ADDRESS (HEAP-POINTER)))
    (IF *GC-NOISILY?*
        (GC-FORMAT GC-OUTPUT "~aGarbage collection finished.~%"
                   #\SEMICOLON)))        ; U editor loses
  (IF (OR *GC-DEBUG?* *PRINT-GC-STATS?*)
      (GC-STATS))
  (COND ((FX< (HEAP-SPACE-REMAINING)
              *GC-MUST-RECLAIM*)
         (ERROR '("GC failed to reclaim ~S quadwords~%"
                  "  You may be out of space, but type (RET) to proceed."))))
  (COND (*GC-LOSER*
         (ERROR "GC encountered a horrible pointer (#x~X -> #x~X).~
               ~%  Don't panic; type (RET) to proceed."
                *GC-LOSER* *GC-LOSER-LOC*)))
  (COND (*GC-FRAME-LOSSAGE?*
         (ERROR "GC encountered a horrible stack frame.~
               ~%  The stack is probably in a wedged state, so (RET) ~
               ~%  and (RESET) may lose here.")))
  (COND (*GC-FAULT-FRAME-LOSSAGE?*
         (ERROR "you shouldn't try to GC with a fault frame on the stack.~
               ~%  You probably won't be able to (RET), so try (RESET).")))
  VAL)

(DEFINE (RESET-GC-STATS)
  (SET *GC-COPY-COUNT* 0)
  (SET *GC-COPY-TICK* 0)
  (SET *GC-REPEAT-COUNT* 0)
  (SET *GC-BOGUS-COUNT* 0)
  (SET *GC-PAIR-COUNT* 0)
  (SET *GC-STRING-COUNT* 0)
  (SET *GC-EXTEND-COUNT* 0)
  (SET *GC-NONSTANDARD-COUNT* 0)
  (SET *GC-UNIT-COUNT* 0)
  (SET *GC-POPULATION-MEMBERS* 0)
  (SET *GC-POPULATION-DEATHS* 0))

(RESET-GC-STATS)

(DEFINE (GC-STATS)
  (LET ((BEFORE (FX- *OLDSPACE-HIGH* *OLDSPACE-LOW*))
        (AFTER (FX- *HEAP-POINTER-AFTER-GC* *HEAP-BEGIN*)))
    (FORMAT (DEBUG-OUTPUT)
            '(";;; Heap size before GC: ~S~%"
              ";;; Heap size after GC:  ~S~%"
              ";;; Net space reclaimed: ~S~%"
              ";;; ~-5s objects copied~%"
              ";;; ~-5s repeat encounters~%"
              ";;; ~-5s pairs moved~%"
              ";;; ~-5s strings moved~%"
              ";;; ~-5s standard extends moved~%"
              ";;; ~-5s non-standard extends moved~%"
              ";;; ~-5s units moved~%"
              ";;; ~-5s bad pointers encountered~%"
              ";;; ~-5s objects persisting in populations~%"
              ";;; ~-5s objects removed from populations~%")
            BEFORE
            AFTER
            (FX- BEFORE AFTER)
            *GC-COPY-COUNT*
            *GC-REPEAT-COUNT*
            *GC-PAIR-COUNT*
            *GC-STRING-COUNT*
            *GC-EXTEND-COUNT*
            *GC-NONSTANDARD-COUNT*
            *GC-UNIT-COUNT*
            *GC-BOGUS-COUNT*
            *GC-POPULATION-MEMBERS*
            *GC-POPULATION-DEATHS*)))

(DEFINE (ENSURE-HEAP-SPACE? SIZE)
  (COND ((FX< (HEAP-SPACE-REMAINING) SIZE)
         (GC)
         (FX>= (HEAP-SPACE-REMAINING) SIZE))
        (ELSE T)))
