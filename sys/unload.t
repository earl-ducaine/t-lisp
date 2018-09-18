(HERALD (TSYS UNLOAD T 33)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Object file loader

;;; This is the machine-dependent portion of FASLOAD, for VAX/Unix.
;;; See Berkeley Unix manual, a.out file format, in section 5.

;;; Predicates for determining object-file-ness.

(DEFINE (OBJECT-FILE? PATH)
  (WITH-OPEN-STREAMS ((STREAM (OPEN PATH '(IN))))
    (OBJECT-FILE-STREAM? STREAM)))

;;; Assume that stream points to the beginning of the file.  All is lost if
;;; it doesn't.

(DEFINE (OBJECT-FILE-STREAM? STREAM)
  (LET ((CHANNEL (STREAM-CHANNEL STREAM)))
    (AND CHANNEL
         (BLOCK0 (TRUE? (CHANNEL-OBJECT-FILE-MAGIC-NUMBER CHANNEL))
                 (CALL-XENOID *LSEEK-XENOID* 0 0
                              (FIXNUM->POINTER (CHANNEL->FILDES CHANNEL)))))))

;;; Return () or magic number.  Side effect: advances file pointer 4 bytes.

(DEFINE (CHANNEL-OBJECT-FILE-MAGIC-NUMBER CHANNEL)
  (LET ((MAGIC (POINTER->FIXNUM (READ-ONE-WORD-RAW CHANNEL))))
    (COND ((OR (FX= MAGIC #o0407)
               (FX= MAGIC #o0410)
               (FX= MAGIC #o0413))
           MAGIC)
          (ELSE NIL))))

;;; Return a unit.

(DEFINE (LOAD-RAW-UNIT STREAM)
  (CHANNEL-LOAD-RAW-UNIT (STREAM-CHANNEL STREAM) STREAM))

(DEFINE (CHANNEL-LOAD-RAW-UNIT CHANNEL STREAM)
  (LET ((MAGIC (CHANNEL-OBJECT-FILE-MAGIC-NUMBER CHANNEL)))
    (IF (NOT MAGIC)
        (ERROR "not an object file~%  (~S ~S)" 'CHANNEL-LOAD-RAW-UNIT CHANNEL)
      (LET ((TEXTSIZE (READ-ONE-WORD-RAW CHANNEL))
            (DATASIZE (READ-ONE-WORD-RAW CHANNEL)))
        ;; Make sure there's enough space in the heap.
        ;(ENSURE-HEAP-SPACE (FX+ TEXTSIZE DATASIZE))
        ;; There should be a better way to do raw allocation
        ;; Align to page boundary if ZMAGIC type?
        (LET ((CODE (MAKE-CODE TEXTSIZE)))
          ;; lseek (fildes, offset, whence)
          (CALL-XENOID *LSEEK-XENOID*
                       0
                       (IF (FX= MAGIC #o0413) (FIXNUM->POINTER 1024.)
                         (FIXNUM->POINTER 32.)) ;Maclisp reader sucks.
                       (FIXNUM->POINTER (CHANNEL->FILDES CHANNEL)))
          (READ-CAREFULLY-RAW CHANNEL CODE TEXTSIZE)    ; vread?
          (COND ((NOT (FX= (CODE-MAGIC-NUMBER CODE) %%INCORRECT-FASL))
                 (ERROR "not a T object file - ~S" STREAM))
                ((FX< DATASIZE (POINTER-ASH (CODE-UNIT-SIZE CODE) -1))
                 (ERROR '("corrupt object file ~S~%"
                          "  Size given in header is ~S; in code is ~S")
                        STREAM
                        DATASIZE
                        (POINTER-ASH (CODE-UNIT-SIZE CODE) -1)))
                (ELSE
                 (LET ((UNIT
                        (MAKE-EXTEND-N 0 (CODE-UNIT-SIZE CODE))))
                   ;; No seek should be necessary at this point.
                   (READ-CAREFULLY-RAW CHANNEL
                                       (POINTER-ADDRESS UNIT)
                                       DATASIZE)
                   (SET-EXTEND-TEMPLATE UNIT *UNIT-TEMPLATE*)
                   (SET-UNIT-CODE UNIT CODE)
                   UNIT))))))))

(DEFINE (READ-CAREFULLY-RAW CHANNEL LOC SIZE)
  (LET ((TRUESIZE (CALL-XENOID *READ-XENOID*
                               SIZE
                               LOC
                               (FIXNUM->POINTER (CHANNEL->FILDES CHANNEL)))))
    (COND ((NEQ? TRUESIZE SIZE)
           (ERROR "value returned by _read wasn't what was expected~
                 ~%Expected ~S and got ~S"
                  (POINTER->FIXNUM SIZE)
                  (POINTER->FIXNUM TRUESIZE))))))

(DEFINE READ-ONE-WORD-RAW
  (LET ((FOOLISH-STATIC-BUFFER (MAKE-VECTOR 1)))
    (LAMBDA (CHANNEL)
      (READ-CAREFULLY-RAW CHANNEL FOOLISH-STATIC-BUFFER (FIXNUM->POINTER 4))
      (VREF FOOLISH-STATIC-BUFFER 0))))
