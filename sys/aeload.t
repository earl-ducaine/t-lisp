(HERALD AELOAD
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Object file loader

;;; This is the machine-dependent portion of FASLOAD; mostly file I/O.
;;; See RELOC.T for the code which does the actual S-expression relocation.

(define (object-file-stream? stream)
  (let ((channel (stream-channel stream)))
    (and channel
         (fixnum-odd? (call-xenoid 'data 'fixnum *t_$object_file_streamp-xenoid*
                                   channel)))))

(define (object-file? spec)
  (with-open-files ((stream (open spec '(in))))
                   (object-file-stream? stream)))

;;; Argument is an open stream.  We don't need to worry about closing it.
;;; Return a unit.

(DEFINE (LOAD-RAW-UNIT STREAM)
  (CHANNEL-LOAD-RAW-UNIT (STREAM-CHANNEL STREAM)))

(DEFINE (CHANNEL-LOAD-RAW-UNIT CHANNEL)
  (LET ((PURE-POS (FILE-REF CHANNEL 8))
        (IMPURE-POS (FILE-REF CHANNEL 16)))
    (CHANNEL-SEEK CHANNEL PURE-POS)
    (LET ((MAGIC (READ-ONE-WORD-RAW CHANNEL)))  ;CODE MAGIC NUMBER
      (SELECT MAGIC
        ((%%INCORRECT-FASL %%CORRECT-FASL))
        (ELSE (ERROR "attempting to load a non-T object file")
              (NOT-PROCEEDABLE)))
      (LET ((PROC-SIZE (READ-ONE-WORD-RAW CHANNEL))
            (DATA-SIZE (BLOCK (CHANNEL-SEEK-RELATIVE CHANNEL 12)
                              (READ-ONE-WORD-RAW CHANNEL))))
        (CHANNEL-SEEK CHANNEL PURE-POS)
        (LET ((CODE (POINTER-ADDRESS (MAKE-EXTEND 0 PROC-SIZE)))
              (UNIT (MAKE-EXTEND-N 0 DATA-SIZE)))
          ;; This should do a MAP - no sweat, but later.
          (CHANNEL-READ CHANNEL CODE (FX* PROC-SIZE 8))
          (CHANNEL-SEEK CHANNEL IMPURE-POS)
          (READ-RAW-IMPURE CHANNEL (POINTER-ADDRESS UNIT) DATA-SIZE)
          (SET-EXTEND-TEMPLATE UNIT *UNIT-TEMPLATE*)
          (SET-UNIT-CODE UNIT CODE)
          UNIT)))))

(LET ((+BUFFER+ (MAKE-XENOID 0)))       ;Own variable

  ;; Return fixnum at given position (read 32 bits & convert)

  (DEFINE (FILE-REF CHANNEL FIXNUM-POSITION)
    (CHANNEL-SEEK CHANNEL FIXNUM-POSITION)
    (CHANNEL-READ CHANNEL +BUFFER+ 4)
    (XENOID->FIXNUM +BUFFER+))

  ;; Return a (potentially unsafe) object that is the next 32 bits in the file
  ;; Do something about this being UNSAFE

  (DEFINE (READ-ONE-WORD-RAW CHANNEL)
    (CHANNEL-READ CHANNEL +BUFFER+ 4)
    (XENOID-POINTER +BUFFER+))
  )

;;; Read the data section - continue to punt to pascal here for now

(DEFINE (READ-RAW-IMPURE CHANNEL ADDRESS SIZE-IN-PESOS)
  (FX= (CALL-XENOID 'DATA 'FIXNUM *T_$READ_IMPURE-XENOID*
          ADDRESS (FX* SIZE-IN-PESOS 8) #\l #\p CHANNEL)
       0))
