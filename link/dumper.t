(herald dumper)

(define-constant ZMAGIC #o413)

(define-structure-type lstate   ;linker state
    pi          ;the pure position independent area
    pd          ;the pure position   dependent area
    im          ;the impure area
    cyms        ;the list of cymbal thunks
    ncyms       ;length of the cyms list
    stryngs     ;the list of stryngs
    stryngsize  ;number of chars in the stryng table
    )

(define (write-link-file stream lstate)
  (write-header          stream lstate)
  (write-text-segment    stream lstate)
  (write-data-segment    stream lstate)
  (write-text-relocation stream lstate)
  (write-data-relocation stream lstate)
  (write-cymbal-table    stream lstate)
  (write-stryng-table    stream lstate)
  )

(define-constant CYMBAL-SIZE 12)    ;size of cymbal struct in bytes
(define-constant  RELOC-SIZE  8)    ;size of relocation struct in bytes

(define (write-header stream lstate)
  (let* ((unaligned-text-size (fx+ (area-frontier (lstate-pi lstate))
                                   (area-frontier (lstate-pd lstate))))
         (text-size (align unaligned-text-size 10))
         (unaligned-data-size (area-frontier (lstate-im lstate)))
         (data-size (align unaligned-data-size 10)))
  (write-int stream ZMAGIC)                 ;magic number
  (write-int stream text-size)              ;text segment size
  (write-int stream data-size)              ;data segment size
  (write-int stream 0)                      ;bss  segment size
  (write-int stream (fx* CYMBAL-SIZE        ;cymbal table size
                         (lstate-ncyms lstate)))
  (write-int stream 0)                      ;bogus entry point
  (write-int stream                         ;text reloc table size
             (fx* RELOC-SIZE (fx+ (area-reloc-count (lstate-pi lstate))
                                  (area-reloc-count (lstate-pd lstate)))))
  (write-int stream                         ;data reloc table size
             (fx* RELOC-SIZE (area-reloc-count (lstate-im lstate))))
  ))

(define (write-text-segment stream lstate)
  (let* ((pi (lstate-pi lstate))
         (pd (lstate-pd lstate))
         (unaligned-text-size (fx+ (area-frontier pi)
                                   (area-frontier pd)))
         (text-size (align unaligned-text-size 10)))
    (for (x in (area-objects pi))
         (do (write-store x stream)))
    (for (x in (area-objects pd))
         (do (write-store x stream)))
    (dotimes (i (fx- text-size unaligned-text-size))
      (write-byte stream 0))))

(define (write-data-segment stream lstate)
  (let* ((im (lstate-im lstate))
         (unaligned-data-size (area-frontier im))
         (data-size (align unaligned-data-size 10)))
    (for (x in (area-objects im))
         (do (write-store x stream)))
    (dotimes (i (fx- data-size unaligned-data-size))
      (write-byte stream 0))))

(define (write-text-relocation stream lstate)
  (for (thunk in (area-reloc-items (lstate-pi lstate)))
       (do (thunk stream)))
  (for (thunk in (area-reloc-items (lstate-pd lstate)))
       (do (thunk stream))))

(define (write-data-relocation stream lstate)
  (for (thunk in (area-reloc-items (lstate-im lstate)))
       (do (thunk stream))))

(define (write-cymbal-table stream lstate)
  (for (thunk in (lstate-cymbals lstate))
       (do (thunk stream))))

(define (write-stryng-table stream lstate)
  (write-int stream (fx+ 4 (lstate-stryngsize lstate)))
  (for (str in (lstate-stryngs lstate))
       (do (write-string stream str))))
