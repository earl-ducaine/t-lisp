(HERALD TCTOP                           ; -*- Mode:T; System:TCOMP -*-
        (ENV TCOMP))

(FORMAT (TERMINAL-OUTPUT) "Initializing compiler...~%")

;;; Patches...
(DEFINE (LSH X Y)
  (IF (FX< Y 0) (FIXNUM-ASHR X (FX- 0 Y)) (FIXNUM-ASHL X Y)))

(LSET *PRINT-LEVEL* 5)   ; Useless
(LSET *PRINT-LENGTH* 8)

(DEFINE *EMPTY*
  (OBJECT NIL ((PRINT SELF STREAM) (WRITES STREAM "#{Empty}"))))
(DEFINE *INTERNAL*
  (OBJECT NIL ((PRINT SELF STREAM) (WRITES STREAM "#{Internal}"))))

(LSET *STAT-COUNTERS* '())
(LSET *COMPILATION-GLOBALS* '())
(LSET *REP-CONVERTERS* '())

(LSET *NOISE-OUTPUT* (TERMINAL-OUTPUT))
(LSET *NOISE-PLUS-TERMINAL* (TERMINAL-OUTPUT))
(LSET *SUPPORT-OUTPUT* (TERMINAL-OUTPUT))
(LSET *ASSEMBLY-OUTPUT* (TERMINAL-OUTPUT))
(LSET *ENV* '())
(LSET *WHERE* '())

;;; Parameters for TRANSDUCE

(DEFINE (*DEFINE-TARGET-PARAMETERS A M S E I Z N)
  (DEFINE *ASSEMBLER-TYPE* A)
  (DEFINE *TARGET-MACHINE* M)
  (DEFINE *TARGET-SYSTEM*  S)
  (DEFINE *ASSEMBLY-EXTENSION*  E)
  (DEFINE *TARGET-INDIRECTION?* I)
  (DEFINE *SUPPORT-EXTENSION* Z)
  (DEFINE *NOISE-EXTENSION*   N))

(DEFINE (ESTABLISH-TARGET-PARAMETERS FS LP)
  (LET ((Z *DEFINE-TARGET-PARAMETERS))
    (COND ((AEGIS-FS? FS)
           (Z    '68000-APOLLO '\68000 'APOLLO 'ASM NIL 'AMS 'AMN))
          ((UNIX-FS? FS)
             (COND ((VAX11-PROCESSOR? LP)
                    (Z 'VAX-UNIX     'VAX     'UNIX   'S   T   'UVS 'UVN))
                   ((PYRAMID-PROCESSOR? LP)
                    (Z 'PYRAMID-UNIX 'PYRAMID 'UNIX   'S   NIL 'UPS 'UPN))
                   (T (ERROR "unknown UN*X processor type ~S" LP))))
          ((VMS-FS? FS)
           (Z    'VAX-VMS      'VAX    'VMS    'MAR T   'VVS 'VVN))
          (T (ERROR "unknown target filesystem type ~S" FS)))))

(COND (*ESTABLISH-TARGET-PARAMETERS?*
       (ESTABLISH-TARGET-PARAMETERS (LOCAL-FS) (LOCAL-PROCESSOR))))