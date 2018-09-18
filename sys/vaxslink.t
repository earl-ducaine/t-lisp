(herald (tsys vaxslink t 73)
        (pre-cook)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;; Automatic SLINK generation
;;; Someday JAR should document what this is really about.

;;; The assembly file generated from this source sets up the SLINK.
;;; The SUPPORT file generated does PUTPROP's to define symbolic values
;;; for the code generator to use in (SLINK -foo-) operands.

;;; Each SLINK spec, e. g. (FROTZ MUMBLE), becomes, in the LAP file,
;;;   .long mumble
;;; and, in the .SUPPORT file,
;;;   (DEFPROP FROTZ 36. SLINK-OFFSET)

;;; THE ORDER OF THESE ENTRIES IS VERY IMPORTANT!!  DO NOT INSERT OR DELETE
;;; (except at the end) UNLESS YOU WANT TO RECOMPILE THE ENTIRE WORLD!

(ceval (gen-slink
        '(
          ;; The first 60. of these are addressable with byte offsets from SLP.
          ;; The rest need word offsets.
          ;; Internal transfer points:
          (paranoid-icall           (peso paranoid-icall))
          (confident-icall          (peso confident-icall))
          (icall                    (peso icall))
          (ireturn                  (peso ireturn))
          (iapply                   (peso iapply))
          (lexpr-setup              (peso lexpr-setup))
          (inapplicable             (peso inapplicable))
          (compiler-lossage         (peso compiler-lossage))
          (()                       (peso 0))   ; Unused
          ;; "Registers":
          (stack-limit              (peso 0))
          (heap-limit               (peso 0))   ; Initialized by kernel
          (jump-from                (peso 0))   ; See ICALL, IRETURN
          (bits                     (peso 0))
          (()                       (peso 0))   ; Unused
          ;; Masks:
          (address-mask             (peso #x+fffffff8))
          (low-11-bits              (peso #x+fffff807))
          (low-19-bits              (peso #x+FFF80007)) ;For STRING-LENGTH
          ;; Random language-level constants:
          (vframe-template          (peso vframe-template))
          (true                     (pointer t))
          (null-character           (pointer #\null))
          (escape-procedure-template (peso escape-procedure-template))
          (symbol-template          (peso symbol-template))
          (vcell-template           (peso vcell-template))
          (vector-template          (peso vector-template))
          (()                       (peso 0))   ; Unused
          (template-guts            (peso %%template-low-peso))
          ;; Cruft for CALL-XENOID:
          (saved-hp                 (peso 0))
          (saved-ap                 (peso 0))
          (saved-fp                 (peso 0))   ; VMS needs
          (()                       (peso 0))   ; Unused
          (()                       (peso 0))   ; Unused
          (paranoid-ireturn         (peso paranoid-ireturn))
          (confident-ireturn        (peso confident-ireturn))
          )))

;;; The Slink...

(define *slink-template*
  (lap-template slink-template c$slink-template ((definee *the-slink*))
    (jmp (@ (slink inapplicable)))))

;;; (define *the-slink* (xenoid-pointer (xenoid "the_slink")))

(lap (globl the-slink)
     (initial-value *the-slink* the-slink))

(define-local-syntax (define-slink-index var exp)
  `(define-constant ,var
     ',(cond ((and (symbol? exp) (number? (get exp 'slink-offset)))
              (/ (get exp 'slink-offset) 4))
             (else (error "bad SLINK-OFFSET property for ~S"
                          exp)
                   0))))

(define-slink-index %%icall-index           icall)
(define-slink-index %%paranoid-icall-index  paranoid-icall)
(define-slink-index %%confident-icall-index confident-icall)

(define-slink-index %%ireturn-index           ireturn)
(define-slink-index %%paranoid-ireturn-index  paranoid-ireturn)
(define-slink-index %%confident-ireturn-index confident-ireturn)

(define-slink-index %%iapply-index          iapply)     ; what good is this?
(define-slink-index %%true-index            true)

(define-slink-index %%jump-from-index       jump-from)
(define-slink-index %%heap-limit-index      heap-limit)
(define-slink-index %%stack-limit-index     stack-limit)
(define-slink-index %%saved-hp-index        saved-hp)
