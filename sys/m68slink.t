(HERALD M68SLINK
        (PRE-COOK)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; The SLINK, and its generation.

;;; The assembly file generated from this source sets up the SLINK.
;;; The SUPPORT file generated does PUTPROP's to define symbolic values
;;; for the code generator to use in (SLINK -foo-) operands.

;;; Each SLINK spec, e. g. (FROTZ MUMBLE), becomes, in the LAP file,
;;;   dc.l mumble
;;; and, in the .SUPPORT file,
;;;   (DEFPROP FROTZ 36. SLINK-OFFSET)
;;; THE ORDER OF THESE ENTRIES IS VERY IMPORTANT!!  DO NOT INSERT OR DELETE
;;; (except at the end) UNLESS YOU WANT TO RECOMPILE THE ENTIRE WORLD!

(CEVAL (GEN-SLINK
        '(
          ;; The first 60. of these are addressable with byte offsets from SLP.
          ;; The rest need word offsets.
          ;; Someday do: PARANOID- and CONFIDENT- versions of ICALL and IRETURN
          ;; (and perhaps even IAPPLY).
          ;; Internal transfer points
          (()                       (HALFPESO 0))
          (PARANOID-ICALL           (HALFPESO  #x+4EF9))
          (PARANOID-ICALL-LINK      (EXTERNAL-ADDRESS PARANOID-ICALL))

          (()                       (HALFPESO 0))
          (CONFIDENT-ICALL          (HALFPESO  #x+4EF9))
          (CONFIDENT-ICALL-LINK     (EXTERNAL-ADDRESS CONFIDENT-ICALL))
                  
          (()                       (HALFPESO 0))
          (ICALL                    (HALFPESO  #x+4EF9))
          (ICALL-LINK               (EXTERNAL-ADDRESS ICALL))
          
          (()                       (HALFPESO 0))
          (IRETURN                  (HALFPESO  #x+4EF9))
          (IRETURN-LINK             (EXTERNAL-ADDRESS IRETURN))
          
          (PARANOID-IRETURN-LINK    (EXTERNAL-ADDRESS PARANOID-IRETURN))
          (CONFIDENT-IRETURN-LINK   (EXTERNAL-ADDRESS CONFIDENT-IRETURN))
          
          (()                       (HALFPESO 0))
          (IAPPLY                   (HALFPESO  #x+4EF9))
          (()                       (EXTERNAL-ADDRESS IAPPLY))
          
          (()                       (HALFPESO 0))
          (LEXPR-SETUP              (HALFPESO  #x+4EF9))
          (()                       (EXTERNAL-ADDRESS LEXPR-SETUP))
          
          (()                       (HALFPESO 0))
          (INAPPLICABLE             (HALFPESO  #x+4EF9))
          (()                       (EXTERNAL-ADDRESS INAPPLICABLE))
          
          (()                       (HALFPESO 0))
          (COMPILER-LOSSAGE         (HALFPESO  #x+4EF9))          
          (()                       (EXTERNAL-ADDRESS COMPILER-LOSSAGE))

          (()                       (PESO 0))   ; Unused

          ;;Registers
          (STACK-LIMIT              (PESO 0))
          (HEAP-LIMIT               (PESO 0))   ; Initialized by kernel
          (JUMP-FROM                (PESO 0))   ; See ICALL, IRETURN
          ;; ... for CALL-XENOID
          (SAVED-HP                 (PESO 0))
          (SAVED-AP                 (PESO 0))
          (BITS                     (PESO 0))
          ;; BIT#0 is the "call-xenoid transition bit"
          ;; BIT#1 is the "machine fault in progress bit"
          (TEMPLATE-GUTS            (PESO %%TEMPLATE-LOW-PESO))

          ;; This belongs below, but snarfs up a formerly unused slot.
          (VFRAME-TEMPLATE          (EXTERNAL-ADDRESS VFRAME-TEMPLATE))

          ;; Masks:
          (ADDRESS-MASK             (PESO #x+FFFFFFF8))
          (LOW-11-BITS              (PESO #x+000007F8))
          (LOW-19-BITS              (PESO #x+0007FFF8)) ;For STRING-LENGTH
          (CHARACTER-MASK           (PESO #x+000000FF)) ; mask for all but byte

          ;; Random language-level constants:
          (TRUE                     (POINTER T))
          (NULL-CHARACTER           (POINTER #\NULL))
          (ESCAPE-PROCEDURE-TEMPLATE
              (EXTERNAL-ADDRESS ESCAPE-PROCEDURE-TEMPLATE))
          (SYMBOL-TEMPLATE          (EXTERNAL-ADDRESS SYMBOL-TEMPLATE))
          (VCELL-TEMPLATE           (EXTERNAL-ADDRESS VCELL-TEMPLATE))
          (VECTOR-TEMPLATE          (ADDRESS VECTOR-TEMPLATE))

          ;; Two data areas for my convenience in hand coding 
          (KERNEL-DATA              (EXTERNAL-ADDRESS KERNEL-DATA))
          (XENO-DATA                (EXTERNAL-ADDRESS XENO-DATA))

          ;; Kernel error messages -- should now be defunct
          (*RE-ENTRY-RETURN-FAULT* ;;"RE-ENTER-SCHEME returned"
           (VCELL-POINTER *RE-ENTRY-RETURN-FAULT*))

          (*HANDLE-INTERRUPT-FAULT* ;;"Cannot handle interrupts yet."
           (VCELL-POINTER *HANDLE-INTERRUPT-FAULT*))

          (*INAPPLICABLE-FAULT* ;;"trying to call an inapplicable datum"
           (VCELL-POINTER *INAPPLICABLE-FAULT*))

          (*COMPILER-LOSSAGE-FAULT* ;;"Incorrectly compiled code" 
           (VCELL-POINTER *COMPILER-LOSSAGE-FAULT*))

          (*ICALL-AP-ALIGNMENT-FAULT* ;;"AP illegally aligned"
           (VCELL-POINTER *ICALL-AP-ALIGNMENT-FAULT*))

          (*ICALL-RETURN-NOT-TEMPLATE-FAULT* ;;"Ret addr not a template"
           (VCELL-POINTER *ICALL-RETURN-NOT-TEMPLATE-FAULT*))

          (*ICALL-NOT-EXTEND-FAULT* ;;"Function isn't an EXTEND"
           (VCELL-POINTER *ICALL-NOT-EXTEND-FAULT*))

          (*ICALL-TEMPLATE-NOT-TEMPLATE-FAULT* ;;"Func. template not a template"
           (VCELL-POINTER *ICALL-TEMPLATE-NOT-TEMPLATE-FAULT*))

          (*ICALL-WRONG-NUMBER-ARGS-FAULT* ;;"Wrong number of args"
           (VCELL-POINTER *ICALL-WRONG-NUMBER-ARGS-FAULT*))

          (*ICALL-NOT-PROCEDURE-FAULT* ;;"Chunk not procedure"
           (VCELL-POINTER *ICALL-NOT-PROCEDURE-FAULT*))

          (*HEAP-OVERFLOW-FAULT* ;;"Heap space nearing exhaustion"
           (VCELL-POINTER *HEAP-OVERFLOW-FAULT*))

          (*IRETURN-BAD-SP-FAULT* ;;"SP badly aligned"
           (VCELL-POINTER *IRETURN-BAD-SP-FAULT*))

          (*IRETURN-BAD-SP-TEMPLATE-FAULT* ;;"Bad return address"
           (VCELL-POINTER *IRETURN-BAD-SP-TEMPLATE-FAULT*))

          (*IAPPLY-IMPROPER-LIST-FAULT* ;;"not a proper list"
           (VCELL-POINTER *IAPPLY-IMPROPER-LIST-FAULT*))

          (*STACK-BASE-FAULT* ;;"point of no return template encountered"
           (VCELL-POINTER *STACK-BASE-FAULT*))

          (*BB-FAULT* ;;"restart from debugger"
           (VCELL-POINTER *BB-FAULT*))

          (*CALL-XENOID-FAULT* ;;"call xenoid found bad/incompatible items"
           (VCELL-POINTER *CALL-XENOID-FAULT*))

          (*AEGIS-FAULT* ;;"T-asynchronous fault"
           (VCELL-POINTER *AEGIS-FAULT*))

          (()                       (PESO 0))   ; Unused

          (()                       (HALFPESO 0))
          (T-SYNCHRONOUS-FAULT      (HALFPESO  #x+4EF9))
          (T-SYNCHRONOUS-FAULT-LINK (EXTERNAL-ADDRESS T-SYNCHRONOUS-FAULT))

          (()                       (HALFPESO 0))
          (ZIPPY-ICALL              (HALFPESO  #x+4EF9))
          (ZIPPY-ICALL-LINK         (EXTERNAL-ADDRESS ZIPPY-ICALL))

          )))

;;; The Slink... 

(DEFINE *SLINK-TEMPLATE*
  (LAP-TEMPLATE SLINK-TEMPLATE C$SLINK-TEMPLATE ((DEFINEE *THE-SLINK*))
    (JMP (SLINK INAPPLICABLE))))

;;; (DEFINE *THE-SLINK* (XENOID-POINTER (XENOID "the_slink")))

(LAP (INITIAL-VALUE *THE-SLINK* THE-SLINK)
       )

(DEFINE-LOCAL-SYNTAX (DEFINE-SLINK-INDEX VAR EXP)
  `(DEFINE-CONSTANT ,VAR
     ',(COND ((AND (SYMBOL? EXP) (NUMBER? (GET EXP 'SLINK-OFFSET)))
              (DIV (GET EXP 'SLINK-OFFSET) 4))
             (ELSE (ERROR "bad SLINK-OFFSET property for ~S"
			  EXP)
		   0))))

(DEFINE-SLINK-INDEX %%ICALL-INDEX           ICALL-LINK)
(DEFINE-SLINK-INDEX %%PARANOID-ICALL-INDEX  PARANOID-ICALL-LINK)
(DEFINE-SLINK-INDEX %%CONFIDENT-ICALL-INDEX CONFIDENT-ICALL-LINK)
(DEFINE-SLINK-INDEX %%ZIPPY-ICALL-INDEX     ZIPPY-ICALL-LINK)

(DEFINE-SLINK-INDEX %%IRETURN-INDEX           IRETURN-LINK)
(DEFINE-SLINK-INDEX %%PARANOID-IRETURN-INDEX  PARANOID-IRETURN-LINK)
(DEFINE-SLINK-INDEX %%CONFIDENT-IRETURN-INDEX CONFIDENT-IRETURN-LINK)

(DEFINE-SLINK-INDEX %%IAPPLY-INDEX          IAPPLY)     ; useless?
(DEFINE-SLINK-INDEX %%TRUE-INDEX            TRUE)

(DEFINE-SLINK-INDEX %%JUMP-FROM-INDEX       JUMP-FROM)
(DEFINE-SLINK-INDEX %%HEAP-LIMIT-INDEX      HEAP-LIMIT)
(DEFINE-SLINK-INDEX %%SAVED-HP-INDEX        SAVED-HP)
