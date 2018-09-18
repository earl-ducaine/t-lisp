(herald (tsys reloc t 104)
        (env tsys)
        (pre-cook))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Dynamic object file relocator

;;; Horrible bug: if a GC happens in the middle of file relocation,
;;; very bad things will happen.

;;; In the best of all  possible worlds, it would be possible to run
;;; most of this code (all except that which must lock out consing)
;;; interpreted.  Let this provide a standard for cleanliness.

;;; This part of the loader is really only the "relocator" or "cooker",
;;; and should be mostly machine-independent.  The thing which actually
;;; hassles with the operating system to do the I/O and deal with the
;;; raw object file format is called LOAD-RAW-UNIT and can be found
;;; elsewhere.

;;; Problem: de-allocation of unused things; namely, the code for the
;;; initialization, string texts for symbols not freshly consed, scratch
;;; area.  Worry later.

;;; Fields within a CODE:
;;;  SIZE      - size of pure area,   in doublepesos
;;;  UNIT-SIZE - size of impure area, in pesos
;;;  SCRATCH-AREA-INDEX - in unit, index to scratch vector
;;;  STRUCTURE-AREA-INDEX - in unit, index for "structure area"
;;;  STRING-AREA-INDEX
;;; Other cruft in CODE:
;;;  Code objects for templates.
;;;  Strings texts for strings referred to by code.  Must be identifiable by
;;;   GC as interior to CODE; GC has to be able to "copy out" the CODE when
;;;   it comes on one of these texts.  Not yet hacked...
;;;  Constant flonums (same remark applies...).

;;; Fields within a UNIT:
;;;  CODE - pointer to the unit's CODE frob
;;;  THING - relocation item for initialization function or for
;;;    dumped object
;;;  SOURCE-FILE-NAME
;;; Other cruft:
;;;  Linkage area.  Pointers to symbols, vcells, quoted structure.
;;;    Also internally contains templates and tprocs.
;;;  Structure area.  Internal objects only: for now this means only
;;;    list cells, actually.  These get copied normally by GC.
;;;  String header area.  Headers get copied by GC.
;;;  String text area.  Texts get copied by GC.  Note!  To improve locality
;;;    of reference, headers & texts should be interspersed.  Not yet hacked.
;;;    In fact, at the moment, string texts are pure.  We need to figure out
;;;    how to distinguish pure from impure texts...
;;;  Scratch area.  This can be explicitly de-allocated after relocation.

(LSET *LOAD-DEBUG?* NIL)

(DEFINE (RELOCATE-UNIT UNIT ENV INITIAL?)
  (RELOCATE-UNIT-1 UNIT INITIAL?)
  (RELOCATE-UNIT-2 UNIT ENV INITIAL?))

;;; Perform the environment-independent relocation.

(DEFINE (RELOCATE-UNIT-1 UNIT INITIAL?)
  (SET (EXTEND-TEMPLATE UNIT) *UNIT-TEMPLATE*)
  (IF *LOAD-DEBUG?*
      (FORMAT *DEBUG-OUTPUT* "Relocating unit at #x~X...~%"
              (POINTER->FIXNUM UNIT)))
  (LET* ((CODE (UNIT-CODE UNIT))
         (WRONG? (AND INITIAL?
                      (EQ? (CODE-MAGIC-NUMBER CODE) %%INCORRECT-FASL)))
         (A (CODE-STRUCTURE-AREA-INDEX CODE))
         (B (CODE-STRING-AREA-INDEX CODE))
         (C (CODE-SCRATCH-AREA-INDEX CODE))
         (D (CODE-UNIT-SIZE CODE)))
    (IF (NOT WRONG?) (RELOCATE-STRING-AREA UNIT B C))
    (RELOCATE-AREA UNIT C D NIL WRONG?)
    (RELOCATE-AREA UNIT A B NIL WRONG?)
    ;; UGLY DEPENDENCY: HERALD and ENV slots precede slot 6.
    (RELOCATE-AREA UNIT 0 6 NIL WRONG?)
    ;; Flush this check when all old fasl files have disappeared.
    (COND ((UNIT-ENV UNIT)
           (ERROR "file ~S ought to be recompiled"
                  (UNIT-SOURCE-FILE-NAME UNIT))
           (SET (UNIT-ENV UNIT) NIL)))
    UNIT))

;;; Deal with the linkage area, which gets value cells stuck into it.

(DEFINE (RELOCATE-UNIT-2 UNIT ENV INITIAL?)
  (LET* ((CODE (UNIT-CODE UNIT))
         (WRONG? (AND INITIAL?
                      (EQ? (CODE-MAGIC-NUMBER CODE) %%INCORRECT-FASL)))
         (A (CODE-STRUCTURE-AREA-INDEX CODE)))
    (SET (UNIT-ENV UNIT) ENV)
    (RELOCATE-AREA UNIT 6 A ENV WRONG?)
    UNIT))

;;; LAST-STRING isn't a valid pointer.  Fix?

(DEFINE (RELOCATE-STRING-AREA UNIT FROM-INDEX TO-INDEX)
  (LET ((FIRST-STRING (MAKE-POINTER (%XLOC UNIT FROM-INDEX) %%STRING-TAG))
        (LAST-STRING  (MAKE-POINTER (%XLOC UNIT   TO-INDEX) %%STRING-TAG))
        (CODE (UNIT-CODE UNIT)))        ; Pure string texts for now.
    (DO ((STRING FIRST-STRING (POINTER-ADD STRING 1)))
        ((EQ? STRING LAST-STRING) T)
      (SET (STRING-POINTER STRING)
           (POINTER-ADD (STRING-POINTER STRING) CODE))
      (IF *LOAD-DEBUG?*
          (FORMAT *DEBUG-OUTPUT* "  String (hdr=#x~X ptr=#x~X) ~S~%"
                  (POINTER->FIXNUM STRING)
                  (POINTER->FIXNUM (STRING-POINTER STRING))
                  STRING))
      )))

;;; *** Relocation items: ***

;;; Each item in a relocation area is one of: an immediate datum (such as
;;; a character or fixnum), "vanilla," or a "special relocation item."
;;; Vanilla items get the address of the unit added to them during relocation.
;;; Immediate data is untouched.
;;; Special items are decoded according to their high 5 bits.

;;; For values of the %% constants see compiler source file "EMIT.T".

;;; Problems for the future: distinguishing different kinds of wired
;;; DEFINEs; vcells internal to unit; general environment path
;;; specifications.

(DEFINE-INTEGRABLE (REL-ITEM-TYPE ITEM)
  (POINTER-BIT-FIELD ITEM %%REL-ITEM-TYPE-FIELD-POS
                          %%REL-ITEM-TYPE-FIELD-SIZE))

(DEFINE-INTEGRABLE (REL-ITEM-INDEX ITEM)
  (POINTER-BIT-FIELD ITEM %%REL-ITEM-INDEX-FIELD-POS
                          %%REL-ITEM-INDEX-FIELD-SIZE))

;;; Relocate (sweep) an area within a unit.

(DEFINE (RELOCATE-AREA UNIT FROM-INDEX TO-INDEX ENV WRONG?)
  (LET ((CODE (UNIT-CODE UNIT)))
    (ITERATE LOOP ((I FROM-INDEX))
      (COND ((FX>= I TO-INDEX) T)
            (ELSE
             (IF *LOAD-DEBUG?*
                 (FORMAT *DEBUG-OUTPUT* "  #x~X: ~D: "
                         (POINTER->FIXNUM (%XLOC UNIT I))
                         I))
             (LET ((ITEM (XREF UNIT I)))
               (COND ((OR (NOT (REL-ITEM? ITEM))
                          (NOT (FX= (REL-ITEM-TYPE ITEM) %%TEMPLATE-REL-TYPE)))
                      (XSET UNIT I (RELOCATE-ITEM ITEM UNIT ENV WRONG?))
                      (LOOP (FX+ I 1)))
                     ((NOT (FIXNUM-ODD? I))
                      (ERROR "bad relocation vector at (XREF ~S ~S)" UNIT I))
                     ;; Worry about BIGNUMs and other internal objects someday
                     (ELSE
                      (LET ((TEM (MAKE-POINTER (%XLOC UNIT I) %%TEMPLATE-TAG))
                            (I+1 (FX+ I 1)))
                        (LET ((Z (XREF UNIT I+1)))
                          (XSET UNIT I (TEMPLATE-GUTS))
                          (SET (TEMPLATE-CHUNK TEM)
                               (IF WRONG? Z (POINTER-ADD Z CODE))))
                        (IF *LOAD-DEBUG?*
                            (FORMAT *DEBUG-OUTPUT* "template: chunk=#x~X~%"
                                    (POINTER->FIXNUM (TEMPLATE-CHUNK TEM))))
                        (LOOP (FX+ I+1 1)))))))))))

;;; Process a single word (peso) to be relocated.
;;; The "normal" thing to do is to simply add the address of the
;;; unit (i.e. of its template) to the item, but if the item is a gc-fwd,
;;; we decode the high bits to decide what to do with it.

(DEFINE (RELOCATE-ITEM ITEM UNIT ENV WRONG?)
  (SELECT (POINTER-TAG ITEM)
    ((%%FIXNUM-TAG)
     (IF *LOAD-DEBUG?*
         (FORMAT *DEBUG-OUTPUT* "fixnum ~S~%" ITEM))
     ITEM)
    ((%%MISC-TAG)
     (COND ((CHAR? ITEM)
            (IF *LOAD-DEBUG?*
                (FORMAT *DEBUG-OUTPUT* "character ~S~%" ITEM))
            ITEM)
           ((FX= (POINTER-ADDRESS ITEM) 100000.)
            (IF *LOAD-DEBUG?*
                (FORMAT *DEBUG-OUTPUT* "obsolete null type~%"))
            '())
           (ELSE
            (ERROR "relocation error - bad misc type #x~X"
                   (POINTER->FIXNUM ITEM)))))
    ((%%REL-ITEM-TAG)
     (LET ((INDEX (REL-ITEM-INDEX ITEM))
           (TYPE (REL-ITEM-TYPE ITEM)))
       (IF *LOAD-DEBUG?*
           (FORMAT *DEBUG-OUTPUT* "type=~D, index=~D: " TYPE INDEX))
       (SELECT TYPE
         ((%%FETCH-REL-TYPE)
          (IF *LOAD-DEBUG?*
              (FORMAT *DEBUG-OUTPUT* "fetch ~S~%"
                      (XREF UNIT INDEX)))
          (XREF UNIT INDEX))
         ((%%SYMBOL-REL-TYPE)
          (LET ((PNAME (CHANGE-TAG (POINTER-ADD UNIT INDEX)
                                   %%EXTEND-TAG
                                   %%STRING-TAG)))
            (LET ((SYM (REALLY-STRING->SYMBOL PNAME)))
              (IF *LOAD-DEBUG?*
                  (FORMAT *DEBUG-OUTPUT* "symbol ~S (pn=#x~X)~%"
                          SYM
                          (POINTER->FIXNUM PNAME)))
              SYM)))
         ((%%FREE-REL-TYPE)
          (IF *LOAD-DEBUG?*
              (FORMAT *DEBUG-OUTPUT* "free reference to ~S~%"
                      (XREF UNIT INDEX)))
          (RELOC-LOOKUP ENV (XREF UNIT INDEX) NIL))
         ((%%SET-REL-TYPE)
          (IF *LOAD-DEBUG?*
              (FORMAT *DEBUG-OUTPUT* "free reference to ~S (assigned)~%"
                      (XREF UNIT INDEX)))
          (CHECK-REBINDING (RELOC-LOOKUP ENV (XREF UNIT INDEX) NIL) NIL UNIT))
         ((%%LOCAL-REL-TYPE)            ; misnomer; should be DEFINE
          (IF *LOAD-DEBUG?*
              (FORMAT *DEBUG-OUTPUT* "definition: ~S~%" (XREF UNIT INDEX)))
          (CHECK-REBINDING (RELOC-LOOKUP ENV (XREF UNIT INDEX) T) T UNIT)) 
         ((%%LSET-REL-TYPE)
          (IF *LOAD-DEBUG?*
              (FORMAT *DEBUG-OUTPUT* "LSET: ~S~%"
                      (XREF UNIT INDEX)))
          (CHECK-REBINDING (RELOC-LOOKUP ENV (XREF UNIT INDEX) T) NIL UNIT))
         ((%%NULL-REL-TYPE)
          (IF *LOAD-DEBUG?* (FORMAT *DEBUG-OUTPUT* "null~%"))
          '())
         ((%%VTEM-REL-TYPE)
          (IF *LOAD-DEBUG?* (FORMAT *DEBUG-OUTPUT* "vector template~%"))
          *VECTOR-TEMPLATE*)
         ((%%INTEGER-REL-TYPE)
          (LET ((L (CHANGE-TAG (POINTER-ADD UNIT INDEX)
                               %%EXTEND-TAG
                               %%PAIR-TAG)))
            (IF *LOAD-DEBUG?* (FORMAT *DEBUG-OUTPUT* "integer ~S~%" L))
            (DO ((N 0 (+ (CAR Z) (* N 65536.)))
                 (Z (CDR L) (CDR Z)))
                ((NULL? Z) (* N (CAR L))))))
         ((%%TRUE-REL-TYPE)
          (IF *LOAD-DEBUG?* (FORMAT *DEBUG-OUTPUT* "true~%"))
          *TRUE-OBJECT*)
         ((%%FILENAME-REL-TYPE)
          (LET ((L (CHANGE-TAG (POINTER-ADD UNIT INDEX)
                               %%EXTEND-TAG
                               %%PAIR-TAG)))
            (IF *LOAD-DEBUG?* (FORMAT *DEBUG-OUTPUT* "filename ~S~%" L))
            (APPLY MAKE-FILENAME L)))

         ;; Future types, perhaps:
         ;;  - Fetch from scratch area.
         ;;  - Relative to structure area.
         ;;  - Relative to string area.
         ;;  - Bignum?  Ratio?
         ;;  - Fetch value from system env (?!).

         (ELSE
	  (RELOCATE-OTHER UNIT TYPE INDEX ENV)))))
    ((%%FLONUM-TAG)
     (COND (WRONG? ITEM)
           (ELSE
            (LET ((OBJ (POINTER-ADD ITEM (UNIT-CODE UNIT))))
              (IF *LOAD-DEBUG?*
                  (FORMAT *DEBUG-OUTPUT* "flonum: #x~X = ~S~%"
                          (POINTER->FIXNUM OBJ) OBJ))
              OBJ))))
    (ELSE
     (LET ((OBJ (POINTER-ADD ITEM (POINTER-ADDRESS UNIT))))
       (IF *LOAD-DEBUG?*
           (FORMAT *DEBUG-OUTPUT* "unit relative: #x~X~%"
                   (POINTER->FIXNUM OBJ)))
       OBJ))))

;;; Check to see whether a redefinition is happening.  Returns vcell argument.

(DEFINE (CHECK-REBINDING VC DFINING? WHO)
  (COND ((NONVALUE? (VCELL-CONTENTS VC)) NIL)   ; do nothing
        ((VCELL-INFO VC)
         (ENV-WARN (IF DFINING? "Redefining" "Assigning") (VCELL-ID VC)))
        (DFINING? 
         (ENV-WARN "Defining" (VCELL-ID VC))))
  (SET (VCELL-INFO VC) (IF DFINING? WHO NIL))
  VC)

(DEFINE (RELOC-LOOKUP ENV ID LOCAL?)
  (LET ((LOC (ENV-LOOKUP ENV ID LOCAL? T)))
    (COND ((VCELL? LOC) LOC)
          (ELSE
           (ERROR '("object files may not refer to non-LOCALE-bound variables"
                    "~%  (~S ~S ~S ~S ~S)")
                  'ENV-LOOKUP ENV ID LOCAL? T)
           (RELOC-LOOKUP ENV '**RELOC-LOOKUP-LOSSAGE** LOCAL?)))))
