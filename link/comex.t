(herald comex)

(define-structure-type foreign-desc
  name)     ;the string that is the name of this procedure

(define-structure-type unit-store
  slots       ;the slots (including header) that this unit' storage requires.
  )

(define-structure-type closure-desc
  unit      ;the unit
  offset    ;and offset within unit where this closure descriptor points to.
  )

(define-structure-type template-desc
  code-vec  ;the code vector
  offset)   ;and offset within the vector where this template points to.

;;; Template stores encountered by the linker are *always* closure-internal
;;; templates. Hence their 'locatives' are unit slots.
(define-structure-type template-store
  nptrs     ;number of pointer slots in this template (-1 for super template)
  nscratch  ;number of scratch slots in this template (-1 for super template)
  nargs     ;number of args this template's fun takes
  h i       ;h and i bits
  code-vec  ;the code vector
  offset    ;and offset within the vector where the auxiliary template begins
  unit-slot ;The unit slot this template store begins on.
  )

(define-structure-type undefined-desc)

(define-structure-type var-node
  name          ;the symbol that is this var's name
  refs          ;a list of (unit . slot) pairs giving the unit slots where this
                ;var's value is kept.
  backpatches   ;a (unit . slot) list of unit slots waiting to be filled with
                ;this var's definition, when it becomes available.
  defined?      ;T iff this var has been defined.
  value)        ;this var's value (if defined).

(let ((node (stype-master var-node-stype)))
  (set (var-node-refs node)
       (set (var-node-backpatches node)
            (set (var-node-defined? node) '()))))

(define (instantiate-comex comex var-table heap-table)
  (let* ((obs (comex-objects comex))
         (ops (comex-opcodes comex))
         (code-vec (comex-code comex))
         (unit-len (vector-length obs))
         (unit-vec (make-vector unit-len))
         (unit (make-unit-store)))
    (do ((i (-1+ unit-len) (-1+ i)))
        ((< i 0))
        (let ((op (vref opcodes i))
              (ob (vref objects i)))
          (set (vref unit-vec i)
               (select op
                 ((op/literal)
                  (set (table-entry heap-table ob) 'pure)
                  ob)
                 ((op/foreign)
                  (let ((new (make-foreign-desc)))
                    (set (foreign-desc-name new) ob)
                    new))
                 ((op/closure)
                  (let ((new (make-template-desc)))
                    (set (template-desc-code-vec new) code-vec)
                    (set (template-desc-offset new) ob)
                    new))
                 ((op/template1)
                  (let ((new (make-template-store)))
                    (set (template-store-unit-slot new) (1+ i))
                    (set-template-store-slots new code-vec ob)
                    new))
                 ((op/template2) (vref unit-vec (-1+ i)))
                 ((op/template3) (vref unit-vec (-1+ i)))
                 ((op/definition)
                  ;;Make the closure that is the value of the defined var,
                  ;;and register the definition in the var table.
                  (let ((new (make-closure-desc)))
                    (set (closure-desc-unit new) unit)
                    (set (closure-desc-offset new) i)
                    (var-definition (car ob) new var-table))
                  ;;Make the template desc that is the closure's header, and
                  ;;return it (so that it will be put in the current unit slot)
                  (let ((new (make-template-desc)))
                    (set (template-desc-code-vec new) code-vec)
                    (set (template-desc-offset new) (cdr ob))
                    new))

                 ((stored-definition)
                  ;;put the current unit slot on the var's refs list.
                  (add-to-var-refs (car ob) unit i var-table)
                  ;;make the closure that is the value of the defined var,
                  ;;and register the definition in the var table
                  (let ((new (make-closure-desc)))
                    (set (closure-desc-unit new) unit)
                    (set (closure-desc-offset new) (cdr ob))
                    (var-definition (car ob) new var-table)
                    ;;the current slot was backpatched by var-definition, but
                    ;;we must redundantly set it by returning the closure desc.
                    new))
                 ((defined)
                  (add-to-var-refs ob unit i var-table))
                 ((lset)
                  (add-to-var-refs ob unit i var-table))
                 ((free)
                  (add-to-var-refs ob unit i var-table))
                 ((special-literal))
                 ((nonlocal))
                 (else (error "illegal opcode ~a" op)))))

        ;;Make and install the top level proc for the unit
        (let ((new (make-closure-desc)))
          (set (closure-desc-unit   new) unit)
          (set (closure-desc-offset new) (comex-top-proc-offset comex))
          (set (unit-store-top-closure unit) new))

        ;;Install the env vec for the unit
        (set (unit-store-env-vec unit) env-vec)

        ;;Register the unit' env vector as going in impure space, and
        ;;the unit' code vector as going in pure pi space.
        (set (table-entry heap-table code-vec) 'pi)
        (set (table-entry heap-table env-vec)  'impure)

        unit)))


;;; free-var is a var that occurs free to some compiled expression.
;;; Var-definition registers VALUE as the value of the free var in the
;;; var table. VALUE is then backpatched into all unit slots that have
;;; been previously defined to be storage cells for the free var.
;;; Var-definition returns VALUE.

(define (var-definition free-var value var-table)
  ;;look up the var node in the var table (allocate the node if it isn't there)
  (let ((node (cond ((table-entry var-table free-var) => identity)
                    (else
                     (set (table-entry var-table free-var)
                          (let ((new (make-var-node)))
                            (set (var-node-name new) free-var)
                            new))))))
    ;; If the node has already been defined, something is funny. Generate
    ;; a warning, then flush the current def, and use current value for future
    ;; refs.
    (cond ((var-node-defined? node)
           (warning "Multiply defined free var: ~a" free-var)))

    ;;backpatch in the def to all the pending unit slots
    (walk (lambda (x) (set (vref (unit-store-env-vec (car x)) (cdr x))
                           value))
          (var-node-backpatches node))
    (set (var-node-backpatches node) '())
    ;;save the def away in the var node, and return it.
    (set (var-node-defined? node) t)
    (set (var-node-value node) value)))


;;; Add-to-var-refs adds another unit slot to some free variable's
;;; refs list. If the var has a definition, courtesy of some previously
;;; encountered (define ...), then the definition is returned.  Otherwise,
;;; the unit slot is added to the var's backpatch list, to be set when
;;; the linker encounters the definition.

(define (add-to-var-refs free-var unit index var-table)
  ;;look up the var node in the var table (allocate the node if it isn't there)
  (let ((node (cond ((table-entry var-table free-var) => identity)
                    (else
                     (set (table-entry var-table free-var)
                          (let ((new (make-var-node)))
                            (set (var-node-name new) free-var)
                            new))))))
    (cond ((var-node-defined? node)
           (set (vref (unit-store-env-vec unit) index)
                (var-node-value node)))
          (else
           (push (var-node-backpatch node) (cons unit index))
           nil))))

;;; fetch the template store slots out of the closure-internal-template's
;;; auxiliary template.
(define (set-template-store-slots ts code index)
  (set (template-store-nptrs ts)    (bref-8 code (-1+ index)))
  (set (template-store-nscratch ts) (bref-8 code (- index 2)))
  (set (template-store-nargs ts)    (bref-8 code (1+ index)))
  (let ((low-byte (bref-8 code index)))
    (set (template-store-h ts) (bit-field low-byte 7 1))
    (set (template-store-i ts) (bit-field low-byte 6 1)))
  (set (template-store-code-vec ts) code)
  (set (template-store-offset ts) index))


;;; So, what's the plan? Instantiate all the comex', one by one, thus wiring
;;; them all together. Each comex instantiation will return one unit-store
;;; struct. Then build a vector containing each unit's top-level closure.
;;; Backpatch the vector into the the global var *T-BOOT-FUNS*. Then
;;; lookup in the global table the global closure *T-BOOTER*. Pass that,
;;; and the boot funs vector off to the virtual gc. Ta da.
