(herald vgc)

;;; Virtual GC

(define-structure-type area         ;A.k.a. "heap"
  frontier      ;Address of next available cell
  objects       ;List of objects allocated
  reloc-items   ;List of relocation items
  reloc-count   ;Number of relocation items
  base          ;The address, relative to this heap's segment (text or data)
                ;that this heap begins at.
  )

(let ((master (stype-master area-stype)))
  (set (area-frontier    master) 0)
  (set (area-objects     master) '())
  (set (area-reloc-items master) '())
  (set (area-reloc-count master) 0))


(define (vgc root reloc-table heap-table lstate)
  ;;Bind. Gross. Probably be better to make the reloc table, etc be parts of
  ;;the lstate, and just always pass the lstate around.
  (bind ((*lstate* lstate)
         (*pi* (lstate-pi lstate))
         (*pd* (lstate-pd lstate))
         (*im* (lstate-im lstate))
         (*heap-table* heap-table))
    (get-descriptor root reloc-table)))


;;; define-vgc-handlers and get-type-method is how we do type dispatch.
;;; This could definitely be bummed in a later pass -- use subprimitives
;;; to extract type bits, and dispatch on that. Currently, we pay a linear
;;; scan for every dispatch, which isn't so great.

;;;(define-vgc-handlers (pred1 handler1) (pred2 handler2) ...) ==>
;;; (define vgc-pred1-frob-maker handler1) ...
;;; (define *vgc-selector-preds* (list pred1 pred2 ...))
;;; (define *vgc-selector-handlers* (list handler1 handler2 ...))

(define-local-syntax (define-vgc-handlers . clauses)
  (loop (initial (pred-name)    (handler-exp)       (pred-names '())
                 (handler-name) (handler-names '()) (handler-define-list '()))
        (for c in clauses)
        (next (pred-name (car c))
              (handler-exp (cadr c))
              (pred-names (cons pred-name pred-names))
              (handler-name (concatenate-symbol 'vgc- pred-name '-frob-maker))
              (handler-names (cons handler-name handler-names))
              (handler-define-list (cons `(define ,handler-name ,handler-exp)
                                         handler-define-list)))
        (result `(block ,@handler-define-list
                        (define *vgc-selector-preds*
                                (list . ,(reverse! pred-names)))
                        (define *vgc-selector-handlers*
                                (list . ,(reverse! handler-names)))))))


;;; Returns a 'descriptor' object for any object given the current relocation
;;; table. If the object has no relocated descriptor in the table, storage
;;; is allocated in one of the pseudo-heaps for the object via ALLOCATE.
;;; and a new descriptor object is made, installed, and returned.
(define (get-descriptor obj reloc-table)
  (cond ((table-entry obj reloc-table) =>
         identity)
        ((immediate? obj)
         (make-immediate-object-descriptor obj reloc-table))
        (else
         (allocate obj reloc-table))))

;;; ALLOCATE reserves space on an appropriate heap for obj, and
;;; associates the resulting descriptor object with obj in the
;;; relocation table.  It checks all of obj's children to ensure that
;;; they have descriptors in the relocation table (and are thus
;;; allocated), and generates relocation requests for all obj's slots
;;; that contain stored descriptors.
(define (allocate obj reloc-table)
  ((get-type-method obj) obj reloc-table (heap-select obj *heap-table*)))


;;; Scan down the predicates in *vgc-selector-preds*, looking for a predicate
;;; that answers true to obj. Return the corresponding handler from
;;; *vgc-selector-handlers*.

(define (get-type-method obj)
  (do ((preds    *vgc-selector-preds*   (cdr preds))
       (handlers *vgc-selector-handlers (cdr handlers)))
      ((if (null? preds)
           (error "No vgc handler for this type of object: ~a~%" obj)
           ((car preds) obj))
       (car handlers))))


;;; Here follow the various handlers for vgc'ing various type of objects.
(define-vgc-handlers

(pair? (lambda (pair reloc-table heap)
  (let* ((addr (area-frontier heap))
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (logior addr tag/pair)))
                 ((write-store self stream)
                  (write-descriptor (table-entry (cdr pair) reloc-table)
                                    stream)
                  (write-descriptor (table-entry (car pair) reloc-table)
                                    stream)))))
      (set (area-frontier heap) (fx+ addr 8))
      (push (area-objects heap) desc)
      (set (table-entry pair reloc-table) desc)
      ;;Trace from the cdr first to linearise lists
      (generate-slot-relocation (cdr pair) heap
                                addr reloc-table)
      (generate-slot-relocation (car pair) heap
                                (fx+ 4 addr) reloc-table)
      desc)))


(vector? (lambda (vec reloc-table heap)
  (let* ((addr (area-frontier heap))
         (nelts (vector-length vec))
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (logior addr tag/stored)))
                 ((write-store self stream)
                  ;;The header
                  (write-int stream (logior (ash nelts 8)
                                            (logior header/general-vector
                                                    tag/imm)))
                  (dotimes (i nelts)
                    (write-descriptor (table-entry (vref vec i) reloc-table)
                                      stream))))))
      (set (area-frontier heap)
           (fx+ addr (fx+ 4 (fx* 4 nelts))))
      (push (area-objects heap) desc)
      (set (table-entry vec reloc-table) desc)
      (loop (incr i .in 0 to nelts)
            (incr a in addr by 4)
            (do (generate-slot-relocation (vref vec i) heap a reloc-table)))
      desc)))


(unit-store? (lambda (unit reloc-table heap)
  (let* ((slots (unit-store-slots unit))
         (nslots (vector-length slots))
         (addr (area-frontier heap))
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (logior addr tag/stored)))
                 ((write-store self stream)
                  (dotimes (i nslots)
                    (let ((ob (vref slots i)))
                      ;;We have to special case closure-internal templates
                      (if (template-store? ob)
                          (if (= i (template-store-unit-slot ob))
                              (write-template-store stream ob reloc-table))
                          (write-descriptor (table-entry ob reloc-table)
                                            stream)))))))
    (set (area-frontier heap)
         (fx+ addr (fx* 4 nslots)))
    (push (area-objects heap) desc)
    (set (table-entry unit reloc-table) desc)
    (dotimes (i nslots)
      (let ((ob (vref slots i)))
        ;;We have to special case closure-internal templates
        (if (or (not (template-store? ob))
                (= i (template-store-unit-slot ob)))
            (generate-slot-relocation ob heap (fx+ addr (fx* 4 i))
                                      reloc-table)
            (generate-slot-relocation (template-store-code-vec ob)
                                      heap (fx+ 4 (fx+ addr (fx* 4 i)))
                                      reloc-table))))
    ;;What did we just do there for closure-internal-templates (represented in
    ;;a unit as a template-store struct)? We caused the code vector that
    ;;the auxiliary template points in to to be traced from, and we generated
    ;;a relocation item for the the auxiliary template pointer. Think about it.
    desc)))


(closure-desc? (lambda (cl reloc-table heap)
  (let* ((unit (closure-desc-unit cl))
         (unit-desc (get-descriptor unit reloc-table))
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self)
                  (fx+ (heap-offset unit-desc)
                       (closure-desc-offset cl)))
                 ((write-descriptor self stream)
                  (write-int stream
                             (logior (heap-offset self) tag/stored))))))

          (set (table-entry cl reloc-table) desc))))

(template-desc? (lambda (tmplt reloc-table heap)
  (assert (eq? *pi* heap))
  ;;first thing, allocate the code vector, if it hasn't been done already.
  (let* ((cv (get-descriptor (template-desc-code-vec tmplt) reloc-table))
         (desc (object ()
                 ((heap-stored self) (heap-stored cv)) ;better be *pi*!
                 ((heap-offset self)
                  (fx+ 4    ;Why add 4? The vector header is 4 bytes.
                       (fx+ (heap-offset cv) (template-desc-offset tmplt))))
                 ((write-descriptor self stream)
                  (write-int stream (logior (heap-offset self) tag/stored))))))
    (set (table-entry tmplt reloc-table) desc))))

(symbol? (lambda (sym reloc-table heap)
  (assert (eq? *pd* heap))
  (let* ((str (symbol->string sym))
         (slen (string-length str))
         (addr (area-frontier heap))
         (end-addr (fx+ 4 (fx+ addr slen)))
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (logior (heap-offset self) tag/stored)))
                 ((write-store self stream)
                  (write-int stream (logior (ash slen 8)
                                            (logior header/symbol tag/imm)))
                  (write-string stream str)
                  (dotimes (i (fx- (align end-addr 2) end-addr))
                    (write-byte stream 0))))))

    (set (area-frontier heap) (align end-addr 2))
    (set (table-entry sym reloc-table) desc))))

(bytev? (lambda (vec reloc-table heap)
  (let* ((addr (area-frontier heap))
         (vlen (bytev-length vec))   ;??? bytev-length ???
         (end-addr (fx+ 4 (fx+ addr vlen)))
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (logior addr tag/stored)))
                 ((write-store self stream)
                  (write-int stream (logior (ash vlen 8)
                                            (logior header/bytev tag/imm)))
                  (dotimes (i vlen)
                    (write-byte stream (bytev-elt vec i)))
                  ;;Pad to the next cell boundary.
                  (dotimes (i (fx- (align end-addr 2) end-addr))
                    (write-byte stream 0))))))
    (set (area-frontier heap) (align end-addr 2))
    (set (table-entry vec reloc-table) desc))))

(string? (lambda (str reloc-table heap)
  (let* ((addr (area-frontier heap))
         (text (string-text str))   ;???
         (tlen (char-vec-length text)) ;???
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (logior addr tag/stored)))
                 ((write-store self stream)
                  (write-int stream (logior (ash tlen 8)
                                            (logior header/slice tag/imm)))
                  (write-descriptor (table-entry text reloc-table) stream)
                  (write-int stream (string-offset str)))))) ;???
    (set (area-frontier) (fx+ addr 12))
    (set (table-entry str reloc-table) desc)
    (generate-slot-relocation text heap (fx+ addr 4) reloc-table)
    desc)))

(text? (lambda (vec reloc-table heap)
  (let* ((addr (area-frontier heap))
         (vlen (text-length vec))   ;??? text-length ???
         (end-addr (fx+ 4 (fx+ addr vlen)))
         (desc (object ()
                 ((heap-stored self) heap)
                 ((heap-offset self) addr)
                 ((write-descriptor self stream)
                  (write-int stream (logior addr tag/stored)))
                 ((write-store self stream)
                  (write-int stream (logior (ash vlen 8)
                                            (logior header/text tag/imm)))
                  (dotimes (i vlen)
                    (write-byte stream (bytev-elt vec i)))
                  ;;Pad to the next cell boundary.
                  (dotimes (i (fx- (align end-addr 2) end-addr))
                    (write-byte stream 0))))))
    (set (area-frontier heap) (align end-addr 2))
    (set (table-entry vec reloc-table) desc))))
    )

;;; CAVEAT DEBUGGER -- BETTER CHECK THE BYTE ORDER OF THIS SUCKER:

(define-constant VAX-JUMP-INSTRUCTION #o117427)     ;Obviously.

(define (write-template-store stream tmplt reloc-table)
  (write-half stream (ash (template-store-unit-slot tmplt) 2))
  (write-half stream (logior (ash (template-store-nptrs tmplt) 8)
                             (logand #o377 (template-store-nscratch tmplt))))
  (write-half stream (logior (ash (template-store-nargs tmplt) 8)
                             (logior (ash (template-store-h tmplt) 7)
                                     (logior (ash (template-store-i tmplt) 6)
                                             (logior (ash header/template 3)
                                                     tag/imm)))))
  (write-half stream VAX-JUMP-INSTRUCTION)
  ;;WARNING -- WARNING -- THIS IS NOT RIGHT. TOO BAD. YOU FIX IT.
  (write-int  stream (logior (template-store-offset tmplt) tag/stored)))

(define-constant N_UNDF 0)
(define-constant N_TEXT 4)
(define-constant N_DATA 6)
(define-constant N_EXT  1)

(define (heap-reloc-thunk slot-heap slot-address desc)
  (let* ((heap (heap-stored desc))
         (n_seg (cond ((or (eq? heap *pi*) (eq? heap *pd*)) N_TEXT)
                      ((eq? heap *im*)                      N_DATA)
                      (else     (error "heap ref not *pi*, *pd*, or *im*: ~a~%"
                                       heap)))))
    (lambda (stream)
      (write-int stream (fx+ (area-base slot-heap) slot-address))
      (write-int stream (logior n_seg (ash 2 25)))))) ; 2 = longword


(define (generate-slot-relocation obj slot-heap slot-address reloc-table)
  (cond ((not (immediate? obj))
         (push (area-reloc-items slot-heap)
               (heap-reloc-thunk slot-heap slot-address
                                 (get-descriptor obj reloc-table)))
         (increment (area-reloc-count slot-heap)))))

(define (immediate? obj)
  (or (character? obj) (fixnum? obj) (short-float? obj)))

(define (make-immediate-object-descriptor obj reloc-table)
  (set (table-entry reloc-table obj)
       (cond ((character? obj)
              (object ()
                ((write-descriptor self stream)
                 (write-int stream (logior (ash (char->ascii obj) 8)
                                           (logior header/char tag/imm))))))
             ((fixnum? obj)
              (object ()
                (write-descriptor self stream)
                (write-int stream (logior (ash obj 2) tag/fixnum))))
             ((short-float obj)
              (object ()
                ((write-descriptor self stream)
                 (error "Oops. Vgc has no method for writing short floats."))))
             (else
              (error
"Vgc has no method for handling immediate descriptors of this type: ~a"
                  obj)))))
