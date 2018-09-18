(herald (tsys exports t 127)
        (env tsys))

;;; Copyright 1983, 1984 Yale University

;;; Standard T syntax and value export lists.

;;; Syntax to be exported from system syntax table to base syntax table.

(declare-tsys-syntax-exports
 '(
   ;; Primitive special forms.  Everything not in this group is implemented
   ;; as a macro.
   quote
   if
   block
   lambda
   lset
   let-syntax
   define-local-syntax

   ;; MACROS
   define
   define-integrable
   define-constant
   set
   block0
   do
   iterate
   locative
   delay
   synonym
   unwind-protect
   with-output-to-string
   with-input-from-string
   with-output-to-list
   with-output-width-stream
   with-open-streams
   import
   locale
   require
   bound?                               ; Unreleased but handy
   catch
   ignore
   ignorable
   comment
   define-macro
   macro-expander
   define-syntax
   herald                               ; signals a syntax error

   ;; LET
   let
   labels
   bind
   destructure
   let*
   destructure*                         ; Candidate for future release
   bind*                                ; Candidate
   receive

   ;; COND
   cond
   xcond
   or
   and
   case
   xcase
   select
   xselect

   ;; OBJECT
   object
   operation
   define-operation
   define-settable-operation
   define-predicate
   define-methods                       ; Moribund

   ;; LOCATION
   modify-location
   modify
   swap
   exchange
   increment
   decrement
   push
   pop

   ;; DEFSTRUCT
   define-structure-type
   ))

;;; Pragmatics

(declare-tsys-syntax-exports
 '(trace
   untrace
   pp
   ))

;;; Internal unreleased special forms.

(declare-tsys-syntax-exports
 '(named-lambda
   the-environment
   var-locative
   variable-value
   set-variable-value
   define-variable-value
   if-integrated
   declare-setter
   gc-defer                         ; Foo.
   %handler                         ; Moribund
   %method
   define-structure-selector
   ))

;;; Values exported to base environment from T system environment.

(declare-tsys-exports
 '(
   ;; KERNEL
   apply
   string-equal?
   string-replace

   ;; SYSTEM
   check-arg
   make-vector
   copy-string
   vector-fill
   string->symbol

   ;; OPERATION
   join
   operation?
   procedure?
   setter
   print
   display

   ;; OPEN
   nil
   t
   else
   *number-of-char-codes*
   not
   false?
   true?
   boolean?
   always
   true
   false
   proj0
   proj1                                ; New.
   proj2                                ; ""
   proj3                                ; ""
   projn
   identity
   proclaim
   enforce
   null?
   atom?
   list?
   eof?
   symbol?
   vector?
   escape-procedure?                    ; Release this.
   set-vector-elt
   vector-elt
   char?
   char->ascii
   ascii->char
   string-tail
   string-nthtail

   ;; PCOMMON
   string?
   pair?
   flonum?
   car
   cdr
   string-length
   vector-length

   ;; PRIMOPS
   string-head
   string-elt
   chopy
   chopy!
   string-tail!
   string-nthtail!
   string-empty?
   cons

   ;; AEGIS
   *eof*
   sin
   cos
   tan
   asin
   acos
   atan
   exp
   log
   sqrt

   ;; M68PRIMOPS
   eq?
   neq?
   char=
   char<
   char>
   charn=
   char>=
   char<=

   ;; ERROR
   assert
   undefined-value
   undefined-effect
   error
   read-error                           ; New
   syntax-error                         ; New

   ;; LIST1 and LIST2
   null-list?
   proper-list?
   list
   cons*
   length
   mem?
   memq?
   nth
   nthcdr
   last
   lastcdr
   ass
   assq
   del
   delq
   del!
   delq!
   sublist
   copy-list
   append
   append!
   reverse
   reverse!

   ;; VECTOR
   vector-replace
   copy-vector
   list->vector
   vector->list
   vector-pos
   vector-posq
   walk-vector
   vector

   ;; CHARACTER
   char=ic                              ; Ought to be documented.
   char<ic
   char>ic
   charn=ic
   char>=ic
   char<=ic
   lowercase?
   uppercase?
   alphabetic?
   char-upcase
   char-downcase
   graphic?
   digit
   digit?
   char->digit
   digit->char
   control?                             ; Document?
   controlify
   uncontrolify

   ;; SYNTAX
   make-syntax-table                    ; New.
   make-empty-syntax-table              ; Newly exported in 2.9.
   syntax-descriptor?                   ; Document.  (2.6)
   macro-expander?                      ; Document.
   expand-macro-form                    ; New with 2.8.
   env-syntax-table
   syntax-table-entry
   macro-expand

   ;; POPULATE
   population?
   make-population
   add-to-population
   remove-from-population
   population->list
   walk-population                      ; Document.

   ;; HASH, TABLE
   object-hash
   object-unhash
   make-table                           ; 2.8
   table-entry                          ; 2.8

   ;; MISC
   symbol->string
   map!
   map
   mapcdr
   walk
   walkcdr
   *and
   *or
   *if
   disjoin
   conjoin
   complement
   compose
   contents
   locative?
   delay?
   force
   values		;!!
   receive-values

   ;; STRUCT
   structure?
   stype-predicator
   stype-constructor
   stype-selector
   stype-selectors
   stype-master
   stype-handler
   stype-id
   selector-id
   copy-structure
   copy-structure!
   make-stype
   stype-compatible?

   ;; POOL
   make-pool
   obtain-from-pool
   return-to-pool

   ;; CARCDR
   caar
   cadr
   cdar
   cddr
   caaar
   caadr
   cadar
   caddr
   cdaar
   cdadr
   cddar
   cdddr
   caaaar
   caaadr
   caadar
   caaddr
   cadaar
   cadadr
   caddar
   cadddr
   cdaaar
   cdaadr
   cdadar
   cdaddr
   cddaar
   cddadr
   cdddar
   cddddr

   ;; TREE
   generate-symbol
   equiv?
   subst
   substq
   substv
   alike?
   alikeq?
   alikev?
   copy-tree
   tree-hash

   ;; STRING
   make-string
   list->string
   string->list
   string-append
   string-slice
   substring
   walk-string
   map-string
   map-string!
   string-upcase
   string-downcase
   string-upcase!
   string-downcase!
   string-fill
   char->string
   string-posq

   ;; ALIASES
   vref
   vset
   char
   chdr
   chdr!
   nthchdr
   nthchdr!
   nthchar

   ;; ARITH
   integer?
   number?
   float?
   short-float?
   real?
   add
   subtract
   multiply
   divide
   div
   negate
   +
   -
   *
   /
   add1
   subtract1
   1+
   -1+
   less?
   not-less?
   equal?
   not-equal?
   greater?
   not-greater?
   <
   <=
   =
   n=
   >
   >=
   negative?
   zero?
   positive?
   not-negative?
   not-zero?
   not-positive?
   <0?
   =0?
   >0?
   >=0?
   n=0?
   <=0?
   remainder
   odd?
   even?
   logand                               ; Release ...
   logior
   logxor
   lognot
   ash
   integer-length
   bit-field
   set-bit-field                        ; ...
   max
   min
   abs
   expt
   gcd
   mod
   ->integer
   ->float

   ;; RATIO
   ratio?
   rational?
   numerator
   denominator

   ;; ENV
   locale?                              ; Document
   make-locale                          ; Document
   make-empty-locale
   *lset
   *define                              ; release?
   *value                               ; release?

   ;; EVAL
   eval
   standard-compiler
   run-compiled-code

   ;; SORT
   sort!
   sort

   ;; READTABLE
   read-table?
   read-table-entry
   whitespace?
   make-read-table
   *standard-read-table*                ; Document.
   *vanilla-read-table*                 ; Document.

   ;; READ
   *nothing-read*
   read-object
   read-refusing-eof
   make-dispatch-read-macro
   dispatch-syntax
   delimiting-read-macro?
   make-list-reader                     ; New
   list-terminator                      ; New

   ;; FORMAT
   format

   ;; PP
   pretty-print

   ;; STROPS
   stream?
   close
   maybe-open
   open
   file-exists?                         ; New.
   line-length
   input-stream?
   interactive-stream?
   read-char
   readc
   unread-char
   unreadc
   peek-char
   peekc
   read
   stream-read-table
   stream-filename
   read-line
   clear-input
   output-stream?
   write-char
   writec
   write-string
   writes
   write-line
   write-spaces
   write
   space
   newline
   fresh-line
   force-output
   hpos
   vpos
   print-width
   display-width

   ;; STREAMS
   string->input-stream
   read-objects-from-string
   make-broadcast-stream
   make-echo-stream
   print-width-greater?                 ; document?
   print-one-line                       ; document?
   concatenate-symbol
   ))

;;; Pragmatics

(declare-tsys-exports
 '(command-line                         ; Not *...* !
   *t-version-number*                   ; Document
   exit
   recklessness
   transcript-on
   transcript-off
   gc
   gc-noisily?                          ; Document
   gc-stats
   set-traced                           ; Internal
   set-untraced                         ; ""
   display-traced-objects
   untrace-traced-objects
   debug
   crawl
   *pp                                  ; ""
   *pp-symbol
   walk-symbols
   identification
   argspectrum
   structure-type
   *t-implementation-env*               ; !
   print-env-warnings?
   gc-noisily?
   system-loaded?

   ;; FS
   ->filename
   make-filename
   filename->string
   filename-fs
   filename-dir
   filename-name
   filename-type
   filename?
   local-fs
   fs-name
   aegis-fs?
   unix-fs?
   vms-fs?
   tops-20-fs?
   local-processor
   mc68000-processor?
   vax11-processor?
   pyramid-processor?

   ;; DEBUG
   where-defined
   get-environment                      ; Document
   disclose
   backtrace

   ;; LOAD
   load
   load-noisily
   load-noisily?
   print-load-message?

   ;; REPL
   breakpoint
   ret
   reset
   repl-results				; New in 2.9
   repl-prompt
   repl-read
   repl-eval
   repl-print
   repl-output                          ; release?
   repl-input                           ; ...
   repl-env
   repl-wont-print?
   *repl-wont-print*
   standard-prompt
   alternate-prompt                     ; document?

   ;; STANDARD
   terminal-input
   terminal-output
   standard-input
   standard-output
   error-output
   debug-output
   ))

(cond ((unix-os? (local-os))
       (declare-tsys-exports
        '(unix-shell-command
          unix-getenv
          stop)))
      ((vms-os? (local-os))
       (declare-tsys-exports
        '(stop))))

;;; Type specific stuff

(declare-tsys-exports
 '(*min-fixnum*
   *max-fixnum*
   fixnum?
   fixnum-add
   fixnum-subtract
   fixnum-logior
   fixnum-logand
   fixnum-logxor
   fixnum-multiply
   fixnum-divide
   fixnum-remainder
   fixnum-abs
   fixnum-min
   fixnum-max
   fixnum-negate
   fixnum-lognot
   fixnum-odd?
   fixnum-bit?
   fixnum-ashl
   fixnum-ashr
   fixnum-ash
   fixnum-length
   fixnum-expt
   fixnum-bit-field
   set-fixnum-bit-field
   fixnum-positive?
   fixnum-negative?
   fixnum-zero?
   fixnum-not-positive?
   fixnum-not-negative?
   fixnum-not-zero?
   fixnum-even?
   fixnum-equal?
   fixnum-less?
   fixnum-greater?
   fixnum-not-equal?
   fixnum-not-less?
   fixnum-not-greater?
   fx+
   fx-
   fx*
   fx/ 
   fx=
   fx<
   fx>
   fxn=
   fx>=
   fx<=
   fx1+
   fxrem
   flonum-add
   flonum-subtract
   flonum-multiply
   flonum-divide
   flonum-less?
   flonum-equal?
   flonum-greater?
   flonum-not-equal?
   flonum-not-less?
   flonum-not-greater?
   fixnum->flonum
   flonum->fixnum
   fl+
   fl-
   fl*
   fl/ 
   fl=
   fl<
   fl>
   fln=
   fl>=
   fl<=
   ))

;;; Symbol tables

(declare-tsys-exports
 '(make-symbol-table
   make-symbol
   walk-symbol-table
   intern
   really-intern
   interned
   interned?
   intern-symbol
   symbol-pname                         ; document
   *the-symbol-table*))

;;; Property lists

(declare-tsys-exports
 '(property
   set-property
   remove-property
   get
   put
   rem))

;;; List utilities

(declare-tsys-exports
 '(mem
   memq
   pos
   posq
   rass                                 ; Ugh.
   rassq
   memass
   memassq
   append-reverse
   append-reverse!
   any
   anycdr
   every
   everycdr
   any?
   anycdr?
   every?
   everycdr?
   circular?
   ))

;;; Required because of inadequate replacements

(declare-tsys-exports
 '(make-mutable-handler
   *define-method                       ; Internal; use define-methods instead
   set-default-handler                  ; Internal
   no-op                                ; For dealing with compiler bugs
   ))

;;; Required by macro expansions & integrable proc expansions
;;; Some of these will be renamed and released in the future.

(declare-tsys-exports
 '(bind-handler
   unwind-protect-handler
   make-old-stype
   %object
   %operation
   %settable-operation
   %predicate
   %state-op
   %dispatch-next
   %massage-default
   handle-operation
   cond-=>-aux
   or-aux
   *check-arg
   *enforce
   *assert
   make-macro-descriptor                ; For MACRO-EXPANDER
   *define-syntax                       ; For DEFINE-SYNTAX
   *the-environment
   call-with-current-continuation
   call-with-continuation   ; old name
   make-locative
   make-delay
   *locale
   *require
   with-open-streams-handler
   make-output-to-string-stream
   make-output-width-stream
   *nonvalue-hack*                      ; for BOUND?
   *symbol-template*
   *vector-template*
   *escape-procedure-template*
   **no-more-cond-clauses**
   **case-fell-off-end**
   **select-fell-off-end**
   **let-missing-initializer**
   **unbound-label**
   losing-xcond
   losing-xcase
   losing-xselect
   *values-vector-length*
   *values-vector*
   losing-receive
   ))
