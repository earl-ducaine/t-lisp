(herald linker)

;1. Instantiate each comex, one at a time.
;2. Build a vector containing the returned closures
;3. Backpatch the vector in to the global var *T-BOOT-PROCS*
;3. Hand nil to the vgc
;4. Hand the vector to the vgc
;5. Make the cymbals _*T-BOOT-PROCS* and _*T-BOOT-PROC* and install reloc thunks
;   for them.
;6. Install the base address' for the heaps
;7. Feed the lstate to write-link-file

(define (link comex-list stream)
  ;;Wire all the comex' together, and build a vector of their top-level procs
  (let* ((var-table   (make-table 'linker-var-table))
         (heap-table  (make-table 'linker-heap-table))
         (top-closures
           (list->vector
             (for (c in comex-list)
                  (save (instantiate-comex c var-table heap-table))))))

    ;;Backpatch in the toplevel closure vector as the value of the
    ;;free var *T-BOOT-PROCS*
    (var-definition '*T-BOOT-PROCS* top-closures var-table)

    (let ((reloc-table (make-table 'linker-reloc-table))
          (lstate (make-lstate)))
      (set (lstate-pi lstate) (make-area))
      (set (lstate-pd lstate) (make-area))
      (set (lstate-im lstate) (make-area))

      ;;Enter nil into the table.
      (vgc nil reloc-table heap-table lstate)
      ;;Root from the top level closures
      (vgc top-closures reloc-table heap-table lstate)

      ;;Install the base addresses for the heaps
      (set (area-base (lstate-pi lstate)) 0)
      (set (area-base (lstate-pd lstate))
           (area-frontier (lstate-pi lstate)))
      (set (area-base (lstate-im lstate)) 0)

      ;;Do it.
      (write-link-file stream lstate)))))

;5. Make the cymbals _*T-BOOT-PROCS* and _*T-BOOT-PROC* and install reloc thunks
;   for them.
