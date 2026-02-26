;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashmap low-level)

  (export make-array-node
          get-population-map
          set-population-map!
          array-size
          get-entry
          get-entry-quickly
          set-entry!

          hash-bits-chunk-max
          hash-bits-exhausted?
          make-hash-bits-source
          hashfunc->popmapfunc

          chain?
          create-chain
          search-chain
          add-to-chain!
          delete-from-chain!
          )

  (import (except (rnrs base (6)) for-each map)
          (rnrs io simple (6)) ;; For debugging.
          (rnrs control (6))
          (rnrs syntax-case (6))
          (rnrs mutable-pairs (6))
          (srfi :1 lists)
          (srfi :143 fixnums)
          (only (srfi :151 bitwise-operations)
                bit-field-set bitwise-and)
          (hashmap hashmap-include))

  (include "hashmap/low-level-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
