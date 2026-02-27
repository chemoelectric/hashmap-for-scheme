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
          hash-bits-max
          hash-bits-exhausted?
          make-hash-bits-source
          hashfunc->popmapfunc

          chain?
          create-chain
          search-chain
          add-to-chain!
          delete-from-chain!
          )

  (import (rnrs base (6))
          (rnrs io simple (6)) ;; For debugging.
          (rnrs control (6))
          (rnrs mutable-pairs (6))
          (only (srfi :1 lists) find find-tail drop-right!)
          (srfi :143 fixnums)
          (hashmap hashmap-include))

  (define-syntax hash-bits-chunk-max
    ;;
    ;; WARNING: This value is for a 64-bit architecture. For a 32-bit
    ;;          architecture, change the value to 4.
    ;;
    (syntax-rules ()
      ((¶) 5)))

  (define-syntax hash-bits-max
    ;;
    ;; We use the 60 least significant bits. This is the size of a
    ;; fixnum in Chez Scheme, and hopefully is as small as a fixnum
    ;; gets in a 64-bit system.
    ;;
    ;; WARNING: 32-bit architectures are not supported. Also if the
    ;;          size of a fixnum is less than 60, that is not
    ;;          supported. If necessary, change the number 60 to a
    ;;          lower number.
    ;;
    (syntax-rules ()
      ((¶) 60)))

  (include "hashmap/low-level-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
