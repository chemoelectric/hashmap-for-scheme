;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashmap hashmap-structure)

  (export make-hashmap
          alist->hashmap
          hashmap?
          hashmap-size
          hashmap-equiv?
          key->bits
          hashmap-trie

          hashmap-empty?
          hashmap-set!
          hashmap-set-from-alist!)

  (import (except (rnrs base (6)) for-each map)
          (rnrs io simple (6)) ;; For debugging.
          (rnrs control (6))
          (rnrs syntax-case (6))
          (rnrs mutable-pairs (6))
          (srfi :1 lists)
          (srfi :42 eager-comprehensions)
          (srfi :143 fixnums)
          (hashmap hashmap-include)
          (hashmap define-record-factory)
          (hashmap low-level))

  (include "hashmap/hashmap-structure-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
