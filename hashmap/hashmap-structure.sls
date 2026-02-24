;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashmap hashmap-structure)

  (export make-hashmap
          alist->hashmap
          hashmap?
          hashmap-count
          set-hashmap-count!
          hashmap-trie
          set-hashmap-trie!)

  (import (except (rnrs base (6)) for-each map)
          (rnrs io simple (6)) ;; For debugging.
          (rnrs control (6))
          (rnrs syntax-case (6))
          (rnrs mutable-pairs (6))
          (srfi :1 lists)
          (srfi :143 fixnums)
          (hashmap hashmap-include)
          (hashmap low-level))

  (include "hashmap/hashmap-structure-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
