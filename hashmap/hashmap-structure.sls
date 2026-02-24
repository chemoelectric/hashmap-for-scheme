;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashmap hashmap-structure)

  (export make-hashmap
          alist->hashmap
          hashmap?
          hashmap-size
          hashmap-equiv?
          key->depth->popmap
          hashmap-trie

          hashmap-empty?
          hashmap-ref
          hashmap-set!
          hashmap-set-from-alist!)

  (import (rename (except (rnrs base (6)) for-each map)
                  (error r6rs-error))
          (rnrs io simple (6)) ;; For debugging.
          (rnrs control (6))
          (rnrs syntax-case (6))
          (rnrs mutable-pairs (6))
          (srfi :1 lists)
          (srfi :143 fixnums)
          (hashmap hashmap-include)
          (hashmap define-record-factory)
          (hashmap low-level))

  (define (error msg . arg*)
    (apply r6rs-error (cons* #f msg arg*)))

  (include "hashmap/hashmap-structure-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
