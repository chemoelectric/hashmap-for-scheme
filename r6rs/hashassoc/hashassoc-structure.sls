;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashassoc hashassoc-structure)

  (export make-hashassoc
          alist->hashassoc
          vector->hashassoc
          hashassoc?
          hashassoc-size
          hashassoc-equiv?
          key->depth->popmap
          hashassoc-trie

          hashassoc-empty?
          hashassoc-ref
          hashassoc-set!
          hashassoc-set-from-alist!
          hashassoc-insert!
          hashassoc-insert-from-alist!
          hashassoc-replace!
          hashassoc-replace-from-alist!

          hashassoc-delete!
          hashassoc-delete-from-list!

          hashassoc-set
          hashassoc-insert
          hashassoc-replace

          hashassoc->alist
          hashassoc->vector
          hashassoc->generator
          hashassoc-fold

          hashassoc-copy

          hashassoc-union
          hashassoc-add!
          hashassoc-intersection
          hashassoc-intersect!
          hashassoc-difference
          hashassoc-subtract!
          hashassoc-symmetric-difference
          hashassoc-disjoint?

          hashassoc=?
          hashassoc<?
          hashassoc<=?
          hashassoc>?
          hashassoc>=?)

  (import (rename (except (rnrs base (6))
                          for-each map vector-fill!
                          vector->list list->vector)
                  (error r6rs-error))
          (only (rnrs arithmetic bitwise (6))
                bitwise-and)
          (rnrs io simple (6))
          (rnrs control (6))
          (rnrs exceptions (6))
          (rnrs mutable-pairs (6))
          (srfi :1 lists)
          (srfi :128 comparators)
          (srfi :133 vectors)
          (srfi :143 fixnums)
          (hashassoc hashassoc-include)
          (hashassoc define-record-factory)
          (hashassoc low-level))

  (define (error msg . arg*)
    (apply r6rs-error (cons* #f msg arg*)))

  (include "common/hashassoc/hashassoc-structure-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
