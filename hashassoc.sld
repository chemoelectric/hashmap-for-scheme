;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashassoc)

  (export make-hashassoc

          alist->hashassoc
          hashassoc->alist

          vector->hashassoc
          hashassoc->vector

          hashassoc-copy
          hashassoc->generator
          hashassoc-fold

          hashassoc?

          hashassoc-size
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

          hashassoc-union
          hashassoc-add!
          hashassoc-intersection
          hashassoc-intersect!
          hashassoc-difference
          hashassoc-subtract!
          hashassoc-symmetric-difference

          hashassoc=?)

  (import (hashassoc hashassoc-structure))

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
