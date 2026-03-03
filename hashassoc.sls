;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashassoc)

  (export make-hashassoc

          alist->hashassoc
          hashassoc->alist

          vector->hashassoc
          hashassoc->vector

          hashassoc->generator
          hashassoc-fold

          hashassoc?

          hashassoc-size
          hashassoc-empty?
          hashassoc-ref

          hashassoc-set!
          hashassoc-set-from-alist!

          hashassoc-delete!
          hashassoc-delete-from-list!)

  (import (hashassoc hashassoc-structure))

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
