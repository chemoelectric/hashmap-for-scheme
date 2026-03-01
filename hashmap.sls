;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashmap)

  (export make-hashmap
          alist->hashmap
          hashmap->alist

          hashmap?

          hashmap-size
          hashmap-empty?
          hashmap-ref

          hashmap-set!
          hashmap-set-from-alist!

          hashmap-delete!
          hashmap-delete-from-list!)

  (import (hashmap hashmap-structure))

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
