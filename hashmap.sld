;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashmap)

  (export make-hashmap
          alist->hashmap

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
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
