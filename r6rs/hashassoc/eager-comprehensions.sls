;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashassoc eager-comprehensions)

  (export :hashassoc-pairs
          :hashassoc-keys
          :hashassoc-values

          do-ec
          list-ec
          append-ec
          string-ec
          string-append-ec
          vector-ec
          vector-of-length-ec
          sum-ec
          product-ec
          min-ec
          max-ec
          any?-ec
          every?-ec
          first-ec
          last-ec
          fold-ec
          fold3-ec

          ;; We do not export dispatched generators. They have to be
          ;; made to work with a Scheme’s module system, and making
          ;; dispatched generators work with a module system
          ;; introduces breakage. Dispatched generators are
          ;; unnecessary, anyway, they are a lot of code to maintain,
          ;; and they impact performance and documentation.
          :list
          :string
          :vector
          :integers
          :range
          :real-range
          :char-range
          :port
          :do
          :let
          :parallel
          :while
          :until)

  ;; Some Schemes have SRFI-42 implementations that work with
  ;; extensions. Some other Schemes, have SRFI-42 implementations that
  ;; do not work or work only partially. So, instead of importing
  ;; SRFI-42, we include the SRFI-42 reference sources.
  (import (rename (rnrs (6)) (error r6rs-error))
          (only (rnrs r5rs (6)) exact->inexact)
          (hashassoc)
          (hashassoc hashassoc-include))

  (define (error message . arg*)
    (apply r6rs-error `(#f ,message . ,arg*)))

  (include "common/hashassoc/ec.scm")
  (include "common/hashassoc/eager-comprehensions-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
