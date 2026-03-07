;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashassoc eager-comprehensions)

  (export :hashassoc)

  (export do-ec
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

  (import (scheme base))
  (import (scheme cxr))
  (import (scheme read))
  (import (scheme r5rs))
  (import (hashassoc))

  ;; Some Schemes have SRFI-42 implementations that work with
  ;; extensions. Some other Schemes, have SRFI-42 implementations that
  ;; do not work or work only partially. So only for listed Schemes is
  ;; SRFI-42 imported here. For others, the SRFI-42 reference sources
  ;; are included.
  (cond-expand
    ((or chibi chicken) (import (srfi 42)))
    (else))

  (begin

    (cond-expand
      ((or chibi chicken) (if #f #f))
      (else
       (include "common/hashassoc/ec.scm")
       ))

    (include "common/hashassoc/eager-comprehensions-implementation.scm")

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
