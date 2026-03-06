;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashassoc eager-comprehensions)

  (export :hashassoc)

  (import (rnrs (6))
          (hashassoc)
          (hashassoc hashassoc-include)
          (srfi :42 eager-comprehensions))

  (include "common/hashassoc/eager-comprehensions-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
