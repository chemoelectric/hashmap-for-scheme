;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashassoc eager-comprehensions)

  (export :hashassoc)

  (import (scheme base))
  (import (hashassoc))
  (cond-expand
    (chicken-5 (import (srfi 42)))
    ((library (srfi 42)) (import (srfi 42)))
    (loko (import (srfi :42 eager-comprehensions)))
    (else (import (srfi srfi-42))))

  (begin

    (include "common/hashassoc/eager-comprehensions-implementation.scm")

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
