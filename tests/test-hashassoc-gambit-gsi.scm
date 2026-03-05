;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (hashassoc))
(import (srfi 1))
(import (srfi 42))
(import (srfi 69))

;; SRFI-128 from Snow.
;; snow-chibi install --impls gambit comparators
(import (comparators))

(include "test-hashassoc-implementation.scm")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: gambit
;;; coding: utf-8
;;; end:
