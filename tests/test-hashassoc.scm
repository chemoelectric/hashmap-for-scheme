#!/usr/bin/env scheme-r7rs
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (hashassoc)
        (hashassoc eager-comprehensions))
(cond-expand
  (chicken-5 (import (srfi 1)))
  ((library (scheme list)) (import (scheme list)))
  ((library (srfi 1)) (import (srfi 1)))
  (loko (import (srfi :1 lists)))
  (else (import (srfi srfi-1))))
(cond-expand
  (chicken-5 (import (srfi 128)))
  ((library (scheme comparator)) (import (scheme comparator)))
  ((library (srfi 128)) (import (srfi 128)))
  (loko (import (srfi :128 comparators)))
  (else (import (srfi srfi-128))))

(include "tests/test-hashassoc-implementation.scm")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
