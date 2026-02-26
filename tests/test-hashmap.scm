#!/usr/bin/env scheme-r7rs
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (hashmap))
(cond-expand
  ((library (scheme list)) (import (scheme list)))
  ((library (srfi 1)) (import (srfi 1)))
  (else (srfi srfi-1)))
(cond-expand
  ((library (scheme comparator)) (import (scheme comparator)))
  ((library (srfi 128)) (import (srfi 128)))
  (else (srfi srfi-128)))
(cond-expand
  ((library (srfi 42)) (import (srfi 42)))
  (else (srfi srfi-42)))
(cond-expand
  ((library (srfi 64)) (import (srfi 64)))
  (else (srfi srfi-64)))

(include "tests/test-hashmap-implementation.scm")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
