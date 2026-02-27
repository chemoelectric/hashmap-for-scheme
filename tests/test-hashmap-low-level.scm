#!/usr/bin/env scheme-r7rs
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (hashmap low-level))
(cond-expand
  (chicken-5 (import (srfi 1)))
  ((library (scheme list)) (import (scheme list)))
  ((library (srfi 1)) (import (srfi 1)))
  (loko (import (srfi :1 lists)))
  (else (import (srfi srfi-1))))
(cond-expand
  (chicken-5 (import (srfi 143)))
  ((library (scheme fixnum)) (import (scheme fixnum)))
  ((library (srfi 143)) (import (srfi 143)))
  (loko (import (srfi :143 fixnums)))
  (else (import (srfi srfi-143))))

(include "tests/test-hashmap-low-level-implementation.scm")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
