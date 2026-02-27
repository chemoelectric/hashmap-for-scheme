#!/usr/bin/env scheme-r7rs
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (hashmap low-level))
(cond-expand
  ((library (scheme list)) (import (scheme list)))
  ((library (srfi 1)) (import (srfi 1)))
  (loko (import (srfi :1 lists)))
  (else (import (srfi srfi-1))))
(cond-expand
  ((library (scheme fixnum)) (import (scheme fixnum)))
  ((library (srfi 143)) (import (srfi 143)))
  (loko (import (srfi :143 fixnums)))
  (else (import (srfi srfi-143))))
(cond-expand
  ((library (srfi 64)) (import (srfi 64)))
  (loko (import (srfi :64 testing)))
  (else (import (srfi srfi-64))))

(include "tests/test-hashmap-low-level-implementation.scm")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
