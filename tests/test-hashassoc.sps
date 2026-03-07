#!/usr/bin/env scheme-script
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(import (except (rnrs base (6)) for-each map)
        (rnrs control (6))
        (rnrs programs (6))
        (rnrs io simple (6))
        (rnrs r5rs (6))
        (srfi :1 lists)
        (srfi :128 comparators)
        (hashassoc hashassoc-include)
        (hashassoc)
        (hashassoc eager-comprehensions))

(include "tests/test-hashassoc-implementation.scm")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
