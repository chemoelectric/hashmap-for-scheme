#!/usr/bin/env scheme-script
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(import (except (rnrs base (6)) for-each map)
        (rnrs programs (6))
        (rnrs io simple (6))
        (rnrs r5rs (6))
        (srfi :1 lists)
        (srfi :143 fixnums)
        (hashmap low-level)
        (hashmap hashmap-include))

(include "tests/test-hashmap-low-level-implementation.scm")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
