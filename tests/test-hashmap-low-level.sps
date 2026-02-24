#!/usr/bin/env scheme-script
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(import (except (rnrs base (6)) for-each map)
        (rnrs programs (6))
        (rnrs io simple (6))
        (srfi :1 lists)
        (srfi :143 fixnums)
        (srfi :64 testing)
        (hashmap low-level))

(define-syntax number-matches?
  (syntax-rules ()
    ((¶ n) (lambda (x) (= x n)))))

(test-begin "chains")
(let ((chain (create-chain '(1 . 2) '(3 . 4))))
  (test-equal #f (search-chain chain (number-matches? 0)))
  (test-equal '(1 . 2) (search-chain chain (number-matches? 1)))
  (test-equal '(3 . 4) (search-chain chain (number-matches? 3)))
  (test-equal chain (delete-from-chain! chain (number-matches? 0)))
  (test-equal '(3 . 4) (delete-from-chain! chain (number-matches? 1))))
(let ((chain (create-chain '(1 . 2) '(3 . 4))))
  (test-equal '(1 . 2) (delete-from-chain! chain (number-matches? 3))))
(test-end)

(let* ((test-result (test-runner-get))
       (failure-count (test-runner-fail-count test-result)))
  (if (zero? failure-count)
    (exit 0)
    (exit 1)))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
