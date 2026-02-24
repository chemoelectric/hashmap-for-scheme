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
  (let-values (((a b) (delete-from-chain! chain (number-matches? 0))))
    (test-equal chain a)
    (test-equal 0 b))
  (let-values (((a b) (delete-from-chain! chain (number-matches? 1))))
    (test-equal '(3 . 4) a)
    (test-equal -1 b)))
(let ((chain (create-chain '(1 . 2) '(3 . 4))))
  (let-values (((a b) (delete-from-chain! chain (number-matches? 3))))
    (test-equal '(1 . 2) a)
    (test-equal -1 b)))
(let* ((chain (create-chain '(1 . 2) '(3 . 4)))
       (n (add-to-chain! chain (number-matches? 1) '(1 . 5))))
  (test-equal (create-chain '(1 . 5) '(3 . 4)) chain)
  (test-equal 0 n))
(let* ((chain (create-chain '(1 . 2) '(3 . 4)))
       (n (add-to-chain! chain (number-matches? 3) '(3 . 5))))
  (test-equal (create-chain '(1 . 2) '(3 . 5)) chain)
  (test-equal 0 n))
(let* ((chain (create-chain '(1 . 2) '(3 . 4)))
       (n (add-to-chain! chain (number-matches? 5) '(5 . 6))))
  (test-equal 1 n)
  (test-equal '(1 . 2) (search-chain chain (number-matches? 1)))
  (test-equal '(3 . 4) (search-chain chain (number-matches? 3)))
  (test-equal '(5 . 6) (search-chain chain (number-matches? 5)))
  (test-equal #f (search-chain chain (number-matches? 7))))
(let* ((chain (create-chain '(1 . 2) '(3 . 4)))
       (n (add-to-chain! chain (number-matches? 5) '(5 . 6))))
  (let-values (((a b) (delete-from-chain! chain (number-matches? 5))))
    (test-equal 1 n)
    (test-equal -1 b)
    (test-equal (create-chain '(1 . 2) '(3 . 4)) a)))
(let* ((chain (create-chain '(1 . 2) '(3 . 4)))
       (n (add-to-chain! chain (number-matches? 5) '(5 . 6))))
  (let-values (((a b) (delete-from-chain! chain (number-matches? 1))))
    (test-equal 1 n)
    (test-equal -1 b)
    (test-equal (create-chain '(5 . 6) '(3 . 4)) a)))
(let* ((chain (create-chain '(1 . 2) '(3 . 4)))
       (n (add-to-chain! chain (number-matches? 5) '(5 . 6))))
  (let-values (((a b) (delete-from-chain! chain (number-matches? 3))))
    (test-equal 1 n)
    (test-equal -1 b)
    (test-equal (create-chain '(5 . 6) '(1 . 2)) a)))
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
