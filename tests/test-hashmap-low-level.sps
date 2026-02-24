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
        (srfi :64 testing)
        (hashmap low-level))

(define hash-bits-limit
  (+ (quotient fx-width (hash-bits-chunk-max))
     (remainder fx-width (hash-bits-chunk-max))
     -1))

(define-syntax number-matches?
  (syntax-rules ()
    ((¶ n) (lambda (x) (= x n)))))

(test-begin "hashes")
(let ((hbsrc (make-hash-bits-source #b10101)))
  (test-equal #b10101 (hbsrc 0)))
(let ((hbsrc (make-hash-bits-source
              (fxarithmetic-shift
               #b10101 (hash-bits-chunk-max)))))
  (test-equal #b10101 (hbsrc 1)))
(let ((hbsrc (make-hash-bits-source
              (fxarithmetic-shift
               #b10101 (* 2 (hash-bits-chunk-max))))))
  (test-equal #b10101 (hbsrc 2)))
(let ((hbsrc (make-hash-bits-source
              (fxarithmetic-shift
               #b10101 (* 4 (hash-bits-chunk-max))))))
  (test-equal #b10101 (hbsrc 4)))
(let ((hbsrc (make-hash-bits-source
              (fxarithmetic-shift
               #b10101 (* (- hash-bits-limit 1)
                          (hash-bits-chunk-max))))))
  (test-assert (positive? (hbsrc (- hash-bits-limit 1)))))
(let ((hbsrc (make-hash-bits-source
              (fxarithmetic-shift
               #b10101 (* (- hash-bits-limit 1)
                          (hash-bits-chunk-max))))))
  (test-eq #f (hash-bits-exhausted? (hbsrc (- hash-bits-limit 1))))
  (test-eq #t (hash-bits-exhausted? (hbsrc hash-bits-limit))))
(test-end "hashes")

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
(test-end "chains")

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
