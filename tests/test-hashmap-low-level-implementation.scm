;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(display " ===== test-hashmap-low-level =====\n")

(define hash-bits-limit 12)

(define-syntax number-matches?
  (syntax-rules ()
    ((¶ n) (lambda (x) (= x n)))))

(include "tests/tests-common.scm")

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
  (test-assert (not (negative? (hbsrc (- hash-bits-limit 1))))))
(let ((hbsrc (make-hash-bits-source
              (fxarithmetic-shift
               #b10101 (* (- hash-bits-limit 1)
                          (hash-bits-chunk-max))))))
  (test-eq #f (hash-bits-exhausted? (hbsrc (- hash-bits-limit 1))))
  (test-eq #t (hash-bits-exhausted? (hbsrc hash-bits-limit))))

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

(display successes)
(display " successes\n")
(display failures)
(display " failures\n")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
