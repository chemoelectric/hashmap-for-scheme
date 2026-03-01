;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define successes 0)
(define failures 0)

(define-syntax test-assert
  (syntax-rules ()
    ((¶ asserted)
     (let ((a ((lambda () asserted))))
       (if a
         (set! successes (+ successes 1))
         (begin
           (set! failures (+ failures 1))
           (display "failed: ")
           (display 'asserted)
           (newline)))))))

(define-syntax test-equal
  (syntax-rules ()
    ((¶ expected tested)
     (let ((e ((lambda () expected)))
           (t ((lambda () tested))))
       (if (equal? e t)
         (set! successes (+ successes 1))
         (begin
           (set! failures (+ failures 1))
           (display "failed: ")
           (display "(equal? ")
           (display 'expected)
           (display " ")
           (display 'tested)
           (display ")")
           (newline)))))))

(define-syntax test-eq
  (syntax-rules ()
    ((¶ expected tested)
     (let ((e ((lambda () expected)))
           (t ((lambda () tested))))
       (if (eq? e t)
         (set! successes (+ successes 1))
         (begin
           (set! failures (+ failures 1))
           (display "failed: ")
           (display "(eq? ")
           (display 'expected)
           (display " ")
           (display 'tested)
           (display ")")
           (newline)))))))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
