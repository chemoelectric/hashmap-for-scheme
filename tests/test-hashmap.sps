#!/usr/bin/env scheme-script
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(import (except (rnrs base (6)) for-each map)
        (rnrs programs (6))
        (rnrs io simple (6))
        (rnrs r5rs (6))
        (srfi :1 lists)
        (srfi :42 eager-comprehensions)
        (srfi :64 testing)
        (srfi :128 comparators)
        (hashmap))

(define (make-string-hashmap . arg*)
  (apply make-hashmap (cons* string=? string-hash arg*)))

(define (alist->string-hashmap alst)
  (alist->hashmap string=? string-hash alst))

(test-begin "big alist of strings -> hashmap")
(let* ((alst (list-ec (:range i 1 1000001)
                      (:let s (number->string i 16))
                      (:let pair `(,s . ,i))
                      pair))
       (hm (alist->string-hashmap alst)))
  (test-assert (every?-ec (:list pair alst)
                          (:let s (car pair))
                          (:let i (cdr pair))
                          (:let pair% (hashmap-ref hm s))
                          (:let s% (car pair%))
                          (:let i% (cdr pair%))
                          (and (string=? s s%) (= i i%)))))
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
