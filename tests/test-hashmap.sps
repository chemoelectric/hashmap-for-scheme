#!/usr/bin/env scheme-script
;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (define-record-factory))

(define-syntax include
  ;;
  ;; From the R⁶RS library report.
  ;;
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ((p (open-file-input-port
                  fn
                  (file-options)
                  (buffer-mode block)
                  (native-transcoder))))
          (let f ((x (get-datum p)))
            (if (eof-object? x)
              (begin (close-port p) '())
              (cons (datum->syntax k x)
                    (f (get-datum p))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax->datum (syntax filename))))
         (with-syntax (((exp ...)
                        (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

(include "hashmap.scm")

(test-begin "key-value")
(test-equal
  (kv-key (make-key-value "key" "value"))
  (kv-key (make-key-value "key" "value")))
(test-equal
  (kv-value (make-key-value "key" "value"))
  (kv-value (make-key-value "key" "value")))
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
