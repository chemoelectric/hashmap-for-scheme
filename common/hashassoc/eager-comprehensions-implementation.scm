;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-syntax :hashassoc
  (syntax-rules (index)
    ((:hashassoc cc var (index i) arg)
     (:do cc
          (let ((gen! (hashassoc->generator arg))))
          ((i 0) (var (gen!)))
          (not (eof-object? var))
          (let ())
          #t
          ((+ i 1) (gen!))))
    ((:hashassoc cc var arg)
     (:do cc
          (let ((gen! (hashassoc->generator arg))))
          ((var (gen!)))
          (not (eof-object? var))
          (let ())
          #t
          ((gen!)))) ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
