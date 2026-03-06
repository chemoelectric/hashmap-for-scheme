;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-syntax :hashassoc
  (syntax-rules (index)
    ((:hashassoc cc var arg)
     (:do cc
          (let ((gen! (hashassoc->generator arg))))
          ((pair (gen!)))
          (not (eof-object? pair))
          (let ((var pair)))
          #t
          ((gen!))))
    ((:hashassoc cc var (index i) arg)
     (:parallel cc (:hashassoc var arg) (:integers i)))))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
