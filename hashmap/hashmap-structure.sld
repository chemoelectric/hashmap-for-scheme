;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashmap hashmap-structure)

  (export make-hashmap
          alist->hashmap
          hashmap?
          hashmap-size
          hashmap-equiv?
          key->depth->popmap
          hashmap-trie

          hashmap-empty?
          hashmap-ref
          hashmap-set!
          hashmap-set-from-alist!)

  (import (except (scheme base)
                  assv assq assoc
                  append cons list make-list length
                  car cdr caar cadr cdar cddr
                  set-car! set-cdr!
                  for-each map list-copy list-ref
                  member memv memq null? pair? reverse)
          (scheme case-lambda))
  (import (hashmap define-record-factory)
          (hashmap low-level))
  (cond-expand
    ((library (scheme list)) (import (scheme list)))
    ((library (srfi 1)) (import (srfi 1)))
    (else (import (srfi srfi-1))))
  (cond-expand
    ((library (scheme fixnum)) (import (scheme fixnum)))
    ((library (srfi 143)) (import (srfi 143)))
    (else (import (srfi srfi-143))))

  (begin

    (include "hashmap/hashmap-structure-implementation.scm")

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
