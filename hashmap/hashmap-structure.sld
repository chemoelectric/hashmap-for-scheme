;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashmap hashmap-structure)

  (export make-hashmap
          alist->hashmap
          vector->hashmap
          hashmap?
          hashmap-size
          hashmap-equiv?
          key->depth->popmap
          hashmap-trie

          hashmap-empty?
          hashmap-ref
          hashmap-set!
          hashmap-set-from-alist!

          hashmap-delete!
          hashmap-delete-from-list!

          hashmap->alist
          hashmap->vector
          hashmap->generator
          hashmap-fold)

  (import (scheme base)
          (scheme write) ;; For debugging.
          (scheme case-lambda))
  (import (hashmap define-record-factory)
          (hashmap low-level))
  (cond-expand
    (chicken-5 (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    ((library (srfi 1)) (import (srfi 1)))
    (else (import (srfi srfi-1))))
  (cond-expand
    (chicken-5 (import (srfi 128)))
    ((library (scheme comparator)) (import (scheme comparator)))
    ((library (srfi 128)) (import (srfi 128)))
    (else (import (srfi srfi-128))))
  (cond-expand
    (chicken-5 (import (srfi 143)))
    ((library (scheme fixnum)) (import (scheme fixnum)))
    ((library (srfi 143)) (import (srfi 143)))
    (else (import (srfi srfi-143))))
  (cond-expand
    (chicken (import (only (chicken bitwise) bitwise-and)))
    ((library (scheme bitwise)) (import (scheme bitwise)))
    ((library (srfi 151)) (import (srfi 151)))
    (else (import (srfi srfi-151))))

  (begin

    (include "hashmap/hashmap-structure-implementation.scm")

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
