;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashassoc hashassoc-structure)

  (export make-hashassoc
          alist->hashassoc
          vector->hashassoc
          hashassoc?
          hashassoc-size
          hashassoc-equiv?
          key->depth->popmap
          hashassoc-trie

          hashassoc-empty?
          hashassoc-ref
          hashassoc-set!
          hashassoc-set-from-alist!
          hashassoc-insert!
          hashassoc-insert-from-alist!
          hashassoc-replace!
          hashassoc-replace-from-alist!

          hashassoc-delete!
          hashassoc-delete-from-list!

          hashassoc-set
          hashassoc-insert
          hashassoc-replace
          hashassoc-delete

          hashassoc->alist
          hashassoc->vector
          hashassoc->generator
          hashassoc-fold

          hashassoc-copy

          hashassoc-union
          hashassoc-add!
          hashassoc-intersection
          hashassoc-intersect!
          hashassoc-difference
          hashassoc-subtract!
          hashassoc-symmetric-difference
          hashassoc-disjoint?

          hashassoc=?
          hashassoc<?
          hashassoc<=?
          hashassoc>?
          hashassoc>=?)

  (import (scheme base)
          (scheme write) ;; For debugging.
          (scheme case-lambda))
  (import (hashassoc define-record-factory)
          (hashassoc low-level))
  (cond-expand
    (chicken-5 (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    ((library (srfi 1)) (import (srfi 1)))
    (loko (import (except (srfi :1 lists)
                           map for-each
                           assoc assv assq
                           member memv memq
                           list-copy list cons make-list)))
    (else (import (srfi srfi-1))))
  (cond-expand
    (chicken-5 (import (srfi 128)))
    ((library (scheme comparator)) (import (scheme comparator)))
    ((library (srfi 128)) (import (srfi 128)))
    (loko (import (srfi :128 comparators)))
    (else (import (srfi srfi-128))))
  (cond-expand
    (chicken-5 (import (srfi 143)))
    ((library (scheme fixnum)) (import (scheme fixnum)))
    ((library (srfi 143)) (import (srfi 143)))
    (loko (import (srfi :143 fixnums)))
    (else (import (srfi srfi-143))))
  (cond-expand
    (chicken (import (only (chicken bitwise) bitwise-and)))
    ((library (scheme bitwise)) (import (scheme bitwise)))
    ((library (srfi 151)) (import (srfi 151)))
    (loko (import (only (rnrs arithmetic bitwise (6))
                        bitwise-and)))
    (else (import (srfi srfi-151))))

  (begin

    (include "common/hashassoc/hashassoc-structure-implementation.scm")

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
