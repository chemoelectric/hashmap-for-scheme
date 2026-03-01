;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashmap low-level)

  (export make-array-node
          get-population-map
          set-population-map!
          array-size
          get-entry
          get-entry-quickly
          set-entry!

          hash-bits-chunk-max
          hash-bits-max
          hash-bits-exhausted?
          make-hash-bits-source
          hashfunc->popmapfunc

          chain?
          create-chain
          search-chain
          add-to-chain!
          delete-from-chain!
          )

  (import (scheme base)
          (scheme case-lambda))
  (cond-expand
    (chicken-5 (import (srfi 1)))
    ((library (scheme list))(import (scheme list)))
    ((library (srfi 1)) (import (srfi 1)))
    (else (import (srfi srfi-1))))
  (cond-expand
    (chicken-5 (import (srfi 143)))
    ((library (scheme fixnum)) (import (scheme fixnum)))
    ((library (srfi 143)) (import (srfi 143)))
    (else (import (srfi srfi-143))))

  (begin

    (define-syntax hash-bits-chunk-max
      ;;
      ;; WARNING: This value is for eight-byte fixnums. For
      ;; four-byte fixnums, change the value to 4.
      ;;
      (syntax-rules ()
        ((¶) 5)))

    (define-syntax hash-bits-max
      ;;
      ;; We use the 60 least significant bits. This is the size of a
      ;; fixnum in Chez Scheme, and hopefully is as small as a fixnum
      ;; gets.
      ;;
      ;; WARNING: If necessary, change the number 60 to a lower
      ;; number.
      ;;
      (syntax-rules ()
        ((¶) 60)))

    (include "hashmap/low-level-implementation.scm")

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
