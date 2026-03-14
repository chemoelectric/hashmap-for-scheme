;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashassoc low-level)

  (export make-array-node
          get-population-map
          set-population-map!
          array-size
          get-entry
          set-entry!

          hash-bits-chunk-max
          hash-bits-max
          hash-bits-exhausted?
          make-hash-bits-source
          hashfunc->popmapfunc

          chain?
          create-chain
          chain->alist
          alist->chain
          search-chain
          replace-in-chain!
          insert-in-chain!
          set-in-chain!
          delete-from-chain!
          copy-chain-with-replacement
          )

  (import (rnrs base (6))
          (rnrs io simple (6)) ;; For debugging.
          (rnrs control (6))
          (rnrs mutable-pairs (6))
          (only (srfi :1 lists) find find-tail drop-right! list-copy)
          (srfi :143 fixnums)
          (hashassoc hashassoc-include))

  (define-syntax hash-bits-chunk-max
    ;;
    ;; WARNING: This value is for eight-byte fixnums. For four-byte
    ;; fixnums, change the value to 4.
    ;;
    (syntax-rules ()
      ((¶) 5)))

  (define-syntax hash-bits-max
    ;;
    ;; We use the 60 least significant bits. This is the size of a
    ;; fixnum in Chez Scheme, and hopefully is as small as a fixnum
    ;; gets.
    ;;
    ;; WARNING: If necessary, change the number 60 to a lower number.
    ;;
    (syntax-rules ()
      ((¶) 60)))

  (define (vector-copy v)
    (let* ((n (vector-length v))
           (w (make-vector n)))
      (do ((i (fx- n 1) (fx- i 1)))
          ((fxnegative? i))
        (vector-set! w i (vector-ref v i)))
      w))

  (include "common/hashassoc/low-level-implementation.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
