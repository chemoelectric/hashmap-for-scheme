;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-record-factory <key-value>
  (constructor> make-key-value)
  (predicate> key-value?)
  (getter> 1 kv-key)
  (getter> 2 kv-value))

(define-syntax nodetag
  (syntax-rules (kv list array)
    ((¶ kv) 'kv)
    ((¶ list) 'list)
    ((¶ array) 'array)))

(define-record-factory <node>
  (constructor> make-node)
  (predicate> node?)
  (getter> 1 node-tag)
  (getter> 2 node-contents))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:

