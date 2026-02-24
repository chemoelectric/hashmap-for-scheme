;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

;;;-------------------------------------------------------------------

(define-syntax make-array-node
  (syntax-rules ()
    ((¶ population-map . entries*)
     (vector population-map . entries*))))

(define-syntax get-population-map
  (syntax-rules ()
    ((¶ node) (vector-ref node 0))))

(define-syntax get-entry
  (syntax-rules ()
    ((¶ node i) (vector-ref node (+ i 1)))))

;;;-------------------------------------------------------------------

(define-syntax population-map-bits-max
  ;;
  ;; Each node array is no bigger than 32 entries.
  ;;
  (syntax-rules ()
    ((¶) 5)))

(define-syntax hash-bits-exhausted?
  ;;
  ;; The size of the hash is not specified. When it is exhausted,
  ;; however, we revert to separate chaining.
  ;;
  (syntax-rules ()
    ((¶ bits) (fxnegative? bits))))

;;;-------------------------------------------------------------------
;;;
;;; The chains are association lists. However, they can be
;;; distinguished from a key-value cons-pair, because they start with
;;; a vector that cannot be an array node: its entry 0 is #f rather
;;; than a fixnum. The association list is entry 1 of the vector.
;;;

(define-syntax create-chain
  (syntax-rules ()
    ((¶ pair1 pair2)
     (vector #f (list pair1 pair2)))))

(define-syntax search-chain
  (syntax-rules ()
    ((¶ chain matches?)
     (let ((lst (vector-ref chain 1)))
       (find (lambda (pair) (matches? (car pair)))
             lst)))))

(define-syntax delete-from-chain!
  ;;
  ;; This macro returns either the same chain vector or a bare
  ;; key-value cons-pair.
  ;;
  (syntax-rules ()
    ((¶ chain matches?)
     (let* ((lst (vector-ref chain 1))
            (tl (find-tail (lambda (pair) (matches? (car pair)))
                           lst)))
       (if tl
         (let* ((next (cdr tl))
                (lst
                 (if (pair? next)
                   (begin
                     (set-car! tl (car next))
                     (set-cdr! tl (cdr next))
                     lst)
                   (drop-right! lst 1))))
           (if (pair? (cdr lst))
             (begin
               (vector-set! chain 1 lst)
               chain)
             (car lst)))
         chain)))))

;;;-------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
