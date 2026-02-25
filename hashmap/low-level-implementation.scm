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

(define-syntax set-population-map!
  (syntax-rules ()
    ((¶ node value) (vector-set! node 0 value))))

(define-syntax array-size
  (syntax-rules ()
    ((¶ node) (fx- (vector-length node) 1))))

(define-syntax get-entry
  (syntax-rules ()
    ((¶ node i)
     (let ((i+1 (fx+ i 1)))
       (if (fx<=? (vector-length node) i+1)
         #f
         (vector-ref node i+1))))))

(define-syntax get-entry-fast
  (syntax-rules ()
    ((¶ node i) (vector-ref node (fx+ i 1)))))

(define-syntax set-entry!
  (syntax-rules ()
    ((¶ node i value) (vector-set! node (fx+ i 1) value))))

;;;-------------------------------------------------------------------

(define-syntax hash-bits-chunk-max
  (syntax-rules ()
    ((¶) (if (<= fx-width 32) 4 5))))

(define-syntax hash-bits-exhausted?
  ;;
  ;; The size of the hash is not specified. When it is exhausted,
  ;; however, we revert to separate chaining.
  ;;
  (syntax-rules ()
    ((¶ bits) (fxnegative? bits))))

(define-syntax make-hash-bits-source
  ;;
  ;; The algorithm can use any number of hash bits. For instance, one
  ;; could ensure that a hash were unique by including the key itself
  ;; in the hash. Scheme, however, usually comes with hash functions
  ;; that provide small integers as hash values. Let us assume such
  ;; hash values, and assume a particular constant bound on the number
  ;; of bits.
  ;;
  ;; We use the [fx-width - 1] least significant bits.
  ;;
  (syntax-rules ()
    ((¶ hash-value)
     (let* ((width-1 (- fx-width 1))
            (mask
             ;; The same value as fx-greatest, but, written this way,
             ;; more obviously a bit-mask.
             (bit-field-set 0 0 width-1))
            (hashval (bitwise-and mask hash-value)))
       (lambda (i)
         (let ((j (* i (hash-bits-chunk-max))))
           (if (<= width-1 j)
             -1 ;; Hash bits exhausted.
             (fxbit-field hashval j
                          (+ j (hash-bits-chunk-max))))))))))

(define (hashfunc->popmapfunc hf)
  (lambda (key)
    (let* ((hashval (hf key))
           (bits-source (make-hash-bits-source hashval)))
      (lambda (i)
        (let ((bits (bits-source i)))
          (if (hash-bits-exhausted? bits)
            bits
            (fxarithmetic-shift-left 1 bits)))))))

;;;-------------------------------------------------------------------
;;;
;;; The chains are association lists. However, they can be
;;; distinguished from a key-value cons-pair, because they start with
;;; a vector that cannot be an array node: its entry 0 is #f rather
;;; than a fixnum. The association list is entry 1 of the vector.
;;;

(define-syntax chain?
  (syntax-rules ()
    ((¶ vec) (eq? (vector-ref vec 0) #f))))

(define-syntax create-chain
  ;;
  ;; Create a new chain from two key-value cons-pairs.
  ;;
  (syntax-rules ()
    ((¶ pair1 pair2)
     (vector #f (list pair1 pair2)))))

(define-syntax search-chain
  ;;
  ;; Return either #f or the key-value cons-pair that matches.
  ;;
  (syntax-rules ()
    ((¶ chain matches?)
     (let ((lst (vector-ref chain 1)))
       (find (lambda (pair) (matches? (car pair)))
             lst)))))

(define-syntax add-to-chain!
  ;;
  ;; Either replaces an existing entry or inserts a new entry. Returns
  ;; the size change: 0 or 1.
  ;;
  (syntax-rules ()
    ((¶ chain matches? new-pair)
     (let* ((lst (vector-ref chain 1))
            (tl (find-tail (lambda (pair) (matches? (car pair)))
                           lst)))
       (if tl
         (begin
           (set-car! tl new-pair)
           0)
         (begin
           (vector-set! chain 1 (cons new-pair lst))
           1))))))

(define-syntax delete-from-chain!
  ;;
  ;; This macro returns either the same chain vector or a bare
  ;; key-value cons-pair. A second return value is the size change:
  ;; either 0 or -1.
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
               (values chain -1))
             (values (car lst) -1)))
         (values chain 0))))))

;;;-------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
