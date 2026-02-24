;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
;;;-------------------------------------------------------------------

(define-record-factory <hashmap>

  (constructor> make-hashmap
                (lambda (construct)
                  (lambda (equiv? hashfunc . alst)
                    (let ((hm (construct
                               0 equiv? (hashfunc->bitsfunc hashfunc)
                               #f)))
                      (hashmap-set-from-alist! hm alst)))))

  (constructor> alist->hashmap
                (lambda (construct)
                  (lambda (equiv? hashfunc alst)
                    (let ((hm (construct
                               0 equiv? (hashfunc->bitsfunc hashfunc)
                               #f)))
                      (hashmap-set-from-alist! hm alst)))))

  (predicate> hashmap?)

  (getter> 1 hashmap-size)
  (setter> 1 set-hashmap-size!)

  (getter> 2 hashmap-equiv?)
  (getter> 3 key->bits)

  (getter> 4 hashmap-trie)
  (setter> 4 set-hashmap-trie!))

;;;-------------------------------------------------------------------

(define (hashmap-empty? hm)
  (zero? (hashmap-size hm)))

(define hashmap-set!
  (case-lambda
    ((hm key value)
     (let ((sz (hashmap-size hm)))
       (if (zero? sz)
         (make-initial-trie! hm key value)
         (insert-entry! hm key value))))
    ((hm key1 value1 . rest*)
     (apply hashmap-set! (hashmap-set! hm key1 value1) rest*))
    ((hm) hm)))

(define (make-initial-trie! hm key value)
  (let ((bits (((key->bits hm) key) 0)))
    (set-hashmap-trie! hm (make-array-node bits `(,key . ,value)))
    (set-hashmap-size! hm 1)
    hm))

(define (insert-entry! hm key value)
  'FIXME
  hm)

(define (hashmap-set-from-alist! hm alst)
  (do-ec (:list pair alst)
         (:let key (car pair))
         (:let value (cdr pair))
         (hashmap-set! hm key value))
  hm)

;;;-------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
