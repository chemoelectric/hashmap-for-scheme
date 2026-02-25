;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
;;;-------------------------------------------------------------------

(define (plist->alist lst)
  (let loop ((p lst)
             (q '())
             (r '())
             (qr #f))
    (if (null-list? p)
      (begin
        (when qr (error "expected pairs of elements" lst))
        (map cons (reverse! q) (reverse! r)))
      (if qr
        (loop (cdr p) q (cons (car p) r) (not qr))
        (loop (cdr p) (cons (car p) q) r (not qr))))))

(define-record-factory <hashmap>

  (constructor> make-hashmap
                (lambda (construct)
                  (lambda (equiv? hashfunc . rest*)
                    (let ((hm (construct
                               0 equiv?
                               (hashfunc->popmapfunc hashfunc) #f))
                          (alst (plist->alist rest*)))
                      (hashmap-set-from-alist! hm alst)))))

  (constructor> alist->hashmap
                (lambda (construct)
                  (lambda (equiv? hashfunc alst)
                    (let ((hm (construct
                               0 equiv?
                               (hashfunc->popmapfunc hashfunc) #f)))
                      (hashmap-set-from-alist! hm alst)))))

  (predicate> hashmap?)

  (getter> 1 hashmap-size)
  (setter> 1 set-hashmap-size!)

  (getter> 2 hashmap-equiv?)
  (getter> 3 key->depth->popmap)

  (getter> 4 hashmap-trie)
  (setter> 4 set-hashmap-trie!))

;;;-------------------------------------------------------------------

(define-syntax entry-index
  ;;
  ;; For an array node at a given depth, and a given depth->popmap
  ;; procedure (which is the representation of a key), find the
  ;; corresponding array entry.
  ;;
  (syntax-rules ()
    ((¶ depth->popmap node depth)
     (let* ((pm (depth->popmap depth))
            (mask (fx- pm 1)))
       (fxbit-count (fxand mask (get-population-map node)))))))

;;;-------------------------------------------------------------------

(define (hashmap-empty? hm)
  (zero? (hashmap-size hm)))

(define (hashmap-ref hm key)
  ;;
  ;; Returns either #f or a (key . value) cons-pair. The ‘key’ part of
  ;; the pair will be the key that was used to store the value, NOT
  ;; that which was used to retrieve it. However, the main reason to
  ;; return a pair instead of just the value is not to be able to
  ;; retrieve the storage-time key. It is so a value of #f can be
  ;; stored unambiguously and yet retrieved without the complications
  ;; needed in SRFI-125. (A reasonable alternative would be to have a
  ;; special unique object for ‘no result’.)
  ;;
  (if (hashmap-empty? hm)
    #f
    (let ((depth->popmap ((key->depth->popmap hm) key)))
      (let loop ((array (hashmap-trie hm))
                 (depth 0)
                 (pm (depth->popmap 0)))
        (if (hash-bits-exhausted? pm)
          #f
          (let* ((mask (fx- pm 1))
                 (i (fxbit-count
                     (fxand mask (get-population-map array))))
                 (entry (get-entry array i)))
            (cond
              ((not entry) #f)
              ((pair? entry)
               (let ((equiv? (hashmap-equiv? hm))
                     (k (car entry)))
                 (if (equiv? key k) entry #f)))
              ((chain? entry)
               (let* ((equiv? (hashmap-equiv? hm))
                      (matches? (lambda (k) (equiv? key k))))
                 (search-chain entry matches?)))
              (else
               (let ((next-depth (fx+ depth 1)))
                 (loop entry next-depth
                       (depth->popmap next-depth)))))))))))

(define hashmap-set!
  (case-lambda
    ((hm key value)
     (if (hashmap-empty? hm)
       (make-initial-trie! hm key value)
       (insert-entry! hm key value)))
    ((hm key1 value1 . rest*)
     (apply hashmap-set! (hashmap-set! hm key1 value1) rest*))
    ((hm) hm)))

(define (make-initial-trie! hm key value)
  (let* ((depth->popmap ((key->depth->popmap hm) key))
         (pm (depth->popmap 0))
         (leaf `(,key . ,value))
         (array (make-array-node pm leaf)))
    (set-hashmap-trie! hm array)
    (set-hashmap-size! hm 1)
    hm))

(define (insert-entry! hm key value)
  (let ((depth->popmap ((key->depth->popmap hm) key)))
    (let loop ((array (hashmap-trie hm))
               (depth 0)
               (setter! (lambda (trie)
                          (set-hashmap-trie! hm trie))))
      (let* ((pm (depth->popmap depth))
             (mask (fx- pm 1))
             (i (fxbit-count
                 (fxand mask (get-population-map array))))
             (entry (get-entry array i)))
        (define (append-to-array)
          (display "(append-to-array)")(newline)
          (let* ((n (array-size array))
                 (array1 (make-vector (fx+ n 2))))
            (set-population-map!
             array1 (fxior pm (get-population-map array)))
            (do ((j 0 (fx+ j 1)))
                ((fx=? j n))
              (set-entry! array1 j (get-entry-fast array j)))
            (set-entry! array1 n `(,key . ,value))
            (setter! array1)
            (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
            hm))
        (define (expand-before-pair)
          (display "(expand-before-pair)")(newline)
          (let* ((n (array-size array))
                 (array1 (make-vector (fx+ n 2))))
            (set-population-map!
             array1 (fxior pm (get-population-map array)))
            (do ((j 0 (fx+ j 1)))
                ((fx=? j i))
              (set-entry! array1 j (get-entry-fast array j)))
            (set-entry! array1 i `(,key . ,value))
            (do ((j i (fx+ j 1)))
                ((fx=? j n))
              (set-entry! array1 (fx+ j 1)
                          (get-entry-fast array j)))
            (setter! array1)
            (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
            hm))
        (define (expand-after-pair)
          (display "(expand-after-pair)")(newline)
          (let* ((n (array-size array))
                 (array1 (make-vector (fx+ n 2)))
                 (i+1 (fx+ i 1)))
            (set-population-map!
             array1 (fxior pm (get-population-map array)))
            (do ((j 0 (fx+ j 1)))
                ((fx=? j i+1))
              (set-entry! array1 j (get-entry-fast array j)))
            (set-entry! array1 i+1 `(,key . ,value))
            (do ((j i+1 (fx+ j 1)))
                ((fx=? j n))
              (set-entry! array1 (fx+ j 1)
                          (get-entry-fast array j)))
            (setter! array1)
            (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
            hm))
        (define (make-a-new-array-or-chain depth pair1 setter!)
          (display "(make-a-new-array-or-chain)")(newline)
          (let* ((key1 (car pair1))
                 (kdpm (key->depth->popmap hm))
                 (depth+1 (fx+ depth 1))
                 (pm% ((kdpm key) depth+1))
                 (pm1% (if (hash-bits-exhausted? pm%)
                         pm%
                         ((kdpm key1) depth+1))))
            (cond
              ((hash-bits-exhausted? pm%)
               (display " -------- make a new chain")(newline)
               (let ((chain (create-chain `(,key . ,value) pair1)))
                 (setter! chain)
                 (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
                 hm))
              ((fx<? pm% pm1%)
               (display " -------- make a new array of two pairs")(newline)
               (let ((array1 (make-array-node (fxior pm% pm1%)
                                              `(,key . ,value) pair1)))
                 (setter! array1)
                 (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
                 hm))
              ((fx<? pm1% pm%)
               (display " -------- make a new array of two pairs")(newline)
               (let ((array1 (make-array-node (fxior pm% pm1%)
                                               pair1 `(,key . ,value))))
                 (setter! array1)
                 (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
                 hm))
              (else
               (display " -------- make a new array of one array")(newline)
               (let* ((array1 (make-vector 2))
                      (setter1! (lambda (elem)
                                  (set-entry! array1 0 elem))))
                 (set-population-map! array1 pm%)
                 (make-a-new-array-or-chain depth+1 pair1 setter1!))))))
        (define (insert-at-pair)
          (let ((key1 (car entry)))
            ;; First test if the keys are equivalent. If so, no
            ;; hashing is necessary.
            (if ((hashmap-equiv? hm) key key1)
              (begin
                (set-entry! array i `(,key . ,value))
                hm)
              (let ((pm1 (((key->depth->popmap hm) key1) depth)))
                (cond
                  ((fx<? pm pm1) (expand-before-pair))
                  ((fx<? pm1 pm) (expand-after-pair))
                  (else (make-a-new-array-or-chain
                         depth entry
                         (lambda (elem)
                           (set-entry! array i elem)))))))))
        (cond
          ((not entry) (append-to-array))
          ((pair? entry) (insert-at-pair))
          (else 'FIXME))))))

(define (hashmap-set-from-alist! hm alst)
  (let loop ((p alst)
             (hm hm))
    (if (null-list? p)
      hm
      (loop (cdr p) (hashmap-set! hm (caar p) (cdar p))))))

;;;-------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
