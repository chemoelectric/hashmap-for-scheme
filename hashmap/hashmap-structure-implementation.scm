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

      (define (insert-at-chain chain)
        (let* ((equiv? (hashmap-equiv? hm))
               (matches? (lambda (k) (equiv? key k)))
               (number (add-to-chain! chain matches?
                                      `(,key . ,value))))
          (unless (fxzero? number)
            (set-hashmap-size! hm (+ 1 (hashmap-size hm))))
          hm))

      (let ((pm (depth->popmap depth)))
        (cond
          ((hash-bits-exhausted? pm)
           ;; ‘array’ is actually a chain, not an array.
           (insert-at-chain array))
          (else
           (let* ((popmap (get-population-map array))
                  (new-popmap (fxior pm popmap))
                  (mask (fx- pm 1))
                  (i (fxbit-count (fxand mask popmap))))

             (define (expand-the-current-array)
               (let* ((n (array-size array))
                      (array1 (make-vector (fx+ n 2))))
                 (set-population-map! array1 new-popmap)
                 (do ((j 0 (fx+ j 1)))
                     ((fx=? j i))
                   (set-entry! array1 j
                               (get-entry-quickly array j)))
                 (set-entry! array1 i `(,key . ,value))
                 (if (not (fx=? i n))
                   (do ((j i (fx+ j 1)))
                       ((fx=? j n))
                     (set-entry! array1 (fx+ j 1)
                                 (get-entry-quickly array j))))
                 (setter! array1)
                 hm))

             (define (insert-at-pair)
               (let* ((pair1 (get-entry-quickly array i))
                      (key1 (car pair1)))
                 (if ((hashmap-equiv? hm) key key1)
                   (begin ;; Replace the existing pair.
                     (set-entry! array i `(,key . ,value))
                     hm)
                   (grow-trie-at-pair
                    pair1 depth (lambda (elem)
                                  (set-entry! array i elem))))))

             (define (grow-trie-at-pair deepness pair1 setter!)
               (let* ((key1 (car pair1))
                      (kdpm (key->depth->popmap hm))
                      (deepness+1 (fx+ deepness 1))
                      (pm% ((kdpm key) deepness+1))
                      (pm1% (if (hash-bits-exhausted? pm%)
                              pm%
                              ((kdpm key1) deepness+1))))
                 (cond
                   ((hash-bits-exhausted? pm%)
                    (let ((chain (create-chain `(,key . ,value)
                                               pair1)))
                      (setter! chain)
                      (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
                      hm))
                   ((fx<? pm% pm1%)
                    (let ((array1
                           (make-array-node (fxior pm% pm1%)
                                            `(,key . ,value) pair1)))
                      (setter! array1)
                      (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
                      hm))
                   ((fx<? pm1% pm%)
                    (let ((array1
                           (make-array-node (fxior pm% pm1%)
                                            pair1 `(,key . ,value))))
                      (setter! array1)
                      (set-hashmap-size! hm (+ 1 (hashmap-size hm)))
                      hm))
                   (else
                    (let* ((array1 (make-vector 2))
                           (setter1! (lambda (elem)
                                       (set-entry! array1 0 elem))))
                      (set-population-map! array1 pm%)
                      (setter! array1)
                      (grow-trie-at-pair deepness+1 pair1
                                         setter1!))))))

             (define (increase-depth)
               (let* ((array1 (get-entry-quickly array i))
                      (depth1 (fx+ depth 1))
                      (setter1 (lambda (elem)
                                 (set-entry! array i elem))))
                 (loop array1 depth1 setter1)))

             (cond
               ((not (fx=? popmap new-popmap))
                (expand-the-current-array))
               ((pair? (get-entry-quickly array i))
                (insert-at-pair))
               ((chain? (get-entry-quickly array i))
                (insert-at-chain (get-entry-quickly array i)))
               (else (increase-depth))))))))))

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
