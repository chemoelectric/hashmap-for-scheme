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
      (let loop ((node (hashmap-trie hm))
                 (depth -1))
        (cond
          ((pair? node)
           (let ((equiv? (hashmap-equiv? hm))
                 (k (car node)))
             (and (equiv? key k) node)))
          ((chain? node)
           (let* ((equiv? (hashmap-equiv? hm))
                  (matches? (lambda (k) (equiv? key k))))
             (search-chain node matches?)))
          (else
           (let* ((next-depth (fx+ depth 1))
                  (pm (depth->popmap next-depth)))
             (if (hash-bits-exhausted? pm)
               #f
               (let* ((mask (fx- pm 1))
                      (i (fxbit-count
                          (fxand mask (get-population-map
                                       node))))
                      (entry (get-entry node i)))
                 (if (not entry)
                   #f
                   (loop entry next-depth) ))))))))))

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
  (set-hashmap-trie! hm `(,key . ,value))
  (set-hashmap-size! hm 1)
  hm)
;;;   (let ((depth->popmap ((key->depth->popmap hm) key)))
;;;     (set-hashmap-trie! hm (make-array-node (depth->popmap 0)
;;;                                            `(,key . ,value)))
;;;     (set-hashmap-size! hm 1)
;;;     hm))

(define (insert-entry! hm key value)
  ;;
  ;;
  ;;
  (let* ((k->d->pm (key->depth->popmap hm))
         (depth->popmap (k->d->pm key)))
    (let loop ((node (hashmap-trie hm))
               (depth -1)
               (setter! (lambda (tr) (set-hashmap-trie! hm tr))))
      (define (start-chain)
        (let ((chain (create-chain `(,key . ,value) node)))
          (setter! chain)
          (set-hashmap-size! hm (+ (hashmap-size hm) 1))
          hm))
      (define (start-array-with-pair pm1 node1 pm2 node2)
        (let ((array (make-array-node (fxior pm1 pm2) node1 node2)))
          (setter! array)
          (set-hashmap-size! hm (+ (hashmap-size hm) 1))
          hm))
      (define (insert-at-pair)
        (let* ((next-depth (fx+ depth 1))
               (pm (depth->popmap next-depth)))
          (if (hash-bits-exhausted? pm)
            (start-chain)
            (let ((pm1 ((k->d->pm (car node)) next-depth)))
              (display pm1)(newline)
              (cond
                ((fx<? pm pm1)
                 (start-array-with-pair pm `(,key . ,value) pm1 node))
                ((fx<? pm1 pm)
                 (start-array-with-pair pm1 node pm `(,key . ,value)))
                (else
                 'FIXME))))))
      (define (insert-at-chain)
        (let* ((equiv? (hashmap-equiv? hm))
               (matches? (lambda (k) (equiv? key k)))
               (n (add-to-chain! node matches? `(,key . ,value))))
          (set-hashmap-size! hm (+ (hashmap-size hm) n))
          hm))
      (cond
        ((pair? node)
         (insert-at-pair))
        ((chain? node)
         (insert-at-chain))
        (else
         'FIXME)))
    hm))

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
