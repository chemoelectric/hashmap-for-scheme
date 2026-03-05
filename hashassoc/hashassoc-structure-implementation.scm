;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
;;;-------------------------------------------------------------------

(define-record-factory <hashassoc>

  (constructor> construct-hashassoc)

  (constructor>
   make-hashassoc
   (lambda (construct)
     (lambda (arg1 arg2 . rest*)
       (cond
         ((comparator? arg1)
          (unless (comparator-hashable? arg1)
            (error "expected a hashable comparator" arg1))
          (let* ((cmp arg1)
                 (rest* (cons arg2 rest*))
                 (hm (construct
                      0 (comparator-equality-predicate cmp)
                      (comparator->popmapfunc cmp) #f))
                 (alst (plist->alist rest*)))
            (hashassoc-set-from-alist! hm alst)))
         (else
          (let* ((equiv? arg1)
                 (hashfunc arg2)
                 (hm (construct 0 equiv?
                                (hashfunc->popmapfunc hashfunc) #f))
                 (alst (plist->alist rest*)))
            (hashassoc-set-from-alist! hm alst)))))))

  (constructor>
   alist->hashassoc
   (lambda (construct)
     (case-lambda
       ((equiv? hashfunc alst)
        (let ((hm (construct 0 equiv?
                             (hashfunc->popmapfunc hashfunc) #f)))
          (hashassoc-set-from-alist! hm alst)))
       ((cmp alst)
        (unless (and (comparator? cmp)
                     (comparator-hashable? cmp))
          (error "expected a hashable comparator" cmp))
        (let ((hm (construct 0 (comparator-equality-predicate cmp)
                             (comparator->popmapfunc cmp) #f)))
          (hashassoc-set-from-alist! hm alst))))))

  (constructor>
   vector->hashassoc
   (lambda (construct)
     (case-lambda
       ((equiv? hashfunc vec)
        (let ((hm (construct 0 equiv?
                             (hashfunc->popmapfunc hashfunc) #f)))
          (fill-from-vector! hm vec)))
       ((cmp vec)
        (let ((hm (construct 0 (comparator-equality-predicate cmp)
                             (comparator->popmapfunc cmp) #f)))
          (fill-from-vector! hm vec))))))

  (predicate> hashassoc?)

  (getter> 1 hashassoc-size)
  (setter> 1 set-hashassoc-size!)

  (getter> 2 hashassoc-equiv?)
  (getter> 3 key->depth->popmap)

  (getter> 4 hashassoc-trie)
  (setter> 4 set-hashassoc-trie!))

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

(define (comparator->popmapfunc cmp)
  (let ((hash (comparator-hash-function cmp)))
    (hashfunc->popmapfunc
     (lambda (obj)
       (bitwise-and (hash obj) fx-greatest)))))

(define (fill-from-vector! hm vec)
  (let ((n (vector-length vec)))
    (do ((i 0 (fx+ i 1)))
        ((fx=? i n))
      (let-values (((key value) (car+cdr (vector-ref vec i))))
        (hashassoc-set! hm key value)))
    hm))

;;;-------------------------------------------------------------------
;;;
;;; Retrieval from the structure.
;;;

(define (hashassoc-empty? hm)
  (fxzero? (hashassoc-size hm)))

(define (hashassoc-ref hm key)
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
  (if (hashassoc-empty? hm)
    #f
    (let ((depth->popmap ((key->depth->popmap hm) key)))
      (let loop ((array (hashassoc-trie hm))
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
               (let ((equiv? (hashassoc-equiv? hm))
                     (k (car entry)))
                 (if (equiv? key k) entry #f)))
              ((chain? entry)
               (let* ((equiv? (hashassoc-equiv? hm))
                      (matches? (lambda (k) (equiv? key k))))
                 (search-chain entry matches?)))
              (else
               (let ((next-depth (fx+ depth 1)))
                 (loop entry next-depth
                       (depth->popmap next-depth)))))))))))

;;;-------------------------------------------------------------------
;;;
;;; Insertion into the structure
;;;

(define-syntax mode-set
  (syntax-rules ()
    ((¶) 0)))

(define-syntax mode-insert
  (syntax-rules ()
    ((¶) 1)))

(define-syntax mode-replace
  (syntax-rules ()
    ((¶) 2)))

(define hashassoc-set!
  (case-lambda
    ((hm key value)
     (if (hashassoc-empty? hm)
       (make-initial-trie! hm key value)
       (insert-entry! (mode-set) hm key value)))
    ((hm . rest*)
     (hashassoc-set-from-alist! hm (plist->alist rest*)))))

(define hashassoc-insert!
  (case-lambda
    ((hm key value)
     (if (hashassoc-empty? hm)
       (make-initial-trie! hm key value)
       (insert-entry! (mode-insert) hm key value)))
    ((hm . rest*)
     (hashassoc-insert-from-alist! hm (plist->alist rest*)))))

(define hashassoc-replace!
  (case-lambda
    ((hm key value)
     (if (hashassoc-empty? hm)
       hm
       (insert-entry! (mode-replace) hm key value)))
    ((hm . rest*)
     (hashassoc-set-from-alist! hm (plist->alist rest*)))))

(define (make-initial-trie! hm key value)
  (let* ((depth->popmap ((key->depth->popmap hm) key))
         (pm (depth->popmap 0))
         (leaf `(,key . ,value))
         (array (make-array-node pm leaf)))
    (set-hashassoc-trie! hm array)
    (set-hashassoc-size! hm 1)
    hm))

(define (insert-entry! mode hm key value)
  (let ((depth->popmap ((key->depth->popmap hm) key)))
    (let loop ((array (hashassoc-trie hm))
               (depth 0)
               (setter! (lambda (trie)
                          (set-hashassoc-trie! hm trie))))

      (define (insert-at-chain chain)
        (let* ((equiv? (hashassoc-equiv? hm))
               (matches? (lambda (k) (equiv? key k)))
               (number (cond
                         ((fx=? mode (mode-set))
                          (set-in-chain! chain matches?
                                         `(,key . ,value)))
                         ((fx=? mode (mode-insert))
                          (insert-in-chain! chain matches?
                                            `(,key . ,value)))
                         ((fx=? mode (mode-replace))
                          (replace-in-chain! chain matches?
                                             `(,key . ,value))))))
          (unless (fxzero? number)
            (set-hashassoc-size! hm (fx+ 1 (hashassoc-size hm))))
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

             (define (expand-the-current-array!)
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
                 (set-hashassoc-size! hm (fx+ 1 (hashassoc-size hm)))
                 hm))

             (define (insert-at-pair)
               (let* ((pair1 (get-entry-quickly array i))
                      (key1 (car pair1)))
                 (cond
                   (((hashassoc-equiv? hm) key key1)
                    ;; Do not replace if in ‘insert’ mode.
                    (unless (fx=? mode (mode-insert))
                      ;; Replace the existing pair.
                      (set-entry! array i `(,key . ,value)))
                    hm)
                   ((fx=? mode (mode-replace))
                    ;; Do not insert if in ‘replace’ mode.
                    hm)
                   (else
                    (grow-trie-at-pair
                     depth pair1 (lambda (elem)
                                   (set-entry! array i elem)))))))

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
                      (set-hashassoc-size! hm (fx+ 1 (hashassoc-size hm)))
                      hm))
                   ((fx<? pm% pm1%)
                    (let ((array1
                           (make-array-node (fxior pm% pm1%)
                                            `(,key . ,value) pair1)))
                      (setter! array1)
                      (set-hashassoc-size! hm (fx+ 1 (hashassoc-size hm)))
                      hm))
                   ((fx<? pm1% pm%)
                    (let ((array1
                           (make-array-node (fxior pm% pm1%)
                                            pair1 `(,key . ,value))))
                      (setter! array1)
                      (set-hashassoc-size! hm (fx+ 1 (hashassoc-size hm)))
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
                (if (fx=? mode (mode-replace))
                  hm ;; No insertion when in ‘replace’ mode.
                  (expand-the-current-array!)))
               ((pair? (get-entry-quickly array i))
                (insert-at-pair))
               ((chain? (get-entry-quickly array i))
                (insert-at-chain (get-entry-quickly array i)))
               (else (increase-depth))))))))))

(define (hashassoc-set-from-alist! hm alst)
  (let loop ((p alst)
             (hm hm))
    (if (null-list? p)
      hm
      (loop (cdr p) (hashassoc-set! hm (caar p) (cdar p))))))

(define (hashassoc-insert-from-alist! hm alst)
  (let loop ((p alst)
             (hm hm))
    (if (null-list? p)
      hm
      (loop (cdr p) (hashassoc-insert! hm (caar p) (cdar p))))))

(define (hashassoc-replace-from-alist! hm alst)
  (let loop ((p alst)
             (hm hm))
    (if (null-list? p)
      hm
      (loop (cdr p) (hashassoc-replace! hm (caar p) (cdar p))))))

;;;-------------------------------------------------------------------
;;;
;;; Deletion from the structure.
;;;

(define hashassoc-delete!
  (case-lambda
    ((hm key)
     ;;
     ;; Whether the key is found can be detected by looking for a
     ;; change in hashassoc size.
     ;;
     (case (hashassoc-size hm)
       ((0) hm)
       ((1) (begin
              (when (hashassoc-ref hm key)
                (set-hashassoc-size! hm 0)
                (set-hashassoc-trie! hm #f))
              hm))
       (else (begin
               (delete-from-trie! hm key)
               hm))))
    ((hm . rest*)
     ;;
     ;; Multiple keys can listed in the command.
     ;;
     (hashassoc-delete-from-list! hm rest*))))

(define (delete-from-trie! hm key)
  ;;
  ;; The approach here is to search as if for retrieval, but to build
  ;; a record of the route taken. That record can be used to construct
  ;; a smaller subtrie, going backwards.
  ;;
  (let* ((depth->popmap ((key->depth->popmap hm) key))
         (route-max
          (fx+ (fxquotient (hash-bits-max) (hash-bits-chunk-max))
               (fxremainder (hash-bits-max) (hash-bits-chunk-max))))
         (route (make-vector route-max))
         (depth 0)
         (deepest-depth 0))

    (define-syntax $array
      (syntax-rules ()
        ((¶) 0)))

    (define-syntax $pm
      (syntax-rules ()
        ((¶) 1)))

    (define-syntax $i
      (syntax-rules ()
        ((¶) 2)))

    (define (fill-route!)
      (let loop ((array (hashassoc-trie hm))
                 (pm (depth->popmap 0)))
        (if (hash-bits-exhausted? pm)
          #f
          (let* ((mask (fx- pm 1))
                 (i (fxbit-count
                     (fxand mask (get-population-map array))))
                 (entry (get-entry array i)))
            (vector-set! route depth (vector array pm i))
            (cond
              ((not entry) #f)
              ((pair? entry)
               (let ((equiv? (hashassoc-equiv? hm))
                     (k (car entry)))
                 (equiv? key k)))
              ((chain? entry)
               (let* ((equiv? (hashassoc-equiv? hm))
                      (matches? (lambda (k) (equiv? key k))))
                 (let-values (((rest-of-chain size-change)
                               (delete-from-chain! entry matches?)))
                   (cond
                     ((fxzero? size-change) #f)
                     ((pair? rest-of-chain)
                      ;; The chain is gone and there is now just a
                      ;; key-value pair. Go straight to ‘middle
                      ;; depths’ processing.
                      (set-entry! array i rest-of-chain)
                      (set! deepest-depth (fx+ depth 1))
                      #t)
                     (else
                      ;; The chain is still a chain. We will not have
                      ;; to run rebuild-subtrie!
                      (set-entry! array i rest-of-chain)
                      ;; Indicate we need not rebuild the subtrie.
                      (set! depth 'chain)
                      #t)))))
              (else
               (set! depth (fx+ depth 1))
               (set! deepest-depth depth)
               (loop entry (depth->popmap depth))))))))

    (define (rebuild-subtrie!)
      (let* ((level (vector-ref route depth))
             (array (vector-ref level ($array)))
             (pm (vector-ref level ($pm)))
             (i (vector-ref level ($i)))
             (popmap (get-population-map array))
             (new-popmap (fxxor popmap pm))
             (n (array-size array))
             (n-1 (fx- n 1)))

        (define (current-array-shrunken)
          (let ((array1 (make-vector n)))
            (set-population-map! array1 new-popmap)
            (do ((j 0 (fx+ j 1)))
                ((fx=? j i))
              (set-entry! array1 j
                          (get-entry-quickly array j)))
            (if (not (fx=? i n-1))
              (do ((j i (fx+ j 1)))
                  ((fx=? j n-1))
                (set-entry! array1 j
                            (get-entry-quickly array (fx+ j 1)))))
            array1))

        (define (handle-middle-depth!)
          ;;
          (define (middle-levels-size-one!)
            ;; If the current entry is a key-value pair, eliminate
            ;; this array. Continue at the next level. Othwerwise we
            ;; are done.
            (let ((entry (get-entry-quickly array i)))
              (when (pair? entry)
                (let* ((depth% (fx- depth 1))
                       (level% (vector-ref route depth%))
                       (array% (vector-ref level% ($array)))
                       (i% (vector-ref level% ($i))))
                  (set-entry! array% i% entry)
                  (set! depth depth%)
                  (rebuild-subtrie!)))))
          ;;
          (when (fx=? n 1) (middle-levels-size-one!)))

        (define (handle-deepest-depth!)
          ;;
          (define (deepest-level-size-two!)
            ;; The entry at i is a key-value pair to be deleted.
            (let* ((entry1 (get-entry-quickly array (fx- 1 i)))
                   (depth% (fx- depth 1))
                   (level% (vector-ref route depth%))
                   (array% (vector-ref level% ($array)))
                   (i% (vector-ref level% ($i))))
              (cond
                ((pair? entry1)
                 ;; Replace the current array with the key-value pair
                 ;; that is entry1. Continue at the next level.
                 (set-entry! array% i% entry1)
                 (set! depth depth%)
                 (rebuild-subtrie!))
                (else
                 ;; Remove ‘entry’ pair from the current array.
                 ;; Continue at the next level.
                 (set-entry! array% i% (current-array-shrunken))
                 (set! depth depth%)
                 (rebuild-subtrie!)))))
          ;;
          (define (deepest-level-size-three-or-greater!)
            ;; The entry is a key-value pair to be deleted. Shrink the
            ;; current array. No other action is needed.
            (let* ((depth% (fx- depth 1))
                   (level% (vector-ref route depth%))
                   (array% (vector-ref level% ($array)))
                   (i% (vector-ref level% ($i))))
              (set-entry! array% i% (current-array-shrunken))))
          ;;
          (if (fx=? n 2)
            (deepest-level-size-two!)
            (deepest-level-size-three-or-greater!)))

        (cond
          ((fx=? depth deepest-depth)
           (handle-deepest-depth!))
          ((not (fxzero? depth))
           (handle-middle-depth!)))))

    (define (shrink-foundation-array)
      (let* ((array (hashassoc-trie hm))
             (level (vector-ref route 0))
             (pm (vector-ref level ($pm)))
             (i (vector-ref level ($i)))
             (popmap (get-population-map array))
             (new-popmap (fxxor popmap pm))
             (n (array-size array))
             (array1 (make-vector n))
             (n-1 (fx- n 1)))
        (set-population-map! array1 new-popmap)
        (do ((j 0 (fx+ j 1)))
            ((fx=? j i))
          (set-entry! array1 j
                      (get-entry-quickly array j)))
        (if (not (fx=? i n-1))
          (do ((j i (fx+ j 1)))
              ((fx=? j n-1))
            (set-entry! array1 j
                        (get-entry-quickly array (fx+ j 1)))))
        array1))

    (let ((found? (fill-route!)))
      (when found?
        (cond
          ((eq? depth 'chain) )
          ((fxzero? depth)
           (set-hashassoc-trie! hm (shrink-foundation-array)))
          (else (rebuild-subtrie!)))
        (set-hashassoc-size! hm (fx- (hashassoc-size hm) 1))))))

(define (hashassoc-delete-from-list! hm lst)
  (do ((p lst (cdr p)))
      ((null-list? p))
    (hashassoc-delete! hm (car p)))
  hm)

;;;-------------------------------------------------------------------
;;;
;;; Walking the trie.
;;;

(define (hashassoc->vector hm)
  ;;
  ;; Walk the trie and list the key-value pairs in a vector, in any
  ;; order. The order need not be the same from run to run of the
  ;; procedure, although the implementation may be such that this is
  ;; so.
  ;;
  (if (hashassoc-empty? hm)
    (vector)
    (let recurs ((array (hashassoc-trie hm)))
      (concatenate-vectors
       (map (lambda (i)
              (let ((entry (get-entry-quickly array i)))
                (cond
                  ((pair? entry) (vector entry))
                  ((chain? entry) (list->vector
                                   (chain->alist entry)))
                  (else (recurs entry)))))
            (iota (array-size array)))))))

(define (concatenate-vectors lst)
  (let* ((n (fold (lambda (v m)
                    (+ (vector-length v) m))
                  0 lst))
         (vec (make-vector n))
         (i 0))
    (do ((p lst (cdr p)))
        ((null? p))
      (let* ((v (car p))
             (m (vector-length v)))
        (vector-copy! vec i v)
        (set! i (fx+ i m))))
    vec))

(define (hashassoc->alist hm)
  ;;
  ;; Walk the trie and list the key-value pairs, in any order. The
  ;; order need not be the same from run to run of the procedure,
  ;; although the implementation may be such that this is so.
  ;;
  (hashassoc-fold cons '() hm))

(define (hashassoc->generator hm)
  ;;
  ;; Make a generator that walks the trie and returns the key-value
  ;; pairs, in any order. The order need not be the same from run to
  ;; run of the procedure, although the implementation may be such
  ;; that this is so.
  ;;
  ;; When the generator is finished returning key-value pairs, it
  ;; returns an end-of-file object whenever called.
  ;;
  ;; (The following implementation is in a continuation-passing style,
  ;; to keep the code simple while avoiding call/cc. Some Schemes have
  ;; slow call/cc.)
  ;;
  (if (hashassoc-empty? hm)
    (lambda () (eof-object))
    (make-trie-generator (hashassoc-trie hm))))

(define (make-trie-generator trie)
  (define (generate-array kontinue array i)
    (if (fx=? i (array-size array))
      (kontinue)
      (let ((entry (get-entry-quickly array i)))
        (cond
          ((pair? entry)
           (set! continue-here
             (lambda ()
               (generate-array kontinue array (fx+ i 1))))
           entry)
          ((chain? entry)
           (generate-alist
            (lambda ()
              (generate-array kontinue array (fx+ i 1)))
            (chain->alist entry)))
          (else
           (generate-array
            (lambda ()
              (generate-array kontinue array (fx+ i 1)))
            entry 0))))))
  (define (generate-alist kontinue alst)
    (if (null? alst)
      (kontinue)
      (begin
        (set! continue-here
          (lambda () (generate-alist kontinue (cdr alst))))
        (car alst))))
  (define continue-here
    (lambda ()
      (generate-array (lambda () (eof-object)) trie 0)))
  (lambda () (continue-here)))

(define (hashassoc-fold kons knil hm)
  ;;
  ;; Walk the trie in any order. The order need not be the same from
  ;; run to run of the procedure, although the implementation may be
  ;; such that this is so.
  ;;
  ;;  (kons kv-pairN (...(kons kv-pair1 (kons kv-pair0 knil))...))
  ;;
  ;; This makes any number of other operations simpler to implement.
  ;; For instance it makes a one-liner of hashassoc->alist. (Though
  ;; hashassoc->alist also can be one-lined from hashassoc->vector.)
  ;;
  (if (hashassoc-empty? hm)
    knil
    (let ((result knil))
      (let recurs ((array (hashassoc-trie hm)))
        (for-each
         (lambda (i)
           (let ((entry (get-entry-quickly array i)))
             (cond
               ((pair? entry)
                (set! result (kons entry result)))
               ((chain? entry)
                (set! result
                  (fold kons result (chain->alist entry))))
               (else (recurs entry)))))
         (iota (array-size array))))
      result)))

;;;-------------------------------------------------------------------
;;;
;;; Copying the trie.
;;;

(define (hashassoc-copy hm)
  ;;
  ;; Copy the structure of the hashmap, but not the keys and values.
  ;; Even new key-value pairs are created. Copy merely the references
  ;; to the keys and values and references to the hashassoc-equiv? and
  ;; key->depth->popmap procedures.
  ;;
  ;; (We would not know how to copy keys and values, anyway, unless we
  ;; took as arguments procedures to do the copying. I mention copying
  ;; keys and values because it is necessary if they [and not merely
  ;; the trie] are of linear types. I was inspired to write MUTABLE
  ;; hashmaps because I have an ATS implementation of hashmaps that is
  ;; linear-typed and mutable.)
  ;;
  (let ((sz (hashassoc-size hm)))
    (construct-hashassoc sz (hashassoc-equiv? hm)
                         (key->depth->popmap hm)
                         (if (fxzero? sz)
                           #f (copy-array (hashassoc-trie hm))))))

(define (copy-array array)
  (let* ((n (array-size array))
         (n+1 (fx+ n 1))
         (array% (make-vector n+1)))
    (set-population-map! array% (get-population-map array))
    (do ((i 0 (fx+ i 1)))
        ((fx=? i n))
      (let ((entry (get-entry-quickly array i)))
        (cond
          ((pair? entry)
           (set-entry! array% i `(,(car entry) . ,(cdr entry))))
          ((chain? entry)
           (set-entry! array% i (copy-chain entry)))
          (else
           (set-entry! array% i (copy-array entry))))))
    array%))

(define (copy-chain chain)
  (alist->chain (map (lambda (pair) `(,(car pair) . ,(cdr pair)))
                     (chain->alist chain))))

;;;-------------------------------------------------------------------
;;;
;;; Set-like operations on keys.
;;;

(define (hashassoc-union hm1 . hm*)
  (let ((hm (hashassoc-copy hm1)))
    (apply hashassoc-add! (cons hm hm*))))

(define (hashassoc-add! hm1 . hm*)
  (fold
   (lambda (hm% hm1)
     (hashassoc-fold (lambda (pair hm1)
                       (hashassoc-insert! hm1 (car pair) (cdr pair)))
                     hm1 hm%))
   hm1 hm*))

(define (hashassoc-intersection hm1 . hm*)
  (let ((hm (hashassoc-copy hm1)))
    (apply hashassoc-intersect! (cons hm hm*))))

(define (hashassoc-intersect! hm1 . hm*)
  (let ((alst (hashassoc->alist hm1)))
    (for-each (lambda (hm%)
                (for-each (lambda (pair)
                            (unless (hashassoc-ref hm% (car pair))
                              (hashassoc-delete! hm1 (car pair))))
                          alst))
              hm*)
    hm1))

(define (hashassoc-difference hm1 . hm*)
  (let ((hm (hashassoc-copy hm1)))
    (apply hashassoc-subtract! (cons hm hm*))))

(define (hashassoc-subtract! hm1 . hm*)
  (fold
   (lambda (hm% hm1)
     (hashassoc-fold (lambda (pair hm1)
                       (hashassoc-delete! hm1 (car pair)))
                     hm1 hm%))
   hm1 hm*))

(define (hashassoc-symmetric-difference hm1 hm2)
  (hashassoc-union
   (hashassoc-difference hm1 hm2)
   (hashassoc-difference hm2 hm1)))

(define (hashassoc-disjoint? hm1 hm2)
  (let ((sz1 (hashassoc-size hm1))
        (sz2 (hashassoc-size hm2)))
    (let ((hm1 (if (fx<=? sz1 sz2) hm1 hm2))
          (hm2 (if (fx<=? sz1 sz2) hm2 hm1)))
      (let ((gen! (hashassoc->generator hm1)))
        (let loop ((pair (gen!)))
          (cond ((eof-object? pair) #t)
                ((hashassoc-ref hm2 (car pair)) #f)
                (else (loop (gen!)))))))))

;;;-------------------------------------------------------------------
;;;
;;; Set-like operations on pairs.
;;;

(define (hashassoc-cmp% cmp value=? hm1 hm*)
  (define equiv? (hashassoc-equiv? hm1))
  (define (all-equiv?-are-the-same?)
    (every (lambda (hm) (eq? equiv? (hashassoc-equiv? hm))) hm*))
  (define (sizes-are-in-order?)
    (let loop ((hm1 hm1)
               (hm* hm*))
      (cond
        ((null-list? hm*) #t)
        ((cmp (hashassoc-size hm1) (hashassoc-size (car hm*)))
         (loop (car hm*) (cdr hm*)))
        (else #f))))
  (define val=?
    ;; If value=? is a boolean rather than a procedure, then ignore
    ;; the values. (A good case can be made for either #f or #t to be
    ;; the indicator here. Thus we accept either.)
    (if (boolean? value=?)
      (lambda (a b) #t)
      value=?))
  (define (test hm1 hm2)
    ;; hm1 is always smaller than or equal in size to hm2.
    (if (hashassoc-empty? hm1)
      #t
      (let ((gen! (hashassoc->generator hm1)))
        (let loop ((pair (gen!)))
          (if (eof-object? pair)
            #t
            (let ((pair% (hashassoc-ref hm2 (car pair))))
              (if (and pair% (val=? (cdr pair) (cdr pair%)))
                (loop (gen!))
                #f)))))))
  (unless (or (boolean? value=?)
              (procedure? value=?))
    (error "expected a procedure or boolean" value=?))
  (cond
    ((not (all-equiv?-are-the-same?)) #f)
    ((not (sizes-are-in-order?)) #f)
    (else
     (let loop ((hm1 hm1)
                (hm* hm*))
       (cond ((null-list? hm*) #t)
             ((test hm1 (car hm*))
              (loop (car hm*) (cdr hm*)))
             (else #f))))))

(define (hashassoc-cmp cmp arg1 arg*)
  (if (or (boolean? arg1) (procedure? arg1))
    (hashassoc-cmp% cmp arg1 (car arg*) (cdr arg*))
    (hashassoc-cmp% cmp equal? arg1 arg*)))

(define (hashassoc-reverse-cmp cmp arg1 arg*)
  (cond ((boolean? arg1)
         (let ((arg* (reverse arg*)))
           (hashassoc-cmp% cmp arg1 (car arg*) (cdr arg*))))
        ((procedure? arg1)
         (let ((arg* (reverse arg*)))
           (hashassoc-cmp% cmp (lambda (a b) (arg1 b a))
                           (car arg*) (cdr arg*))))
        (else
         (let ((arg* (reverse (cons arg1 arg*))))
           (hashassoc-cmp% cmp equal? (car arg*) (cdr arg*))))))

(define (hashassoc=? arg1 . arg*)
  (hashassoc-cmp fx=? arg1 arg*))

(define (hashassoc<? arg1 . arg*)
  (hashassoc-cmp fx<? arg1 arg*))

(define (hashassoc<=? arg1 . arg*)
  (hashassoc-cmp fx<=? arg1 arg*))

(define (hashassoc>? arg1 . arg*)
  (hashassoc-reverse-cmp fx<? arg1 arg*))

(define (hashassoc>=? arg1 . arg*)
  (hashassoc-reverse-cmp fx<=? arg1 arg*))

;;;-------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
