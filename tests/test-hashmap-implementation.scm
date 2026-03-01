;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(display " ===== test-hashmap =====\n")

(define successes 0)
(define failures 0)

(define-syntax test-assert
  (syntax-rules ()
    ((¶ asserted)
     (let ((a ((lambda () asserted))))
       (if a
         (set! successes (+ successes 1))
         (begin
           (set! failures (+ failures 1))
           (display "failed: ")
           (display 'asserted)
           (newline)))))))

(define-syntax test-equal
  (syntax-rules ()
    ((¶ expected tested)
     (let ((e ((lambda () expected)))
           (t ((lambda () tested))))
       (if (equal? e t)
         (set! successes (+ successes 1))
         (begin
           (set! failures (+ failures 1))
           (display "failed: ")
           (display "(equal? ")
           (display 'expected)
           (display " ")
           (display 'tested)
           (display ")")
           (newline)))))))

(define-syntax test-eq
  (syntax-rules ()
    ((¶ expected tested)
     (let ((e ((lambda () expected)))
           (t ((lambda () tested))))
       (if (eq? e t)
         (set! successes (+ successes 1))
         (begin
           (set! failures (+ failures 1))
           (display "failed: ")
           (display "(eq? ")
           (display 'expected)
           (display " ")
           (display 'tested)
           (display ")")
           (newline)))))))

(let* ((alist->string-hashmap
        (lambda (alst)
          (alist->hashmap string=? string-hash alst)))
       (alst (list-ec (:range i 1 1000001)
                      (:let s (number->string i 16))
                      (:let pair `(,s . ,i))
                      pair))
       (hm (alist->string-hashmap alst)))
  (test-assert (every?-ec (:list pair alst)
                          (:let s (car pair))
                          (:let i (cdr pair))
                          (:let pair% (hashmap-ref hm s))
                          (:let s% (car pair%))
                          (:let i% (cdr pair%))
                          (and (string=? s s%) (= i i%))))
  (test-equal (length alst) (hashmap-size hm))
  (let ((hm (hashmap-set-from-alist! hm (reverse alst))))
    (test-assert (every?-ec (:list pair alst)
                            (:let s (car pair))
                            (:let i (cdr pair))
                            (:let pair% (hashmap-ref hm s))
                            (:let s% (car pair%))
                            (:let i% (cdr pair%))
                            (and (string=? s s%) (= i i%))))
    (test-equal (length alst) (hashmap-size hm))))

(let* ((tiny-hash (lambda (str) (remainder (string-hash str) 2)))
       (alist->tiny-hashmap
        (lambda (alst)
          (alist->hashmap string=? tiny-hash alst)))
       (alst (list-ec (:range i 1 10001)
                      (:let s (number->string i 16))
                      (:let pair `(,s . ,i))
                      pair))
       (hm (alist->tiny-hashmap alst)))
  (test-assert (every?-ec (:list pair alst)
                          (:let s (car pair))
                          (:let i (cdr pair))
                          (:let pair% (hashmap-ref hm s))
                          (:let s% (car pair%))
                          (:let i% (cdr pair%))
                          (and (string=? s s%) (= i i%))))
  (test-equal (length alst) (hashmap-size hm)))

(let* ((tiny-hash (lambda (str) (remainder (string-hash str) 2)))
       (alist->string-hashmap
        (lambda (alst)
          (alist->hashmap string=? #;tiny-hash string-hash alst)))
       (alst1 (list-ec (:range i 1 2000001)
                       `(,(number->string i 16) . ,i)))
       (hm1 (alist->string-hashmap alst1))
       (lst2 (list-ec (:range i 1 2000001 2)
                      (number->string i 16)))
       (hm2 (hashmap-delete-from-list! hm1 lst2)))
  (test-assert (every?-ec (:list pair (index j) alst1)
                          (if (odd? j))
                          (:let s (car pair))
                          (:let i (cdr pair))
                          (:let pair% (hashmap-ref hm2 s))
                          (:let s% (car pair%))
                          (:let i% (cdr pair%))
                          (and (string=? s s%) (= i i%))))
  (test-equal 1000000 (hashmap-size hm2)))

(display successes)
(display " successes\n")
(display failures)
(display " failures\n")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
