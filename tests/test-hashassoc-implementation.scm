;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(display " ===== test-hashassoc =====\n")

(include "tests/tests-common.scm")

(define (alist->plist lst)
  (let loop ((p lst)
             (q '()))
    (if (null-list? p)
      (reverse! q)
      (loop (cdr p) (cons* (cdar p) (caar p) q)))))

(define (list-merge! less? lst1 lst2)
  ;; This is a tail-recursive implementation, done here as an example
  ;; of an implementation safe against memory blow-up and requiring no
  ;; list reversal. (The implementation here resembles a typical ATS
  ;; implementation for linear lists. Such lists can be used only
  ;; once, and so the merge is of course destructive.)
  (cond
    ((null? lst1) lst2)
    ((null? lst2) lst1)
    (else
     (let ((less12? (less? (car lst1) (car lst2))))
       (let ((lst1 (if less12? (cdr lst1) lst1))
             (lst2 (if less12? lst2 (cdr lst2)))
             (result (if less12? lst1 lst2)))
         (let loop ((lst1 lst1)
                    (lst2 lst2)
                    (last result))
           (cond
             ((null? lst1)
              (set-cdr! last lst2)
              result)
             ((null? lst2)
              (set-cdr! last lst1)
              result)
             ((less? (car lst1) (car lst2))
              (set-cdr! last lst1)
              (loop (cdr lst1) lst2 lst1))
             (else
              (set-cdr! last lst2)
              (loop lst1 (cdr lst2) lst2)))))))))

(define (list-sort! less? lst)
  (let ((n (length lst)))
    (if (<= n 1)
        lst
        (let-values (((lst1 lst2) (split-at! lst (quotient n 2))))
          (list-merge! less?
                       (list-sort! less? lst1)
                       (list-sort! less? lst2))))))

(do-ec
 (:list len (list 1 10 100 1000 10000 100000 1000000))
 (let* ((alist->string-hashassoc
         (lambda (alst)
           (alist->hashassoc string=? string-hash alst)))
        (pair=? (lambda (a b)
                  (and (string=? (car a) (car b))
                       (= (cdr a) (cdr b)))))
        (alst (list-ec (:range i 0 len)
                       `(,(number->string i 16) . ,i)))
        (hm (alist->string-hashassoc alst)))
   (test-assert (every?-ec (:list pair alst)
                           (:let s (car pair))
                           (:let i (cdr pair))
                           (:let pair% (hashassoc-ref hm s))
                           (:let s% (car pair%))
                           (:let i% (cdr pair%))
                           (and (string=? s s%) (= i i%))))
   (test-equal len (hashassoc-size hm))
   (when (<= len 1000)
     (test-assert (lset= pair=? alst (hashassoc->alist hm))))
   (let ((hm (hashassoc-set-from-alist! hm (reverse alst))))
     (test-assert (every?-ec (:list pair alst)
                             (:let s (car pair))
                             (:let i (cdr pair))
                             (:let pair% (hashassoc-ref hm s))
                             (:let s% (car pair%))
                             (:let i% (cdr pair%))
                             (and (string=? s s%) (= i i%))))
     (test-equal len (hashassoc-size hm)))))

(do-ec
 (:list len (list 1 10 100 1000 10000))
 (let* ((tiny-hash (lambda (str) (remainder (string-hash str) 2)))
        (pair=? (lambda (a b)
                  (and (string=? (car a) (car b))
                       (= (cdr a) (cdr b)))))
        (alist->tstring-hashassoc
         (lambda (alst)
           (alist->hashassoc string=? tiny-hash alst)))
        (alst (list-ec (:range i 0 len)
                       `(,(number->string i 16) . ,i)))
        (hm (alist->tstring-hashassoc alst)))
   (test-assert (every?-ec (:list pair alst)
                           (:let s (car pair))
                           (:let i (cdr pair))
                           (:let pair% (hashassoc-ref hm s))
                           (:let s% (car pair%))
                           (:let i% (cdr pair%))
                           (and (string=? s s%) (= i i%))))
   (when (<= len 1000)
     (test-assert (lset= pair=? alst (hashassoc->alist hm))))
   (test-equal len (hashassoc-size hm))))

(do-ec
 (:list len (list 1 10 100 1000 10000 100000))
 (let* ((alist->string-hashassoc
         (lambda (alst)
           (alist->hashassoc string=? string-hash alst)))
        (alst1 (list-ec (:range i 0 (* 2 len))
                        `(,(number->string i 16) . ,i)))
        (hm1 (alist->string-hashassoc alst1))
        (lst2 (list-ec (:range i 0 (* 2 len) 2)
                       (number->string i 16)))
        (hm2 (hashassoc-delete-from-list! hm1 lst2)))
   (test-assert (every?-ec (:list pair (index j) alst1)
                           (if (odd? j))
                           (:let s (car pair))
                           (:let i (cdr pair))
                           (:let pair% (hashassoc-ref hm2 s))
                           (:let s% (car pair%))
                           (:let i% (cdr pair%))
                           (and (string=? s s%) (= i i%))))
   (when (<= len 1000)
     (test-assert
         (lset= string=?
                (lset-difference string=? (map car alst1) lst2)
                (map! car (hashassoc->alist hm2)))))
   (test-equal len (hashassoc-size hm2))))

(do-ec
 (:list len (list 1 10 100 1000))
 (:list my-hash (list string-hash
                      (lambda (str)
                        (remainder (string-hash str) 2))))
 (let* ((pair=? (lambda (a b)
                  (and (string=? (car a) (car b))
                       (= (cdr a) (cdr b)))))
        (alst1 (list-ec (:range i 0 len)
                        `(,(number->string i 16) . ,i)))
        (hm1 (if (<= (length alst1) 10)
               (apply make-hashassoc
                      (cons* string=? my-hash
                             (alist->plist alst1)))
               (alist->hashassoc string=? my-hash alst1)))
        (g1 (hashassoc->generator hm1))
        (alst2 '()))
   (do ((pair (g1) (g1)))
       ((eof-object? pair))
     (set! alst2 (cons pair alst2)))
   (test-assert (lset= pair=? alst1 alst2))
   (let ((hm-empty (apply hashassoc-delete!
                          (cons hm1 (map car alst2)))))
     (test-assert (hashassoc-empty? hm-empty))
     (let ((g2 (hashassoc->generator hm-empty)))
       (test-assert (eof-object? (g2)))))))

(do-ec
 (:list len (list 1 10 100 1000))
 (:list my-hash (list string-hash
                      (lambda (str)
                        (remainder (string-hash str) 2))))
 (let* ((pair=? (lambda (a b)
                  (and (string=? (car a) (car b))
                       (= (cdr a) (cdr b)))))
        (alst1 (list-ec (:range i 0 len)
                        `(,(number->string i 16) . ,i)))
        (vec2 (list->vector alst1))
        (hm1 (alist->hashassoc string=? my-hash alst1))
        (hm2 (vector->hashassoc string=? my-hash vec2)))
   (test-assert (lset= pair=?
                       (hashassoc->alist hm1)
                       (hashassoc->alist hm2)))
   (let ((hm2 (vector->hashassoc string=? my-hash
                                 (hashassoc->vector hm2))))
     (test-assert (lset= pair=?
                         (hashassoc->alist hm1)
                         (hashassoc->alist hm2))))))

(do-ec
 ;;
 ;; Test hashassoc-set-from-alist!
 ;;      hashassoc-insert-from-alist!
 ;;      hashassoc-replace-from-alist!
 ;;
 (:list len (list 1 10 100 1000))
 (:list my-hash (list string-hash
                      (lambda (str)
                        (remainder (string-hash str) 2))))
 (let* ((pair=? (lambda (a b)
                  (and (string=? (car a) (car b))
                       (= (cdr a) (cdr b)))))
        (hm-1 (make-hashassoc string=? my-hash))
        (hm-2 (make-hashassoc string=? my-hash))
        (hm-3 (make-hashassoc string=? my-hash))
        (alst1 (list-ec (:range i 0 len)
                        `(,(number->string i 16) . ,i)))
        (alst2 (list-ec (:range i 0 len)
                        `(,(number->string i 16) . ,(- i)))))
   (hashassoc-set-from-alist! hm-1 alst1)
   (hashassoc-insert-from-alist! hm-2 alst1)
   (hashassoc-replace-from-alist! hm-3 alst1)
   (test-equal len (hashassoc-size hm-1))
   (test-equal len (hashassoc-size hm-2))
   (test-assert (lset= pair=?
                       (hashassoc->alist hm-1)
                       (hashassoc->alist hm-2)))
   (test-assert (hashassoc-empty? hm-3))
   (hashassoc-replace-from-alist! hm-2 alst2)
   (test-assert (lset= pair=?
                       (hashassoc->alist hm-1)
                       (map (lambda (pair)
                              `(,(car pair) . ,(- (cdr pair))))
                            (hashassoc->alist hm-2))))
   (hashassoc-insert-from-alist! hm-2 alst2)
   (test-assert (lset= pair=?
                       (hashassoc->alist hm-1)
                       (map (lambda (pair)
                              `(,(car pair) . ,(- (cdr pair))))
                            (hashassoc->alist hm-2))))
   (hashassoc-insert-from-alist! hm-3 alst1)
   (test-assert (lset= pair=?
                       (hashassoc->alist hm-1)
                       (hashassoc->alist hm-3)))))

(do-ec
 ;;
 ;; Test hashassoc-delete-from-list!
 ;;
 (:list len (list 1 10 100 1000))
 (:list my-hash (list string-hash
                      (lambda (str)
                        (remainder (string-hash str) 2))))
 (let* ((pair=? (lambda (a b)
                  (and (string=? (car a) (car b))
                       (= (cdr a) (cdr b)))))
        (alst1 (list-ec (:range i 0 len)
                        `(,(number->string i 16) . ,i)))
        (hm1 (alist->hashassoc string=? my-hash alst1))
        (hm2 (hashassoc-copy hm1)))
   (hashassoc-delete-from-list! hm1 (map car alst1))
   (test-equal 0 (hashassoc-size hm1))
   (test-equal len (hashassoc-size hm2))
   (test-assert (lset= pair=? alst1 (hashassoc->alist hm2)))))

(do-ec
 ;;
 ;; Test hashassoc=? hashassoc<? and so forth.
 ;;
 (:list my-hash (list string-hash
                      (lambda (str)
                        (remainder (string-hash str) 2))))
 (let* ((num->str (lambda (i) (number->string i 16)))
        (hm0 (alist->hashassoc string=? my-hash '()))
        (hm10 (alist->hashassoc string=? my-hash
                                (map cons
                                     (map num->str (iota 10))
                                     (iota 10))))
        (hm100 (alist->hashassoc string=? my-hash
                                 (map cons
                                      (map num->str (iota 100))
                                      (iota 100))))
        (hm1000 (alist->hashassoc string=? my-hash
                                  (map cons
                                       (map num->str (iota 1000))
                                       (iota 1000))))
        (hm1000a (alist->hashassoc string=? my-hash
                                   (map cons
                                        (map num->str (iota 1000))
                                        (map number->string
                                             (iota 1000)))))
        (hm1000b (alist->hashassoc string=? my-hash
                                   (map cons
                                        (map num->str (iota 1000))
                                        (map (lambda (x) (* +i x))
                                             (iota 1000))))))

   (test-assert (hashassoc=? hm1000))
   (test-assert (hashassoc>? hm1000))
   (test-assert (hashassoc>=? hm1000))
   (test-assert (hashassoc<? hm1000))
   (test-assert (hashassoc<=? hm1000))

   (test-assert (hashassoc=? hm0 hm0 hm0 hm0))
   (test-assert (hashassoc=? hm1000 hm1000 hm1000 hm1000))
   (test-assert (not (hashassoc=? hm1000 hm1000a hm1000 hm1000a)))
   (test-assert (hashassoc=? #f hm1000 hm1000a hm1000 hm1000a))
   (test-assert (hashassoc=? #t hm1000 hm1000a hm1000 hm1000a))
   (test-assert (hashassoc=? (lambda (a b)
                               (zero? (+ (* a a) (* b b))))
                             hm1000 hm1000b hm1000 hm1000b))

   (test-assert (not (hashassoc=? hm0 hm10 hm100 hm1000)))
   (test-assert (not (hashassoc<? hm0 hm10 hm100 hm100 hm1000)))
   (test-assert (hashassoc<=? hm0 hm10 hm100 hm100 hm1000))
   (test-assert (not (hashassoc>? hm0 hm10 hm100 hm1000)))
   (test-assert (not (hashassoc>=? hm0 hm10 hm100 hm1000)))
   (test-assert (not (hashassoc>? hm0 hm10 hm100 hm1000)))
   (test-assert (not (hashassoc>=? hm0 hm10 hm100 hm1000)))

   (test-assert (not (hashassoc=? hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<? hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<=? hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>? hm1000 hm100 hm10 hm0))
   (test-assert (hashassoc>=? hm1000 hm100 hm10 hm0))
   (test-assert (not (hashassoc>? hm1000 hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>=? hm1000 hm1000 hm100 hm10 hm0))

   (test-assert (not (hashassoc=? #f hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<? #f hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<=? #f hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>? #f hm1000 hm100 hm10 hm0))
   (test-assert (hashassoc>=? #f hm1000 hm100 hm10 hm0))
   (test-assert (not (hashassoc>? #f hm1000 hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>=? #f hm1000 hm1000 hm100 hm10 hm0))

   (test-assert (not (hashassoc=? #t hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<? #t hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<=? #t hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>? #t hm1000 hm100 hm10 hm0))
   (test-assert (hashassoc>=? #t hm1000 hm100 hm10 hm0))
   (test-assert (not (hashassoc>? #t hm1000 hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>=? #t hm1000 hm1000 hm100 hm10 hm0))

   (test-assert (not (hashassoc=? eqv? hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<? eqv? hm1000 hm100 hm10 hm0)))
   (test-assert (not (hashassoc<=? eqv? hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>? eqv? hm1000 hm100 hm10 hm0))
   (test-assert (hashassoc>=? eqv? hm1000 hm100 hm10 hm0))
   (test-assert (not (hashassoc>? eqv? hm1000 hm1000 hm100 hm10 hm0)))
   (test-assert (hashassoc>=? eqv? hm1000 hm1000 hm100 hm10 hm0))
   ))

(do-ec
 ;;
 ;; Test hashassoc-union et al., and use a comparator.
 ;;
 (:list my-hash (list string-hash
                      (lambda (str)
                        (remainder (string-hash str) 2))))
 (let* ((cmp (make-comparator string? string=? string<? my-hash))
        (hm1 (make-hashassoc cmp "0" 48 "1" 49 "2" 50 "3" 51 "4" 52 "5" 53))
        (hm2 (make-hashassoc cmp "A" 65 "B" 66 "C" 67 "D" 68 "E" 69 "F" 70))
        (hm3 (make-hashassoc cmp "a" 97 "b" 98 "c" 99 "d" 100 "e" 101 "f" 102))
        (hm4 (make-hashassoc cmp "0" 48 "1" 49 "2" 50 "D" 68 "E" 69 "F" 70))
        (hm5 (make-hashassoc cmp "a" 97 "1" 49 "2" 50 "D" 68 "E" 69 "F" 70)))
   (test-assert (hashassoc=? = (hashassoc-union hm1 hm2 hm3)
                             (make-hashassoc cmp "0" 48 "1" 49 "2" 50 "3" 51 "4" 52 "5" 53
                                             "A" 65 "B" 66 "C" 67 "D" 68 "E" 69 "F" 70
                                             "a" 97 "b" 98 "c" 99 "d" 100 "e" 101 "f" 102)))
   (test-assert (hashassoc=? = (hashassoc-intersection hm1 hm2 hm3) (make-hashassoc cmp)))
   (test-assert (hashassoc=? = (hashassoc-difference hm1 hm2 hm3) hm1))
   (test-assert (hashassoc=? = (hashassoc-symmetric-difference hm1 hm2)
                             (hashassoc-union hm1 hm2)))
   (test-assert (hashassoc-disjoint? hm1 hm2))
   (test-assert (not (hashassoc-disjoint? hm3 hm5)))
   (test-assert (hashassoc=? = (hashassoc-union hm3 hm4 hm5)
                             (make-hashassoc cmp "a" 97 "b" 98 "c" 99 "d" 100 "e" 101 "f" 102
                                             "0" 48 "1" 49 "2" 50 "D" 68 "E" 69 "F" 70)))
   (test-assert (hashassoc=? = (hashassoc-intersection hm3 hm4 hm5) (make-hashassoc cmp)))
   (test-assert (hashassoc=? = (hashassoc-intersection hm4 hm5 hm3) (make-hashassoc cmp)))
   (test-assert (hashassoc=? = (hashassoc-intersection hm5 hm3 hm4) (make-hashassoc cmp)))
   (test-assert (hashassoc=? = (hashassoc-intersection hm3 hm4) (make-hashassoc cmp)))
   (test-assert (hashassoc=? = (hashassoc-intersection hm4 hm5)
                             (make-hashassoc cmp "1" 49 "2" 50 "D" 68 "E" 69 "F" 70)))
   (test-assert (hashassoc=? = (hashassoc-difference hm4 hm5)
                             (make-hashassoc cmp "0" 48)))
   (test-assert (hashassoc=? = (hashassoc-difference hm5 hm4)
                             (make-hashassoc cmp "a" 97)))
   (test-assert (hashassoc=? = (hashassoc-symmetric-difference hm4 hm5)
                             (make-hashassoc cmp "0" 48 "a" 97)))
   (test-assert (hashassoc=? = (hashassoc-symmetric-difference hm5 hm4)
                             (make-hashassoc cmp "0" 48 "a" 97)))
   ))

(do-ec
 (:list my-hash (list string-hash
                      (lambda (str)
                        (remainder (string-hash str) 2))))
 (let* ((cmp (make-comparator string? string=? string<? my-hash))
        (pair=? (lambda (a b)
                  (and (string=? (car a) (car b))
                       (= (cdr a) (cdr b)))))
        (hm1 (hashassoc-ec cmp (:range i 0 1000)
                           `(,(number->string i 16) . ,i)))
        (hm2 hm1))
   (do ((i 0 (+ i 1)))
       ((= i 500))
     (set! hm2 (hashassoc-replace hm2 (number->string i 16) (+ 10000 i))))
   (let* ((alst2 (list-sort! (lambda (e1 e2)
                               (< (string->number (car e1) 16)
                                  (string->number (car e2) 16)))
                             (hashassoc->alist hm2)))
          (alst2a (take alst2 500))
          (alst2b (drop alst2 500)))
     (test-assert (lset= pair=? alst2a
                         (list-ec (:range i 0 500)
                                  `(,(number->string i 16)
                                    . ,(+ 10000 i)))))
     (test-assert (lset= pair=? alst2b
                         (list-ec (:range i 500 1000)
                                  `(,(number->string i 16)
                                    . ,i))))
     (test-assert (lset= pair=?
                         (hashassoc->alist hm1)
                         (list-ec (:range i 0 1000)
                                  `(,(number->string i 16)
                                    . ,i)))))
   ))

(display successes)
(display " successes\n")
(display failures)
(display " failures\n")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
