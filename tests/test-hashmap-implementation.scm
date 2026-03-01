;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(display " ===== test-hashmap =====\n")

(include "tests/tests-common.scm")

(do-ec
 (:list len (list 1 10 100 1000 10000 100000 1000000))
 (let* ((alist->string-hashmap
         (lambda (alst)
           (alist->hashmap string=? string-hash alst)))
        (alst (list-ec (:range i len)
                       `(,(number->string i 16) . ,i)))
        (hm (alist->string-hashmap alst)))
   (test-assert (every?-ec (:list pair alst)
                           (:let s (car pair))
                           (:let i (cdr pair))
                           (:let pair% (hashmap-ref hm s))
                           (:let s% (car pair%))
                           (:let i% (cdr pair%))
                           (and (string=? s s%) (= i i%))))
   (test-equal len (hashmap-size hm))
   (when (<= len 1000)
     (test-assert (lset= (lambda (a b)
                           (and (string=? (car a) (car b))
                                (= (cdr a) (cdr b))))
                         alst (hashmap->alist hm))))
   (let ((hm (hashmap-set-from-alist! hm (reverse alst))))
     (test-assert (every?-ec (:list pair alst)
                             (:let s (car pair))
                             (:let i (cdr pair))
                             (:let pair% (hashmap-ref hm s))
                             (:let s% (car pair%))
                             (:let i% (cdr pair%))
                             (and (string=? s s%) (= i i%))))
     (test-equal len (hashmap-size hm)))))

(do-ec
 (:list len (list 1 10 100 1000 10000))
 (let* ((tiny-hash (lambda (str) (remainder (string-hash str) 2)))
        (alist->tstring-hashmap
         (lambda (alst)
           (alist->hashmap string=? tiny-hash alst)))
        (alst (list-ec (:range i len)
                       `(,(number->string i 16) . ,i)))
        (hm (alist->tstring-hashmap alst)))
   (test-assert (every?-ec (:list pair alst)
                           (:let s (car pair))
                           (:let i (cdr pair))
                           (:let pair% (hashmap-ref hm s))
                           (:let s% (car pair%))
                           (:let i% (cdr pair%))
                           (and (string=? s s%) (= i i%))))
   (when (<= len 1000)
     (test-assert (lset= (lambda (a b)
                           (and (string=? (car a) (car b))
                                (= (cdr a) (cdr b))))
                         alst (hashmap->alist hm))))
   (test-equal len (hashmap-size hm))))

(do-ec
 (:list len (list 1 10 100 1000 10000 100000))
 (let* ((alist->string-hashmap
         (lambda (alst)
           (alist->hashmap string=? string-hash alst)))
        (alst1 (list-ec (:range i 0 (* 2 len))
                        `(,(number->string i 16) . ,i)))
        (hm1 (alist->string-hashmap alst1))
        (lst2 (list-ec (:range i 0 (* 2 len) 2)
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
   (when (<= len 1000)
     (test-assert
         (lset= string=?
                (lset-difference string=? (map car alst1) lst2)
                (map! car (hashmap->alist hm2)))))
   (test-equal len (hashmap-size hm2))))

(display successes)
(display " successes\n")
(display failures)
(display " failures\n")

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
