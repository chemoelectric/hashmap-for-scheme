;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(test-begin "big alist of strings -> hashmap")
(let* ((make-string-hashmap
        (lambda arg*
          (apply make-hashmap (cons* string=? string-hash arg*))))
       (alist->string-hashmap
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
  (test-equal (length alst) (hashmap-size hm)))
(test-end)

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
