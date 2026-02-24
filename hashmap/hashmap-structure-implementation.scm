;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
;;;-------------------------------------------------------------------

(define-record-factory <hashmap>
  (constructor> make-hashmap
                (lambda (construct)
                  (case-lambda
                    (() (construct 0 #f))
                    ((pred key->hash . alst)
                     (let ((hm (construct 0 #f)))
                       ;; ;;;;; FIXME: DO THE alist-> here. ;;;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
                       hm)))))
  (constructor> alist->hashmap
                (lambda (construct)
                  (lambda (pred key->hash alst)
                    (let ((hm (construct 0 #f)))
                      ;; ;;;;; FIXME: DO THE alist-> here. ;;;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
                      hm))))
  (predicate> hashmap?)
  (getter> 1 hashmap-count)
  (setter> 1 set-hashmap-count!)
  (getter> 2 hashmap-trie)
  (getter> 2 set-hashmap-trie!))

;;;-------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
