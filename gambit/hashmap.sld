;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashmap)

  (export make-hashmap
          alist->hashmap
          hashmap->alist
          hashmap->generator

          hashmap?

          hashmap-size
          hashmap-empty?
          hashmap-ref

          hashmap-set!
          hashmap-set-from-alist!

          hashmap-delete!
          hashmap-delete-from-list!)

  (import (except (scheme base)
                  assv assq assoc
                  append cons list make-list length
                  car cdr caar cadr cdar cddr
                  set-car! set-cdr!
                  for-each map list-copy list-ref
                  member memv memq null? pair? reverse)
          (scheme case-lambda))
  (import (srfi 1))
  (import (srfi 151))
  (import (rename (gambit prim fixnum)
                  (fx= fx=?)
                  (fx< fx<?)
                  (fx<= fx<=?)))

  (begin

    (define-syntax hash-bits-chunk-max
      ;;
      ;; WARNING: This value is for eight-byte fixnums. For four-byte
      ;; fixnums, change the value to 4.
      ;;
      (syntax-rules ()
        ((¶) 5)))

    (define-syntax hash-bits-max
      ;;
      ;; We use the 60 least significant bits. This is the size of a
      ;; fixnum in Chez Scheme, and hopefully is as small as a fixnum
      ;; gets.
      ;;
      ;; WARNING: If necessary, change the number 60 to a lower
      ;; number.
      ;;
      (syntax-rules ()
        ((¶) 60)))

    (define-syntax define-record-factory
      (lambda (stx)

        (define (insert-identifiers . x*)
          ;; Create a list of gensym-generated identifiers within the
          ;; syntactic context. (This is not the same datum->syntax as
          ;; that of R⁶RS, and this gensym is not the same as in the
          ;; (chezscheme) library of Chez Scheme.)
          (datum->syntax stx (map gensym x*)))

        (syntax-case stx ()
          ((_ designation rule ...)
           (syntax-case
               (insert-identifiers
                'original-constructor%
                'constructor%
                'original-predicate%
                'predicate%
                'fields%
                'access%) ()
             ((original-constructor%
               constructor%
               original-predicate%
               predicate%
               fields%
               access%)
              (syntax ;; Avoid #' because many readers cannot handle it.
               (begin
                 (define-record-type designation
                   (original-constructor% fields%)
                   original-predicate%
                   (fields% access%))
                 (define (constructor% . obj)
                   (original-constructor% (list->vector obj)))
                 (define (predicate% obj)
                   (original-predicate% obj))
                 (define-syntax record-rule
                   (syntax-rules ( constructor>
                                   predicate>
                                   getter> setter> )

                     ((_ constructor predicate access (constructor> name proc))
                      (define name (proc constructor)))

                     ((_ constructor predicate access (constructor> name))
                      (define name constructor))

                     ((_ constructor predicate access (predicate> name proc))
                      (define name (proc predicate)))

                     ((_ constructor predicate access (predicate> name))
                      (define name predicate))

                     ((_ constructor predicate access (getter> i name proc))
                      (define name
                        (proc
                         (lambda (obj)
                           (vector-ref (access obj) (- i 1))))))

                     ((_ constructor predicate access (getter> i name))
                      (define name
                        (lambda (obj)
                          (vector-ref (access obj) (- i 1)))))

                     ((_ constructor predicate access (setter> i name proc))
                      (define name
                        (proc
                         (lambda (obj value)
                           (vector-set! (access obj) (- i 1) value)))))

                     ((_ constructor predicate access (setter> i name))
                      (define name
                        (lambda (obj value)
                          (vector-set! (access obj) (- i 1) value))))))

                 (record-rule constructor% predicate% access%
                              rule)
                 ...))))))))
    
    (define fxbit-field bit-field)
    (include "hashmap/low-level-implementation.scm")
    (include "hashmap/hashmap-structure-implementation.scm")

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: gambit
;;; coding: utf-8
;;; end:
