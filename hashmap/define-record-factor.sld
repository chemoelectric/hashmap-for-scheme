;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashmap define-record-factory)

  (export define-record-factory)

  (import (scheme base))

  (begin

    (define-syntax define-record-factory
      (syntax-rules ()
        ((_ designation rule ...)
         (begin

           (define-record-type designation
             (original-constructor% fields)
             predicate
             (fields access))

           (begin
             (define-syntax record-rule
               (syntax-rules (constructor>
                              predicate>
                              getter> setter>)

                 ((_ constructor predicate access
                     (constructor> name proc))

                  (define name (proc constructor)))

                 ((_ constructor predicate access
                     (constructor> name))

                  (define name constructor))

                 ((_ constructor predicate access
                     (predicate> name proc))

                  (define name (proc predicate)))

                 ((_ constructor predicate access
                     (predicate> name))

                  (define name predicate))

                 ((_ constructor predicate access
                     (getter> i name proc))

                  (define name
                    (proc
                     (lambda (obj)
                       (vector-ref (access obj) (- i 1))))))

                 ((_ constructor predicate access
                     (getter> i name))

                  (define name
                    (lambda (obj)
                      (vector-ref (access obj) (- i 1)))))

                 ((_ constructor predicate access
                     (setter> i name proc))

                  (define name
                    (proc
                     (lambda (obj value)
                       (vector-set! (access obj) (- i 1) value)))))

                 ((_ constructor predicate access
                     (setter> i name))

                  (define name
                    (lambda (obj value)
                      (vector-set! (access obj) (- i 1) value))))))

             (record-rule
              (lambda fields
                (original-constructor% (list->vector fields)))
              predicate access rule)
             ...)))))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chibi
;;; coding: utf-8
;;; end:
