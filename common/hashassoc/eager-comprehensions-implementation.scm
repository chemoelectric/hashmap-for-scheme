;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-syntax hashassoc-gen
  (syntax-rules (index)
    ((¶ binding cc var (index i) arg)
     (:do cc
          (let ((gen! (hashassoc->generator arg))))
          ((var (gen!)) (i 0))
          (not (eof-object? var))
          binding
          #t
          ((gen!) (+ i 1))))
    ((¶ binding cc var (index i) arg1 arg2 ...)
     (:do cc
          (let ()
            (define gen*
              (list (hashassoc->generator arg1)
                    (hashassoc->generator arg2)
                    ...))
            (define (gen!)
              (let loop1 ((v ((car gen*))))
                (if (eof-object? v)
                  (begin
                    (set! gen* (cdr gen*))
                    (if (null? gen*)
                      (eof-object)
                      (loop1 ((car gen*)))))
                  v))))
          ((var (gen!)) (i 0))
          (not (eof-object? var))
          binding
          #t
          ((gen!) (+ i 1))))
    ((¶ binding cc var arg)
     (:do cc
          (let ((gen! (hashassoc->generator arg))))
          ((var (gen!)))
          (not (eof-object? var))
          binding
          #t
          ((gen!))))
    ((¶ binding cc var arg1 arg2 ...)
     (:do cc
          (let ()
            (define gen*
              (list (hashassoc->generator arg1)
                    (hashassoc->generator arg2)
                    ...))
            (define (gen!)
              (let loop1 ((v ((car gen*))))
                (if (eof-object? v)
                  (begin
                    (set! gen* (cdr gen*))
                    (if (null? gen*)
                      (eof-object)
                      (loop1 ((car gen*)))))
                  v))))
          ((var (gen!)))
          (not (eof-object? var))
          binding
          #t
          ((gen!))))
    ))

(define-syntax :hashassoc-pairs
  (syntax-rules (index)
    ((:hashassoc cc var (index i) arg)
     (hashassoc-gen (let ()) cc var (index i) arg))
    ((:hashassoc cc var (index i) arg1 arg2 ...)
     (hashassoc-gen (let ()) cc var (index i) arg1 arg2 ...))
    ((:hashassoc cc var arg)
     (hashassoc-gen (let ()) cc var arg))
    ((:hashassoc cc var arg1 arg2 ...)
     (hashassoc-gen (let ()) cc var arg1 arg2 ...))))

(define-syntax :hashassoc-keys
  (syntax-rules (index)
    ((:hashassoc cc var (index i) arg)
     (hashassoc-gen (let ((var (car var)))) cc var (index i) arg))
    ((:hashassoc cc var (index i) arg1 arg2 ...)
     (hashassoc-gen (let ((var (car var)))) cc var (index i) arg1 arg2 ...))
    ((:hashassoc cc var arg)
     (hashassoc-gen (let ((var (car var)))) cc var arg))
    ((:hashassoc cc var arg1 arg2 ...)
     (hashassoc-gen (let ((var (car var)))) cc var arg1 arg2 ...))))

(define-syntax :hashassoc-values
  (syntax-rules (index)
    ((:hashassoc cc var (index i) arg)
     (hashassoc-gen (let ((var (cdr var)))) cc var (index i) arg))
    ((:hashassoc cc var (index i) arg1 arg2 ...)
     (hashassoc-gen (let ((var (cdr var)))) cc var (index i) arg1 arg2 ...))
    ((:hashassoc cc var arg)
     (hashassoc-gen (let ((var (cdr var)))) cc var arg))
    ((:hashassoc cc var arg1 arg2 ...)
     (hashassoc-gen (let ((var (cdr var)))) cc var arg1 arg2 ...))))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
