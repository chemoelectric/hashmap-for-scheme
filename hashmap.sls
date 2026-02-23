;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashmap)

  (export)

  (import (rnrs (6))
          (define-record-factory))

  (define-syntax include
    ;;
    ;; From the R⁶RS library report.
    ;;
    (lambda (x)
      (define read-file
        (lambda (fn k)
          (let ((p (open-file-input-port
                    fn
                    (file-options)
                    (buffer-mode block)
                    (native-transcoder))))
            (let f ((x (get-datum p)))
              (if (eof-object? x)
                (begin (close-port p) '())
                (cons (datum->syntax k x)
                      (f (get-datum p))))))))
      (syntax-case x ()
        ((k filename)
         (let ((fn (syntax->datum (syntax filename))))
           (with-syntax (((exp ...)
                          (read-file fn (syntax k))))
             (syntax (begin exp ...))))))))

  (include "hashmap.scm")

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
