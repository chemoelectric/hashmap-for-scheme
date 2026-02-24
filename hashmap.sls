;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashmap)

  (export)

  (import (except (rnrs base (6)) for-each map)
          (hashmap-include include)
          (hashmap define-record-factory))

  )

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chez
;;; coding: utf-8
;;; end:
