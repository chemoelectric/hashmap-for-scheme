;; Copyright © 2026 Barry Schwartz
;; SPDX-License-Identifier: MIT

(define-library (hashmap define-record-factory)

  (export define-record-factory)

  (import (only (chicken base) gensym))
  (import (only (chicken syntax) er-macro-transformer))
  (import (scheme base))
  (import (srfi 1))

  (begin

    (define-syntax define-record-factory
      (er-macro-transformer
       (lambda (form rename compare)

         (define rn rename)

         (define (caddr x) (cadr (cdr x)))
         (define (cadddr x) (cadr (cddr x)))

         (define one-arg?
           (lambda (rule*)
             (and (pair? rule*)
                  (list? (car rule*))
                  (= (length (car rule*)) 2))))

         (define two-arg?
           (lambda (rule*)
             (and (pair? rule*)
                  (list? (car rule*))
                  (= (length (car rule*)) 3))))

         (define three-arg?
           (lambda (rule*)
             (and (pair? rule*)
                  (list? (car rule*))
                  (= (length (car rule*)) 4))))

         (define arg1 (lambda (rule*) (cadr (car rule*))))
         (define arg2 (lambda (rule*) (caddr (car rule*))))
         (define arg3 (lambda (rule*) (cadddr (car rule*))))

         (define (rule-match? symb)
           (lambda (rule*)
             (and (symbol? (caar rule*))
                  (compare (caar rule*) (rn symb)))))

         (define constructor>? (rule-match? 'constructor>))
         (define predicate>? (rule-match? 'predicate>))
         (define getter>? (rule-match? 'getter>))
         (define setter>? (rule-match? 'setter>))

         (define define-record-type% (rn 'define-record-type))
         (define define% (rn 'define))
         (define lambda% (rn 'lambda))
         (define begin% (rn 'begin))
         (define vector-ref% (rn 'vector-ref))
         (define vector-set!% (rn 'vector-set!))
         (define list->vector% (rn 'list->vector))
         (define minus% (rn '-))

         (define (error-in-rule rule*)
           (error "syntax error in rule" (car rule*)))

         (let ((n (length form)))
           (cond
             ((= n 1)
              (error "record type designation is missing" form))
             ((not (symbol? (cadr form)))
              (error "record type designation is not a symbol"
                     (cadr form)))
             (else
              (let* ((designation (cadr form))
                     (original-constructor% (gensym))
                     (constructor% (gensym))
                     (original-predicate% (gensym))
                     (predicate% (gensym))
                     (fields% (gensym))
                     (access% (gensym)))
                (let loop ((code '())
                           (rule* (cddr form)))
                  (cond
                    ((one-arg? rule*)
                     (cond
                       ((constructor>? rule*)
                        (let* ((name (arg1 rule*))
                               (definition
                                 `(,define% ,name ,constructor%)))
                          (loop (cons definition code) (cdr rule*))))
                       ((predicate>? rule*)
                        (let* ((name (arg1 rule*))
                               (definition
                                 `(,define% ,name ,predicate%)))
                          (loop (cons definition code) (cdr rule*))))
                       (else (error-in-rule rule*))))
                    ((two-arg? rule*)
                     (cond
                       ((constructor>? rule*)
                        (let* ((name (arg1 rule*))
                               (proc (arg2 rule*))
                               (definition
                                 `(,define% ,name (,proc ,constructor%))))
                          (loop (cons definition code) (cdr rule*))))
                       ((predicate>? rule*)
                        (let* ((name (arg1 rule*))
                               (proc (arg2 rule*))
                               (definition
                                 `(,define% ,name (,proc ,predicate%))))
                          (loop (cons definition code) (cdr rule*))))
                       ((getter>? rule*)
                        (let* ((i (arg1 rule*))
                               (name (arg2 rule*))
                               (definition
                                 `(,define% ,name
                                    (,lambda%
                                     (obj)
                                     (,vector-ref% (,access% obj)
                                                   (,minus% ,i 1))))))
                          (loop (cons definition code) (cdr rule*))))
                       ((setter>? rule*)
                        (let* ((i (arg1 rule*))
                               (name (arg2 rule*))
                               (definition
                                 `(,define% ,name
                                    (,lambda%
                                     (obj value)
                                     (,vector-set!% (,access% obj)
                                                    (,minus% ,i 1)
                                                    value)))))
                          (loop (cons definition code) (cdr rule*))))
                       (else (error-in-rule rule*))))
                    ((three-arg? rule*)
                     (cond
                       ((getter>? rule*)
                        (let* ((i (arg1 rule*))
                               (name (arg2 rule*))
                               (proc (arg3 rule*))
                               (definition
                                 `(,define% ,name
                                    (,proc
                                     (,lambda%
                                      (obj)
                                      (,vector-ref% (,access% obj)
                                                    (,minus% ,i 1)))))))
                          (loop (cons definition code) (cdr rule*))))
                       ((setter>? rule*)
                        (let* ((i (arg1 rule*))
                               (name (arg2 rule*))
                               (proc (arg3 rule*))
                               (definition
                                 `(,define% ,name
                                    (,proc
                                     (,lambda%
                                      (obj value)
                                      (,vector-set!% (,access% obj)
                                                     (,minus% ,i 1)
                                                     value))))))
                          (loop (cons definition code) (cdr rule*))))
                       (else (error-in-rule rule*))))
                    (else
                     (let ((heading
                            `(,begin%
                              (,define-record-type% ,designation
                                (,original-constructor% ,fields%)
                                ,original-predicate%
                                (,fields% ,access%))
                              (,define% (,constructor% . obj)
                                (,original-constructor%
                                 (,list->vector% obj)))
                              (,define% (,predicate% obj)
                                (,original-predicate% obj)))))
                       (append heading (reverse code)))))))))))))

    ))

;;; local variables:
;;; mode: scheme
;;; geiser-scheme-implementation: chicken
;;; coding: utf-8
;;; end:
