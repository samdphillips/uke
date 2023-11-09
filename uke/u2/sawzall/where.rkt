#lang racket/base

(require (for-syntax racket/base)
         racket/unsafe/ops
         syntax/parse/define
         uke/u2/main)

(provide where)

;; XXX needs to also work with categorical index
(define-syntax-parse-rule (where df-expr (binder:id ...) body ...+)
  #:declare df-expr (expr/c #'dataframe?)
  #:with (binder-series ...) (generate-temporaries #'(binder ...))
  (let ()
    (define df df-expr.c)
    ;; XXX check df for column names
    (define binder-series (dataframe-series-ref df 'binder)) ...
    (define (pred? binder ...)
      (define result (let () body ...))
      result)
    (define new-index
      (index-compose
       (dataframe-index df)
       (make-vector-index
        (unsafe-vector*->immutable-vector!
         (for/vector ([i (in-range (index-size (dataframe-index df)))]
                      #:when (pred? (series-ref binder-series i) ...)) i)))))
    (struct-copy dataframe df [index new-index])))
