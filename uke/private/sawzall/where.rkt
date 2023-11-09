#lang racket/base

(require (for-syntax racket/base)
         racket/unsafe/ops
         syntax/parse/define
         uke/dataframe
         uke/index
         uke/series)

(provide where)

(define-syntax-parse-rule (where df-expr (binder:id ...) body ...+)
  #:declare df-expr (expr/c #'dataframe?)
  #:with (binder-series ...) (generate-temporaries #'(binder ...))
  (let ()
    (define df df-expr.c)
    ;; XXX check df for series names
    (define df-index (dataframe-index df))
    (define binder-series (dataframe-series*-ref df 'binder)) ...
    (define (pred? i)
      (define binder (dataframe-cell-ref* df-index binder-series i)) ...
      (define result (let () body ...))
      result)
    (dataframe-select df pred?)))
