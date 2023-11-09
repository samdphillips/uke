#lang racket/base

(require (for-syntax racket/base
                     "syntax.rkt")
         racket/unsafe/ops
         syntax/parse/define
         uke/u2/main)

(provide create)

(define-syntax-parse-rule (create df-expr column:create-column-spec ...)
  #:declare df-expr (expr/c #'dataframe?)
  (let ()
    (define df df-expr.c)
    (define n (dataframe-num-rows df))
    (define column.vec-id (make-vector n #f)) ...
    (define column.func-id column.func) ...
    ;; XXX check df for column names
    (define-values (column.series-id ...)
      (values (dataframe-series-ref df 'column.binder) ...)) ...
    (define dfi (dataframe-index df))
    (for ([i (in-range (index-size dfi))])
      (vector-set! column.vec-id i
                   (column.func-id (series-ref column.series-id i) ...))
      ...)
    (dataframe-add-series* df
                           (vector->series 'column.name
                                           (unsafe-vector*->immutable-vector!
                                            column.vec-id)) ...)))

