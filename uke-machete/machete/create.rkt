#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "private/syntax.rkt")
         qi
         racket/unsafe/ops
         uke/dataframe
         uke/index
         uke/series)

(provide (for-space qi create))

(define-qi-syntax-rule (create series:create-column-spec ...)
  (esc (λ (df)
         (define n (dataframe-num-rows df))
         (define series.vec-id (make-vector n #f)) ...
         (define series.flow-id series.flow) ...
         ;; XXX check df for column names
         ;; XXX this binds the same series multiple times
         (define-values (series.in-series-var ...)
           (values (dataframe-series*-ref df 'series.in-series-name) ...)) ...
         (define idx (dataframe-index df))
         (for ([i (in-indices idx)])
           (vector-set! series.vec-id i
                        (series.flow-id
                         (dataframe-cell-ref* idx series.in-series-var i) ...))
           ...)
         (dataframe-add-series*
          (dataframe-remove-series* df 'series.name ...)
          (vector->series 'series.name
                          (unsafe-vector*->immutable-vector! series.vec-id)
                          #:properties
                          (hash {~@ 'series.property series.property-expr}
                                ...)) ...))))