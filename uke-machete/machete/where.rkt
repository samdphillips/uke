#lang racket/base

(require (for-syntax syntax/parse)
         qi
         uke/dataframe)

(provide (for-space qi where))

(define-qi-syntax-rule (where (series-name:id ...) flo)
  (esc (λ (df)
         (define pred?
           (dataframe-series-lift df '(series-name ...) (flow flo)))
         (dataframe-select df pred?))))