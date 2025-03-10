#lang racket/base

(require (for-syntax syntax/parse)
         qi
         uke/dataframe)

(provide (for-space qi where))

(define-qi-syntax-rule (where (col-name:id ...) flo)
  (esc (λ (df)
         (define pred?
           (dataframe-column-lift df '(col-name ...) (flow flo)))
         (dataframe-select df pred?))))