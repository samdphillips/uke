#lang racket/base

(require (for-syntax syntax/parse)
         qi
         uke/dataframe)

(provide (for-space qi where))

(define-qi-syntax-rule (where (col-name:id ...) flo)
  (esc (λ (df)
         (define pred?
           (dataframe-column-lift df (flow flo) 'col-name ...))
         (dataframe-select df pred?))))