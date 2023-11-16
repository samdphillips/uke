#lang racket/base

(require (for-syntax racket/base)
         uke/dataframe
         uke/index
         syntax/parse/define)

(provide group-with)

(define-syntax-parse-rule (group-with df-expr series-name:id ...)
  #:declare df-expr (expr/c #'dataframe?)
  (let ()
    (define df df-expr.c)
    (define key-func
      (dataframe-series-lift df (list 'series-name ...) list))
    (dataframe-group df key-func)))
