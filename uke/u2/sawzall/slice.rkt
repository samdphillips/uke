#lang racket/base

(require (for-syntax racket/base
                     "syntax.rkt")
         syntax/parse/define
         uke/u2/main)

(provide slice)

(define-syntax-parse-rule (slice df-expr s:slice-spec)
  #:declare df-expr (expr/c #'dataframe?)
  (let ()
    (define df df-expr.c)
    (define name-pred? s.pred?)
    (define series-name-pred?
      (λ (a-series)
        (name-pred? (series-name a-series))))
    (define series*
      (for/list ([a-series (dataframe-series df)]
                 #:when (series-name-pred? a-series))
        a-series))
    (struct-copy dataframe df [series* series*])))

