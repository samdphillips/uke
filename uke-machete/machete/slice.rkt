#lang racket/base

(require (for-syntax "private/syntax.rkt")
         uke/dataframe
         uke/series
         qi)

(provide (for-space qi slice))

;; XXX: simple support for something like dataframe-reorder-series, maybe a
;;      different reorder operator
(define-qi-syntax-rule (slice s:slice-spec)
  (esc (λ (df)
         (define name-pred? s.pred?)
         (define series-name-pred?
           (lambda (a-series)
             (name-pred? (series-name a-series))))
         (define (update s*)
           (for/list ([a-series (in-list s*)]
                      #:when (series-name-pred? a-series))
             a-series))
         (dataframe-series*-update df update))))