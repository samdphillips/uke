#lang racket/base

(require (for-syntax "private/syntax.rkt")
         uke/column
         uke/dataframe
         qi)

(provide (for-space qi slice))

;; XXX: simple support for something like dataframe-reorder-column, maybe a
;;      different reorder operator
(define-qi-syntax-rule (slice s:slice-spec)
  (esc (λ (df)
         (define name-pred? s.pred?)
         (define column-name-pred?
           (lambda (col)
             (name-pred? (column-name col))))
         (define (update s*)
           (for/list ([col (in-list s*)]
                      #:when (column-name-pred? col))
             col))
         (dataframe-column*-update df update))))