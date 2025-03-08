#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "private/syntax.rkt")
         qi
         racket/unsafe/ops
         uke/column
         uke/dataframe
         uke/index)

(provide (for-space qi create))

;; XXX create multiple columns from a single create-column-spec
(define-qi-syntax-rule (create col:create-column-spec ...)
  (esc (λ (df)
         (define n (dataframe-num-rows df))
         (define col.vec-id (make-vector n #f)) ...
         (define col.flow-id col.flow) ...
         ;; XXX change error when df is missing column name
         ;; XXX this binds the same column multiple times
         (define-values (col.in-col-var ...)
           (values (dataframe-column*-ref df 'col.in-col-name) ...)) ...
         (define idx (dataframe-index df))
         (for ([i (in-indices idx)])
           (vector-set! col.vec-id i
                        (col.flow-id
                         (dataframe-cell-ref* idx col.in-col-var i) ...))
           ...)
         (dataframe-add-column*
          (dataframe-remove-column* df 'col.name ...)
          (vector->column 'col.name
                          (unsafe-vector*->immutable-vector! col.vec-id)
                          #:properties
                          (hash {~@ 'col.property col.property-expr}
                                ...)) ...))))