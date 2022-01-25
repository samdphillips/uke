#lang racket/base

(require syntax/parse/define
         "index.rkt"
         "series.rkt")

(provide (struct-out dataframe)
         for/dataframe)

(struct dataframe (index series*))

;; XXX: actually compute a compatible index
;; XXX: move to index.rkt
(define (compatible-index a-series-list)
  (series-index (car a-series-list)))

(define (make-dataframe a-series-list #:index [an-index (compatible-index a-series-list)])
  (dataframe an-index a-series-list))

(struct dataframe-builder (column-names num-rows columns) #:mutable)

(define initial-dataframe-builder-size (make-parameter 256))

(define (make-dataframe-builder column-names)
  (define column-size (initial-dataframe-builder-size))
  (define columns
    (build-vector (length column-names)
                  (lambda (i) (make-vector column-size))))
  (dataframe-builder column-names 0 columns))

(define (dataframe-builder-build builder)
  (define col-names (dataframe-builder-column-names builder))
  (define size (dataframe-builder-num-rows builder))
  (define col-vecs (dataframe-builder-columns builder))
  (define series-list
    (for/list ([col-name (in-list col-names)]
               [col-data (in-vector col-vecs)])
      (vector->series col-name col-data size)))
  (make-dataframe series-list))

(define (vector-grow vec)
  (define orig-size (vector-length vec))
  (define size (* 2 orig-size))
  (define new-vec (make-vector size))
  (vector-copy! new-vec 0 vec)
  new-vec)

(define (dataframe-builder-column-maybe-grow size columns i)
  (define col (vector-ref columns i))
  (define grow? (< (vector-length col) size))
  (when grow?
    (set! col (vector-grow col))
    (vector-set! columns i col))
  col)

(define (dataframe-builder-add-row! builder value-list)
  (define ri (dataframe-builder-num-rows builder))
  (define columns (dataframe-builder-columns builder))
  (set-dataframe-builder-num-rows! builder (add1 ri))
  (define nri (add1 ri))
  (set-dataframe-builder-num-rows! builder nri)
  (for ([i (in-naturals)]
        [val (in-list value-list)])
    (define col (dataframe-builder-column-maybe-grow nri columns i))
    (vector-set! col ri val)))

(define-syntax-parse-rule (for/dataframe (column-names:id ...) for-clauses body ...)
  #:with this-syntax this-syntax
  (let ([builder (make-dataframe-builder '(column-names ...))])
    (for/fold/derived this-syntax
                      ()
                      for-clauses
                      (call-with-values
                        (lambda () body ...)
                        (lambda (column-names ...)
                          (dataframe-builder-add-row! builder (list column-names ...))
                          (values))))
    (dataframe-builder-build builder)))

