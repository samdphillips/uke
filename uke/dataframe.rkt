#lang racket/base

(require syntax/parse/define
         "index.rkt"
         "series.rkt")

(provide (struct-out dataframe)
         dataframe-num-rows
         dataframe-series
         dataframe-series-ref
         dataframe-add-series*
         dataframe-remove-series*
         for/dataframe)

(struct dataframe (index series*))

;; XXX: actually compute a compatible index
;; XXX: move to index.rkt
(define (compatible-index a-series-list)
  (series-index (car a-series-list)))

(define (make-dataframe a-series-list #:index [an-index (compatible-index a-series-list)])
  (dataframe an-index a-series-list))

(define (dataframe-num-rows df)
  (index-size (dataframe-index df)))

(define (dataframe-series a-dataframe)
  (define an-index (dataframe-index a-dataframe))
  (for/list ([a-series (in-list (dataframe-series* a-dataframe))])
    (series-push-index a-series an-index)))

(define (dataframe-series-ref df a-series-name)
  (for/first ([a-series (in-list (dataframe-series* df))]
              #:when (equal? (series-name a-series)
                             a-series-name))
    (series-push-index a-series (dataframe-index df))))

(define (dataframe-add-series* df . new-series)
  ;; XXX check that new-series index is compatible with the dataframe index
  (define df-idx (dataframe-index df))
  (define series*
    (for/list ([a-series (in-list (dataframe-series* df))])
      (series-push-index a-series df-idx)))
  (struct-copy dataframe df
               [index   (seq-identity-index (index-size df-idx))]
               [series* (append series* new-series)]))

(define (dataframe-remove-series* df . series-names)
  (define series*
    (for/list ([a-series (in-list (dataframe-series* df))]
               #:unless (memq (series-name a-series) series-names))
      a-series))
  (struct-copy dataframe df [series* series*]))

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

