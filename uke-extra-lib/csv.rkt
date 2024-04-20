#lang racket/base

(require csv-reading
         racket/sequence
         racket/unsafe/ops
         uke/dataframe
         uke/index
         uke/series
         uke/util)

(provide csv->dataframe)

(define (csv->dataframe inp
                        [reader-spec null]
                        #:series-names [series-names 'first])
  (define next-row (make-csv-reader inp reader-spec))
  (define (make-producer) (in-producer next-row null?))
  (define-values (names rows)
    (cond
      [(eq? 'first series-names)
       (values (map string->symbol (next-row)) (make-producer))]
      [(not series-names)
       (define first (next-row))
       (values (map (λ (v) (gensym 'series-)) first)
               (sequence-append (list first)
                                (make-producer)))]
      [(list? series-names)
       (values series-names (make-producer))]))
  (define ((col-ref i) v) (list-ref v i))
  (define store
    (unsafe-vector*->immutable-vector!
     (for/vector ([r rows]) r)))
  (define idx (make-linear-index (vector-length store)))
  (define series
    (for/list ([name (in-list names)]
               [i (in-naturals)])
      (make-series name idx store #:projection (col-ref i))))
  (make-dataframe #:index idx series))
