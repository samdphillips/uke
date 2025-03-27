#lang racket/base

(require csv-reading
         racket/sequence
         racket/unsafe/ops
         uke/column
         uke/dataframe
         uke/index)

(provide csv->dataframe)

(define (csv->dataframe inp
                        [reader-spec null]
                        #:column-names [col-names 'first])
  (define next-row (make-csv-reader inp reader-spec))
  (define (list->vector* vs)
    (unsafe-vector*->immutable-vector!
     (list->vector vs)))
  (define (make-producer)
    (in-producer (λ ()
                   (define vs (next-row))
                   (if (null? vs)
                       vs
                       (list->vector* vs)))
                 null?))
  (define-values (names rows)
    (cond
      [(eq? 'first col-names)
       (values (map string->symbol (next-row)) (make-producer))]
      [(not col-names)
       (define first (next-row))
       (values (map (λ (v) (gensym 'column-)) first)
               (sequence-append (list (list->vector* first))
                                (make-producer)))]
      [(list? col-names)
       (values col-names (make-producer))]))
  (define ((col-ref i) v) (vector-ref v i))
  (define store
    (unsafe-vector*->immutable-vector!
     (for/vector ([r rows]) r)))
  (define idx (make-linear-index (vector-length store)))
  (define col*
    (for/list ([name (in-list names)]
               [i (in-naturals)])
      (make-column name idx store #:projection (col-ref i))))
  (make-dataframe #:index idx col*))
