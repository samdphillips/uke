#lang racket/base

(require racket/format
         racket/list
         racket/match
         racket/sequence
         uke
         (submod uke/private/index for-bench)
         uke/machete)

(define (make-df-linear size)
  (for/dataframe (x) ([i (in-range size)]) (random)))

(define (make-df-vector size)
  (let ([df (make-df-linear size)]
        [shuffle-indices
         (λ (idx)
           (make-vector-index
            (list->vector
             (shuffle
              (sequence->list (in-indices idx))))))])
    (dataframe-index-update df shuffle-indices)))

(define make-df* (list make-df-linear make-df-vector))
(define index-sort* (list index-sort/list index-sort/heap index-sort/vector))

(define (~csv vs)
  (apply ~a #:separator "," vs))

(define (write-csv df outp)
  (define columns (dataframe-columns df))
  (displayln
   (~csv (for/list ([col (in-list columns)]) (column-name col)))
   outp)
  (for ([i (in-indices (dataframe-index df))])
    (displayln
     (~csv (for/list ([col (in-list columns)]) (column-ref col i)))
     outp)))

(call-with-output-file #:exists 'replace "sort-bench.csv"
  (λ (outp)
    (write-csv
     (for*/dataframe (sort index-type size real-time cpu-time gc-time)
       ([index-sort (in-list index-sort*)]
        [size (in-list '(10000 100000 500000 1000000))]
        #:do [(displayln (~a #:separator " / "
                             (object-name index-sort)
                             size))]
        [make-df (in-list make-df*)]
        [trials 6])
       (define df (make-df size))
       (define idx (dataframe-index df))
       (define f (dataframe-column-lift df values 'x))
       (define (lt? a b) (< (f a) (f b)))
       (collect-garbage)
       (collect-garbage)
       (collect-garbage)
       (define-values (_result cpu-time real-time gc-time)
         (time-apply index-sort (list idx lt?)))
       (values (object-name index-sort)
               (match idx
                 [(? linear-index?) 'lin]
                 [(? vector-index?) 'vec])
               size
               real-time
               cpu-time
               gc-time))
     outp)))
