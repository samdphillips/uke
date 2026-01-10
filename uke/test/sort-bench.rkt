#lang racket/base

(require racket/format
         racket/list
         racket/match
         racket/sequence
         uke
         (submod uke/private/index for-bench)
         uke/machete)

(define (do-index-update df index-sort)
  (define f (dataframe-column-lift df values 'x))
  (define (lt? a b) (< (f a) (f b)))
  (define kind
    (match (dataframe-index df)
      [(? linear-index?) 'lin]
      [(? vector-index?) 'vec]))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (λ (idx)
    (display (~a "[" kind "]: "))
    (time (index-sort idx lt?))))

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

(for* ([index-sort (in-list index-sort*)]
       [size (in-list '(10000 100000 500000))]
       #:do [(displayln (~a "=== " (object-name index-sort) " / " size " ==="))]
       [make-df (in-list make-df*)])
  (define df (make-df size))
  (dataframe-index-update df (do-index-update df index-sort)))

#|

;; df0 is a linear index

(define df1
  (dataframe-index-update df0 (do-index-update df0 'lin index-sort/list)))

(define df2
  (dataframe-index-update df0 (do-index-update df0 'lin index-sort/heap)))

;; df2 is a vector index

(define df3
  (dataframe-index-update df2 (do-index-update df2 'vec index-sort/list)))

(define df4
  (dataframe-index-update df2 (do-index-update df2 'vec index-sort/heap)))
|#