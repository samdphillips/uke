#lang racket/base

(require racket/generic
         "index.rkt"
         "util.rkt")

(define-generics series
  ;; series-name :: Series -> String / #f
  (series-name series)
  ;; series-index :: Series -> Index
  (series-index series)
  ;; Series -> Vector
  (series-store series)
  ;; Series K -> V
  (series-ref series k)

  #:defaults
  ([immutable-vector?
    (define (series-name vec) #f)

    (define (series-index vec)
      (identity-index (vector-length vec)))

    (define (series-store vec) vec)])

  #:fallbacks
  [(define (series-ref s i) (default-series-ref s i))])

(define (default-series-ref s i)
  (define j (index-lookup (series-index s) i))
  (vector-ref (series-store s) j))

(struct basic-series (name index store)
  #:methods gen:series
  [(define (series-name s) (basic-series-name s))
   (define (series-index s) (basic-series-index s))
   (define (series-store s) (basic-series-store s))])


