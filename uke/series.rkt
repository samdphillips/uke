#lang racket/base

(require racket/sequence
         racket/unsafe/ops
         racket/vector
         "error.rkt"
         "index.rkt"
         "store.rkt"
         "util.rkt")

(provide make-series
         series?
         series-name
         series-size
         series-index
         series-store
         series-ref
         series-push-index
         series-compact?
         series-compact
         build-series
         ->series
         vector->series
         sequence->series)

;; XXX: series metadata

(struct series (name index store)
  #:transparent
  #:property prop:sequence
  (λ (s) (sequence-map (λ (i) (series-ref s i))
                       (in-indices (series-index s)))))

(define (series-size a-series)
  (index-size (series-index a-series)))

(define (make-series name index store)
  (series name index store))

(define (series-ref a-series i)
  (store-ref (series-store a-series) (index-ref (series-index a-series) i)))

(define (->series name seq)
  (cond
    [(vector? seq) (vector->series name seq)]
    [else (sequence->series name seq)]))

(define (vector->series name vec #:size [size #f] #:offset [offset #f])
  (define series-offset (or offset 0))
  (define vlen (vector-length vec))
  (define series-size   (or size (- vlen series-offset)))
  (cond
    [(> (+ series-offset series-size) vlen)
     (raise-uke-error exn:uke:series
                      'vector->series
                      "series size ~a at offset ~a is out of bounds for vector length ~a"
                      series-size series-offset vlen)]
    [(immutable? vec)
     (series name (make-linear-index series-size series-offset) vec)]
    [else
     (define i series-offset)
     (define j (+ series-offset series-size))
     (make-series name
                  (make-linear-index series-size 0 1)
                  (unsafe-vector*->immutable-vector!
                   (vector-copy vec i j)))]))

(define (sequence->series name seq)
  (define-values (store len)
    (sequence->list/length seq))
  (make-series name (make-linear-index len) (list->immutable-vector store)))

(define (series-push-index s idx)
  (struct-copy series s [index (index-compose (series-index s) idx)]))

(define (series-compact? a-series)
  (define idx (series-index a-series))
  (and (index-compact? idx)
       (= (store-length (series-store a-series))
          (index-size idx))))

(define (series-compact a-series)
  (cond
    [(series-compact? a-series) a-series]
    [else
     (build-series (series-name a-series)
                   (series-size a-series)
                   (λ (i) (series-ref a-series i)))]))

(define (build-series name size f)
  (make-series name
               (make-linear-index size 0 1)
               (unsafe-vector*->immutable-vector!
                (build-vector size f))))

