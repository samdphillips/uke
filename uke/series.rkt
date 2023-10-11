#lang racket/base

(require racket/sequence
         racket/unsafe/ops
         racket/vector
         "index.rkt"
         "store.rkt"
         "util.rkt")

(provide series?
         series-name
         series-index
         series-store
         series-ref
         series-push-index
         ->series
         vector->series
         sequence->series)

(struct series (name index store)
  #:transparent
  #:property prop:sequence
  (λ (s) (sequence-map (λ (i) (series-ref s i))
                       (in-indices (series-index s)))))

(define (series-ref a-series i)
  (store-ref (series-store a-series) (index-ref (series-index a-series) i)))

(define (->series name seq)
  (cond
    [(vector? seq) (vector->series name seq)]
    [else (sequence->series name seq)]))

(define (vector->series name vec #:size [size #f] #:offset [offset #f])
  (define series-offset (or offset 0))
  (define series-size   (or size (- (vector-length vec) series-offset)))
  (cond
    [(immutable? vec)
     (series name (make-linear-index series-size series-offset) vec)]
    [else
     (define i series-offset)
     (define j (+ series-offset series-size))
     (series name
             (make-linear-index series-size 0 1)
             (unsafe-vector*->immutable-vector!
              (vector-copy vec i j)))]))

(define (sequence->series name seq)
  (define-values (store len)
    (sequence->list/length seq))
  (series name (make-linear-index len) (list->immutable-vector store)))

(define (series-push-index s idx)
  (struct-copy series s [index (index-compose (series-index s) idx)]))

;; build-series