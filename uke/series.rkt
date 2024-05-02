#lang racket/base

(require racket/format
         racket/sequence
         racket/unsafe/ops
         racket/vector
         "error.rkt"
         "index.rkt"
         "store.rkt"
         "util.rkt")

(provide make-series
         series?
         series=?
         series*=?
         series-name
         series-size
         series-index
         series-properties
         series-projection
         series-store
         series-name-update
         series-index-update
         series-properties-update
         series-projection-update
         series-ref
         series-property-ref
         series-push-index
         series-compact?
         series-compact
         series-slice
         series-render-cell
         build-series
         ->series
         vector->series
         sequence->series)

(struct series (name properties index projection store)
  #:transparent
  #:property prop:sequence
  (λ (s) (sequence-map (λ (i) (series-ref s i))
                       (in-indices (series-index s)))))

(define (series-name-update a-series f)
  (struct-copy series a-series [name (f (series-name a-series))]))

(define (series-index-update a-series f)
  (struct-copy series a-series [index (f (series-index a-series))]))

(define (series-properties-update a-series f)
  (struct-copy series a-series [properties (f (series-properties a-series))]))

(define (series-projection-update a-series f)
  (struct-copy series a-series [projection (f (series-projection a-series))]))

(define (series-size a-series)
  (index-size (series-index a-series)))

(define (series=? s1 s2)
  (and (= (series-size s1) (series-size s2))
       (equal? (series-name s1) (series-name s2))
       (for/and ([i (in-indices (series-index s1))])
         (equal? (series-ref s1 i)
                 (series-ref s2 i)))))

(define (series*=? idx1 s1 idx2 s2)
  (define ((make-ref idx s) i)
    (series-ref s (index-ref idx i)))
  (define ref1 (make-ref idx1 s1))
  (define ref2 (make-ref idx2 s2))
  (and (= (index-size idx1) (index-size idx2))
       (equal? (series-name s1) (series-name s2))
       (for/and ([i (in-indices idx1)])
         (equal? (ref1 i) (ref2 i)))))

(define (make-series name
                     index
                     store
                     #:projection [projection #f]
                     #:properties [properties (hash)])
  (series name properties index projection store))

(define (apply-series-projection a-series v)
  (cond
    [(series-projection a-series) => (λ (p) (p v))]
    [else v]))

(define (series-ref a-series i)
  (define si (index-ref (series-index a-series) i))
  (define v (store-ref (series-store a-series) si))
  (apply-series-projection a-series v))

(define (series-property-ref a-series property [default #f])
  (hash-ref (series-properties a-series) property (λ () default)))

(define (->series name
                  seq
                  #:projection [projection #f]
                  #:properties [properties (hash)])
  (cond
    [(vector? seq) (vector->series name
                                   seq
                                   #:projection projection
                                   #:properties properties)]
    [else (sequence->series name
                            seq
                            #:projection projection
                            #:properties properties)]))

(define (vector->series name
                        vec
                        #:size [size #f]
                        #:offset [offset #f]
                        #:projection [projection #f]
                        #:properties [properties (hash)])
  (define series-offset (or offset 0))
  (define vlen (vector-length vec))
  (define series-size   (or size (- vlen series-offset)))
  (cond
    [(> (+ series-offset series-size) vlen)
     (raise-uke-error
      exn:uke:series
      'vector->series
      "series size ~a at offset ~a is out of bounds for vector length ~a"
      series-size series-offset vlen)]
    [(immutable? vec)
     (make-series name
                  (make-linear-index series-size series-offset)
                  vec
                  #:projection projection
                  #:properties properties)]
    [else
     (define i series-offset)
     (define j (+ series-offset series-size))
     (make-series name
                  (make-linear-index series-size 0 1)
                  (unsafe-vector*->immutable-vector!
                   (vector-copy vec i j))
                  #:projection projection
                  #:properties properties)]))

(define (sequence->series name
                          seq
                          #:projection [projection #f]
                          #:properties [properties (hash)])
  (define-values (store len)
    (sequence->list/length seq))
  (make-series name
               (make-linear-index len)
               (list->immutable-vector store)
               #:projection projection
               #:properties properties))

(define (series-push-index s idx)
  (struct-copy series s [index (index-compose (series-index s) idx)]))

(define (series-compact? a-series)
  (and (not (series-projection a-series))
       (let ([idx (series-index a-series)])
         (and (index-compact? idx)
              (= (store-length (series-store a-series))
                 (index-size idx))))))

(define (series-compact a-series)
  (cond
    [(series-compact? a-series) a-series]
    [else
     (build-series (series-name a-series)
                   (series-size a-series)
                   (λ (i) (series-ref a-series i))
                   #:properties (series-properties a-series))]))

(define (series-slice a-series start [size (- (series-size a-series) start)])
  (series-index-update a-series (λ (idx) (index-slice idx start size))))

(define (series-render-cell a-series v)
  (define fmt (series-property-ref a-series '#:render ~a))
  (fmt v))

(define (build-series name
                      size
                      f
                      #:projection [projection #f]
                      #:properties [properties (hash)])
  (make-series name
               (make-linear-index size 0 1)
               (unsafe-vector*->immutable-vector!
                (build-vector size f))
               #:projection projection
               #:properties properties))
