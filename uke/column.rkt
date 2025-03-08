#lang racket/base

(require racket/format
         racket/sequence
         racket/unsafe/ops
         racket/vector
         "error.rkt"
         "index.rkt"
         "store.rkt"
         "util.rkt")

(provide make-column
         column?
         column=?
         column*=?
         column-name
         column-size
         column-index
         column-properties
         column-projection
         column-store
         column-name-update
         column-index-update
         column-properties-update
         column-projection-update
         column-ref
         column-property-ref
         column-push-index
         column-compact?
         column-compact
         column-slice
         column-render-cell
         build-column
         ->column
         vector->column
         sequence->column)

(struct column (name properties index projection store)
  #:transparent
  #:property prop:sequence
  (λ (col) (sequence-map (λ (i) (column-ref col i))
                         (in-indices (column-index col)))))

(define (column-name-update col f)
  (struct-copy column col [name (f (column-name col))]))

(define (column-index-update col f)
  (struct-copy column col [index (f (column-index col))]))

(define (column-properties-update col f)
  (struct-copy column col [properties (f (column-properties col))]))

(define (column-projection-update col f)
  (struct-copy column col [projection (f (column-projection col))]))

(define (column-size col)
  (index-size (column-index col)))

(define (column=? s1 s2)
  (and (= (column-size s1) (column-size s2))
       (equal? (column-name s1) (column-name s2))
       (for/and ([i (in-indices (column-index s1))])
         (equal? (column-ref s1 i)
                 (column-ref s2 i)))))

(define (column*=? idx1 s1 idx2 s2)
  (define ((make-ref idx s) i)
    (column-ref s (index-ref idx i)))
  (define ref1 (make-ref idx1 s1))
  (define ref2 (make-ref idx2 s2))
  (and (= (index-size idx1) (index-size idx2))
       (equal? (column-name s1) (column-name s2))
       (for/and ([i (in-indices idx1)])
         (equal? (ref1 i) (ref2 i)))))

(define (make-column name
                     index
                     store
                     #:projection [projection #f]
                     #:properties [properties (hash)])
  (column name properties index projection store))

(define (apply-column-projection col v)
  (cond
    [(column-projection col) => (λ (p) (p v))]
    [else v]))

(define (column-ref col i)
  (define si (index-ref (column-index col) i))
  (define v (store-ref (column-store col) si))
  (apply-column-projection col v))

(define (column-property-ref col property [default #f])
  (hash-ref (column-properties col) property (λ () default)))

(define (->column name
                  seq
                  #:projection [projection #f]
                  #:properties [properties (hash)])
  (cond
    [(vector? seq) (vector->column name
                                   seq
                                   #:projection projection
                                   #:properties properties)]
    [else (sequence->column name
                            seq
                            #:projection projection
                            #:properties properties)]))

(define (vector->column name
                        vec
                        #:size [size #f]
                        #:offset [offset #f]
                        #:projection [projection #f]
                        #:properties [properties (hash)])
  (define col-offset (or offset 0))
  (define vlen (vector-length vec))
  (define col-size (or size (- vlen col-offset)))
  (cond
    [(> (+ col-offset col-size) vlen)
     (raise-uke-error
      exn:uke:column
      'vector->column
      "column size ~a at offset ~a is out of bounds for vector length ~a"
      col-size col-offset vlen)]
    [(immutable? vec)
     (make-column name
                  (make-linear-index col-size col-offset)
                  vec
                  #:projection projection
                  #:properties properties)]
    [else
     (define i col-offset)
     (define j (+ col-offset col-size))
     (make-column name
                  (make-linear-index col-size 0 1)
                  (unsafe-vector*->immutable-vector!
                   (vector-copy vec i j))
                  #:projection projection
                  #:properties properties)]))

(define (sequence->column name
                          seq
                          #:projection [projection #f]
                          #:properties [properties (hash)])
  (define-values (store len)
    (sequence->list/length seq))
  (make-column name
               (make-linear-index len)
               (list->immutable-vector store)
               #:projection projection
               #:properties properties))

(define (column-push-index s idx)
  (struct-copy column s [index (index-compose (column-index s) idx)]))

(define (column-compact? col)
  (and (not (column-projection col))
       (let ([idx (column-index col)])
         (and (index-compact? idx)
              (= (store-length (column-store col))
                 (index-size idx))))))

(define (column-compact col)
  (cond
    [(column-compact? col) col]
    [else
     (build-column (column-name col)
                   (column-size col)
                   (λ (i) (column-ref col i))
                   #:properties (column-properties col))]))

(define (column-slice col start [size (- (column-size col) start)])
  (column-index-update col (λ (idx) (index-slice idx start size))))

(define (column-render-cell col v)
  (define fmt (column-property-ref col '#:render ~a))
  (fmt v))

(define (build-column name
                      size
                      f
                      #:projection [projection #f]
                      #:properties [properties (hash)])
  (make-column name
               (make-linear-index size 0 1)
               (unsafe-vector*->immutable-vector!
                (build-vector size f))
               #:projection projection
               #:properties properties))
