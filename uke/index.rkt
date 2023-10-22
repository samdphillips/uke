#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/unsafe/ops
         "error.rkt")

(provide prop:index
         index?
         index-ref
         index-size
         index-compose
         in-indices

         exn:uke:index?

         vector-index?
         linear-index?

         make-vector-index

         make-linear-index
         linear-index-offset
         linear-index-stride)

(module* for-test #f
  (provide (struct-out linear-index)))

;; index-ref
;; index-size
;; index-compose
;; index-compose!
;; XXX probably needs property guard
(define-values (prop:index index? index-ops)
  (make-struct-type-property 'index))

(define (get-index-op idx slot)
  (vector-ref (index-ops idx) slot))

(define (apply-index-op idx slot . args)
  (apply (get-index-op idx slot) idx args))

(define (check-index-bounds who idx i)
  (unless (and (<= 0 i) (< i (index-size idx)))
    (raise-uke-error exn:uke:index who "index out of range for index: ~a" i)))

(define (check-index-sizes-compatible who i0 i1)
  (unless (<= (index-size i0) (index-size i1))
    (raise-uke-error exn:uke:index
                     who
                     "index sizes incompatible: ~a > ~a"
                     (index-size i0)
                     (index-size i1))))

(define (index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (apply-index-op idx 0 i))

(define (index-size idx)
  (apply-index-op idx 1))

(define (index-compose i0 i1)
  (check-index-sizes-compatible 'index-compose i1 i0)
  (cond
    [(get-index-op i0 2) => (λ (f) (f i0 i1))]
    [else (generic-index-compose i0 i1)]))

;; XXX in-indices should probably work on indexes, series, and dataframes.
(define in-indices-sequence
  (procedure-rename
   (λ (idx)
     (in-range (index-size idx)))
   'in-indices))

(define-sequence-syntax in-indices
  (λ () #'in-indices-sequence)
  (syntax-parser
    [(v:id (in-indices an-index)) #'(v (in-range (index-size an-index)))]
    [_ #f]))

(define (do-generic-index-compose i0 i1)
  (for/vector #:length (index-size i1) ([i (in-indices i1)])
    (index-ref i0 (index-ref i1 i))))

(define (generic-index-compose i0 i1)
  ;; do-generic-index-compose must return a fresh vector
  (make-vector-index
   (unsafe-vector*->immutable-vector!
     (do-generic-index-compose i0 i1))))

(define (linear-index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (+ (linear-index-offset idx)
     (* (linear-index-stride idx) i)))

(define (do-linear-index-compose i0 i1)
  (define o0 (linear-index-offset i0))
  (define s0 (linear-index-stride i0))
  (define o1 (linear-index-offset i1))
  (define s1 (linear-index-stride i1))
  (values (index-size i1)
          (+ o0 (* s0 o1))
          (* s0 s1)))

(define (linear-index-compose i0 i1)
  (cond
    [(not (linear-index? i1)) (generic-index-compose i0 i1)]
    [else
     (call-with-values
      (λ () (do-linear-index-compose i0 i1))
      make-linear-index)]))

(struct linear-index (size offset stride)
  #:transparent
  #:property prop:index
  (vector linear-index-ref
          (λ (idx) (linear-index-size idx))
          linear-index-compose))

(define (make-linear-index size [offset 0] [stride 1])
  (linear-index size offset stride))

(define (vector-index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (vector-ref (vector-index-mapping idx) i))

(define (vector-index-size idx)
  (vector-length (vector-index-mapping idx)))

(struct vector-index (mapping)
  #:transparent
  #:property prop:index
  (vector vector-index-ref
          vector-index-size
          #f))

(define (make-vector-index vec)
  (vector-index (vector->immutable-vector vec)))
