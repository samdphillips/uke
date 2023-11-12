#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/sequence
         racket/unsafe/ops
         racket/vector
         "error.rkt")

(provide prop:index
         index?
         index-ref
         index-size
         index-compose
         index-max-range
         index-compact?
         in-indices

         index-sort

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
;; XXX eventually need a property guard if this is exported
(define-values (prop:index index? index-ops)
  (make-struct-type-property 'index))

(define (get-index-op idx slot)
  (vector-ref (index-ops idx) slot))

(define (apply-index-op idx slot . args)
  (apply (get-index-op idx slot) idx args))

(define (check-index-bounds who idx i)
  (unless (and (<= 0 i) (< i (index-size idx)))
    (raise-uke-error exn:uke:index who "index out of range for index: ~a" i)))

(define (check-index-compatible who i0 i1)
  (unless (<= (index-max-range i0) (index-size i1))
    (raise-uke-error exn:uke:index
                     who
                     "indexes incompatible: range ~a > size ~a"
                     (index-max-range i0)
                     (index-size i1))))

(define (index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (apply-index-op idx 0 i))

(define (index-size idx)
  (apply-index-op idx 1))

;; XXX if i1 == (linear-index sz 0 1) and (index-size i0) == sz return i0
;; XXX if i1 == (linear-index sz 0 1) and (vector-index? i0) and (index-size i0) != sz
;;     only copy relevant part of index instead of recalculating the whole table.
(define (index-compose i0 i1)
  (check-index-compatible 'index-compose i1 i0)
  (cond
    [(get-index-op i0 2) => (λ (f) (f i0 i1))]
    [else (generic-index-compose i0 i1)]))

(define (index-max-range idx)
  (if (zero? (index-size idx))
      -1
      (apply-index-op idx 3)))

(define (index-compact? idx)
  (and (linear-index? idx)
       (zero? (linear-index-offset idx))
       (let ([s (linear-index-stride idx)])
         (or (= 1 s) (= 0 s)))))

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

(define (index-sort idx lt?)
  (make-vector-index
   (list->vector
    (sort (sequence->list (in-indices-sequence idx)) lt?))))

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

(define (linear-index-max-range idx)
  (+ (linear-index-offset idx)
     (* (linear-index-stride idx) (linear-index-size idx))))

(struct linear-index (size offset stride)
  #:transparent
  #:property prop:index
  (vector linear-index-ref
          (λ (idx) (linear-index-size idx))
          linear-index-compose
          linear-index-max-range))

(define (make-linear-index size [offset 0] [stride 1])
  (linear-index size offset stride))

(define (vector-index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (vector-ref (vector-index-mapping idx) i))

(define (vector-index-size idx)
  (vector-length (vector-index-mapping idx)))

(define (-vector-index-max-range idx)
  (vector-index-max-range idx))

(struct vector-index (max-range mapping)
  #:transparent
  #:property prop:index
  (vector vector-index-ref
          vector-index-size
          #f
          -vector-index-max-range))

(define (make-vector-index vec)
  (vector-index (if (zero? (vector-length vec))
                    -1
                    (vector-argmax values vec))
                (vector->immutable-vector vec)))
