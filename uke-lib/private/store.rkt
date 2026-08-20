#lang racket/base

(require racket/mutability
         racket/vector
         racket/unsafe/ops)

(provide store?
         store-length
         store-ref
         store-copy
         store-append)

(define store? immutable-vector?)
(define store-length vector-length)
(define store-ref vector-ref)

(define (store-copy st [i 0] [j (store-length st)])
  (unsafe-vector*->immutable-vector! (vector-copy st i j)))

(define (store-append st0 st1)
  (unsafe-vector*->immutable-vector!
    (vector-append st0 st1)))
