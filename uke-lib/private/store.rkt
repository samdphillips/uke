#lang racket/base

(require racket/vector
         "util.rkt")

(provide store?
         store-length
         store-ref
         store-copy
         store-append)

(define store? vector?)
(define store-length vector-length)
(define store-ref vector-ref)
(define store-copy vector-copy)

(define (store-append st0 st1)
  (vector-append st0 st1))