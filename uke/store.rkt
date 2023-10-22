#lang racket/base

(require racket/vector
         "util.rkt")

(provide store?
         store-length
         store-ref
         store-copy)

(define store? vector?)
(define store-length vector-length)
(define store-ref vector-ref)
(define store-copy vector-copy)

