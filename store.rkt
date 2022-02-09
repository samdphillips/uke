#lang racket/base

(require racket/generic
         "util.rkt")

(provide store?
         store-length
         store-ref
         store-copy)

(define-generics store
  ;; Store -> Nonnegative-Integer
  (store-length store)

  ;; Store Nonnegative-Integer -> Any
  (store-ref store i)

  ;; Store -> Store
  ;; Ideally a store is immutable, but some nice to use stores, like
  ;; fxvector and flvector, are not. For those cases we make a copy.
  (store-copy store)

  #:defaults
  ([immutable-vector?
    (define (store-length a-vector) (vector-length a-vector))
    (define (store-ref a-vector i)  (vector-ref a-vector i))
    (define (store-copy a-vector)   a-vector)]))

