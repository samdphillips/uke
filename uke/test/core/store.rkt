#lang racket/base

(require rackunit
         uke/store)

(test-case "store? is also an immutable-vector?"
  (check-false (store? (vector 1 2 3 4)))
  (check-true (store? (vector-immutable 1 2 3 4))))

(test-case "store-append creates a store"
  (define s0 (vector-immutable 1 2))
  (define s1 (vector-immutable 3 4))
  (define s2 (store-append s0 s1))
  (check-pred store? s2))
