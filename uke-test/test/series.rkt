#lang racket/base

(require racket/sequence
         rackunit
         uke/index
         (submod uke/index for-test)
         uke/series)

;; 000
(test-case "vector->series mutable w/ defaults"
  (define v (build-vector 10 values))
  (define s (vector->series 'a v))
  (check-match (series-index s) (linear-index 10 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9)))

;; 001
(test-case "vector->series immutable w/ defaults"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->series 'a v))
  (check-match (series-index s) (linear-index 10 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9)))

;; 010
(test-case "vector->series mutable w/ size + default offset"
  (define v (build-vector 10 values))
  (define s (vector->series 'a v #:size 5))
  (check-match (series-index s) (linear-index 5 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(0 1 2 3 4)))

;; 011
(test-case "vector->series immutable w/ size + default offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->series 'a v #:size 5))
  (check-match (series-index s) (linear-index 5 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4)))


;; 100
(test-case "vector->series mutable w/ default size + offset"
  (define v (build-vector 10 values))
  (define s (vector->series 'a v #:offset 2))
  (check-match (series-index s) (linear-index 8 0 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6 7 8 9))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(2 3 4 5 6 7 8 9)))

;; 101
(test-case "vector->series immutable w/ default size + offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->series 'a v #:offset 2))
  (check-match (series-index s) (linear-index 8 2 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6 7 8 9)))


;; 110
(test-case "vector->series mutable w/ size + offset"
  (define v (build-vector 10 values))
  (define s (vector->series 'a v #:size 5 #:offset 2))
  (check-match (series-index s) (linear-index 5 0 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(2 3 4 5 6)))

;; 111
(test-case "vector->series immutable w/ size + offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->series 'a v #:size 5 #:offset 2))
  (check-match (series-index s) (linear-index 5 2 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6)))

;; size out of bounds with default offset
;; size out of bounds with offset