#lang racket/base

(require racket/sequence
         rackunit
         uke/error
         uke/index
         (submod uke/index for-test)
         uke/series)

;; There are 2**3 combinations of arguments for `vector->series`.  The cases are enumerated
;; +----- default offset / offset
;; |+---- default size / size
;; vvv--- mutable/immutable vector
;; xxx

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

(test-case "size out of bounds with default offset"
  (define v (vector))
  (check-exn
   exn:uke:series?
   (λ () (vector->series 'a v #:size 20))))

(test-case "size out of bounds with offset"
  (define v (build-vector 10 values))
  (check-exn
   exn:uke:series?
   (λ () (vector->series 'a v #:size 10 #:offset 5))))

(test-case "build-series"
  (define s (build-series 'a 10 values))
  (check-match (series-index s) (linear-index 10 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9)))

(test-case "series-compact - no change"
  (define s (build-series 'a 10 values))
  (check-true (series-compact? s))
  (check-eq? s (series-compact s)))

(test-case "series-compact - shrink"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->series 'a v #:size 5))
  (define t (series-compact s))
  (check-false (series-compact? s))
  (check-true (series-compact? t))
  (check-not-eq? s t)
  (check-equal? (sequence->list s)
                (sequence->list t)))

(test-case "series-compact - offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->series 'a v #:offset 5))
  (define t (series-compact s))

  (check-false (series-compact? s))
  (check-true (series-compact? t))
  (check-not-eq? s t)
  (check-equal? (sequence->list s)
                (sequence->list t)))

(test-case "series-compact - shared store"
  (define v (vector->immutable-vector (build-vector 20 values)))
  (define s
    (make-series 'a
                 (make-linear-index 10 0 2)
                 v))
  (define t (series-compact s))
  (check-false (series-compact? s))
  (check-true (series-compact? t))
  (check-not-eq? s t)
  (check-equal? (sequence->list s)
                (sequence->list t)))

(test-case "series-compact - vector index"
  (define s
    (make-series 'a
                 (make-vector-index
                  (build-vector 10 values))
                 (vector->immutable-vector (build-vector 10 values))))
  (define t (series-compact s))
  (check-false (series-compact? s))
  (check-true (series-compact? t))
  (check-not-eq? s t)
  (check-equal? (sequence->list s)
                (sequence->list t)))

(test-case "series-compact - vector index - out of order"
  (define s
    (make-series 'a
                 (make-vector-index
                  (build-vector 10 (λ (i) (- 9 i))))
                 (vector->immutable-vector (build-vector 10 values))))
  (define t (series-compact s))
  (check-false (series-compact? s))
  (check-true (series-compact? t))
  (check-not-eq? s t)
  (check-equal? (sequence->list s)
                (sequence->list t)))
