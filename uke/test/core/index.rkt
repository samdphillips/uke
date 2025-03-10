#lang racket/base

(require racket/sequence
         rackunit
         uke/index
         (submod uke/private/index for-test))

(test-case "linear identity index"
  (define idx (make-linear-index 10))
  (check-equal? (index-size idx) 10)
  (check-equal? (sequence->list (in-indices idx))
                '(0 1 2 3 4 5 6 7 8 9))
  (check-equal? (index-ref idx 0) 0)
  (check-equal? (index-ref idx 9) 9))

(test-case "linear-index oob lookup"
  (define idx (make-linear-index 10))
  (check-exn exn:uke:index? (lambda () (index-ref idx -1)))
  (check-exn exn:uke:index? (lambda () (index-ref idx 10)))
  (check-exn exn:uke:index? (lambda () (index-ref idx 100))))

(test-case "linear/linear same index-compose"
  (define i (make-linear-index 10))
  (define j (index-compose i i))
  (check-equal? (index-size j) 10))

(test-case "linear/linear smaller index-compose"
  (define i (make-linear-index 10))
  (define j (make-linear-index 20))
  (define k (index-compose j i))
  (check-equal? (index-size k) 10)
  (check-exn exn:uke:index? (lambda () (index-compose i j))))

(test-case "linear/linear index-compose"
  (define i (make-linear-index 10 0 2))
  (define j (make-linear-index 10))
  (define k (index-compose i j))
  (check-match k (linear-index 10 0 2)))

(test-case "linear-index max-range"
  (check-equal? (index-max-range (make-linear-index 10)) 10)
  (check-equal? (index-max-range (make-linear-index 9 1)) 10)
  (check-equal? (index-max-range (make-linear-index 5 0 2)) 10)
  (check-equal? (index-max-range (make-linear-index 3 4 2)) 10)
  (check-equal? (index-max-range (make-linear-index 0)) -1)
  (check-equal? (index-max-range (make-linear-index 0 1 1)) -1))

(test-case "linear/linear incompatible compose"
  (check-exn exn:uke:index?
             (λ () (index-compose (make-linear-index 10)
                                  (make-linear-index 10 0 2))))
  (check-exn exn:uke:index?
             (λ () (index-compose (make-linear-index 10)
                                  (make-linear-index 10 1 1)))))

(test-case "vector-index empty"
  (check-equal? (index-size (make-vector-index #())) 0)
  (check-equal? (index-max-range (make-vector-index #())) -1))

(test-case "vector-index"
  (define idx (make-vector-index (vector-immutable 3 2 1 0)))
  (check-equal? (sequence->list (in-indices idx)) '(0 1 2 3))
  (check-equal? (index-size idx) 4)
  (check-equal? (index-ref idx 0) 3)
  (check-equal? (index-ref idx 1) 2)
  (check-equal? (index-ref idx 2) 1)
  (check-equal? (index-ref idx 3) 0)
  (check-equal? (index-max-range idx) 3)
  (check-exn exn:uke:index? (lambda () (index-ref idx -1)))
  (check-exn exn:uke:index? (lambda () (index-ref idx 4))))

(test-case "vector/vector index compose"
  (define i (make-vector-index (vector-immutable 3 2 1 0)))
  (define j (make-vector-index (vector-immutable 0 1 2 3)))

  (define ii (index-compose i i))
  (check-true (vector-index? ii))
  (check-equal? (index-max-range ii) 3)

  (define jj (index-compose j j))
  (check-true (vector-index? jj))
  (check-equal? (index-max-range jj) 3)

  (check-not-eq? ii jj)

  (check-equal? ii j)
  (check-equal? jj j))

(test-case "linear/vector index compose"
  (define i (make-linear-index 3))
  (define j (make-vector-index (vector 2 1 0)))

  (define ij (index-compose i j))
  (check-true (vector-index? ij))
  (check-equal? (index-size ij) 3)
  (check-equal? (index-max-range ij) 2)
  (check-equal? (for/list ([t (in-indices ij)])
                  (index-ref ij t))
                '(2 1 0))

  (define ji (index-compose j i))
  (check-equal? (index-size ji) 3)
  (check-equal? (index-max-range ji) 2)
  (check-equal? (for/list ([t (in-indices ji)])
                  (index-ref ji t))
                '(2 1 0)))

(test-case "linear/vector smaller index compose"
  (define i (make-linear-index 3))
  (define j (make-vector-index (vector 3 2 1 0)))
  (define ji (index-compose j i))
  (check-true (vector-index? ji))
  (check-equal? (index-size ji) 3)
  (check-equal? (index-max-range ji) 3)
  (check-equal? (for/list ([t (in-indices ji)])
                  (index-ref ji t))
                '(3 2 1))
  (check-exn exn:uke:index? (lambda () (index-compose i j))))

(test-case "index-select"
  (define i0 (make-linear-index 100))
  (define i1 (index-select i0 (λ (i) (<= 10 i 19))))
  (check-equal? (index-size i1) 10)
  (define i2 (index-select i1 even?))
  (check-equal? (index-size i2) 5)
  (check-equal? (for/list ([i (in-indices i2)])
                  (index-ref i2 i))
                '(10 12 14 16 18)))

(test-case "index-select - reversed"
  (define i0 (index-compose (make-linear-index 100)
                            (make-linear-index 100 99 -1)))
  (define i1 (index-select i0 (λ (i) (<= 10 i 14))))
  (check-equal? (index-ref i0 0) 99)
  (check-equal? (index-size i1) 5)
  (check-equal? (for/list ([i (in-indices i1)])
                  (index-ref i1 i))
                '(89 88 87 86 85)))

(test-case "index-slice linear"
  (define i (make-linear-index 100))
  (define s0 (index-slice i 0 10))
  (check-true (linear-index? s0))
  (check-equal? (index-size s0) 10)
  (check-equal? (index-ref s0 0) 0)
  (check-equal? (index-ref s0 9) 9)

  (define s1 (index-slice i 90 10))
  (check-true (linear-index? s1))
  (check-equal? (index-size s1) 10)
  (check-equal? (index-ref s1 0) 90)
  (check-equal? (index-ref s1 9) 99)

  (define s2 (index-slice i 90))
  (check-true (linear-index? s2))
  (check-equal? (index-size s2) 10)
  (check-equal? (index-ref s2 0) 90)
  (check-equal? (index-ref s2 9) 99)

  (define s3 (index-slice i 10 10))
  (check-true (linear-index? s3))
  (check-equal? (index-size s3) 10)
  (check-equal? (index-ref s3 0) 10)
  (check-equal? (index-ref s3 9) 19))

(test-case "index-slice vector"
  (define i (index-select (make-linear-index 100) even?))
  (define s0 (index-slice i 0 10))
  (check-true (vector-index? s0))
  (check-equal? (index-size s0) 10)
  (check-equal? (index-ref s0 0) 0)
  (check-equal? (index-ref s0 9) 18)

  (define s1 (index-slice i 40 10))
  (check-true (vector-index? s1))
  (check-equal? (index-size s1) 10)
  (check-equal? (index-ref s1 0) 80)
  (check-equal? (index-ref s1 9) 98)

  (define s2 (index-slice i 40))
  (check-true (vector-index? s2))
  (check-equal? (index-size s2) 10)
  (check-equal? (index-ref s2 0) 80)
  (check-equal? (index-ref s2 9) 98)

  (define s3 (index-slice i 10 10))
  (check-true (vector-index? s3))
  (check-equal? (index-size s3) 10)
  (check-equal? (index-ref s3 0) 20)
  (check-equal? (index-ref s3 9) 38))

(test-case "index-pick"
  (define i0 (make-linear-index 100))
  (define i1 (index-pick i0 '(0 5 25 55 75)))
  (check-equal? (index-size i1) 5)
  (check-equal? (for/list ([i (in-indices i1)]) (index-ref i1 i))
                '(0 5 25 55 75)))