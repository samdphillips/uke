#lang racket/base

(require racket/sequence
         rackunit
         uke/column
         uke/error
         uke/index
         (submod uke/private/index for-test))

;; There are 2**3 combinations of arguments for `vector->column`.  The cases are enumerated
;; +----- default offset / offset
;; |+---- default size / size
;; vvv--- mutable/immutable vector
;; xxx

;; 000
(test-case "vector->column mutable w/ defaults"
  (define v (build-vector 10 values))
  (define s (vector->column 'a v))
  (check-match (column-index s) (linear-index 10 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9)))

;; 001
(test-case "vector->column immutable w/ defaults"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->column 'a v))
  (check-match (column-index s) (linear-index 10 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9)))

;; 010
(test-case "vector->column mutable w/ size + default offset"
  (define v (build-vector 10 values))
  (define s (vector->column 'a v #:size 5))
  (check-match (column-index s) (linear-index 5 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(0 1 2 3 4)))

;; 011
(test-case "vector->column immutable w/ size + default offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->column 'a v #:size 5))
  (check-match (column-index s) (linear-index 5 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4)))


;; 100
(test-case "vector->column mutable w/ default size + offset"
  (define v (build-vector 10 values))
  (define s (vector->column 'a v #:offset 2))
  (check-match (column-index s) (linear-index 8 0 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6 7 8 9))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(2 3 4 5 6 7 8 9)))

;; 101
(test-case "vector->column immutable w/ default size + offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->column 'a v #:offset 2))
  (check-match (column-index s) (linear-index 8 2 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6 7 8 9)))


;; 110
(test-case "vector->column mutable w/ size + offset"
  (define v (build-vector 10 values))
  (define s (vector->column 'a v #:size 5 #:offset 2))
  (check-match (column-index s) (linear-index 5 0 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6))
  (vector-set! v 0 10)
  (check-equal? (sequence->list s) '(2 3 4 5 6)))

;; 111
(test-case "vector->column immutable w/ size + offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->column 'a v #:size 5 #:offset 2))
  (check-match (column-index s) (linear-index 5 2 1))
  (check-equal? (sequence->list s) '(2 3 4 5 6)))

(test-case "vector->column size out of bounds with default offset"
  (define v (vector))
  (check-exn
   exn:uke:column?
   (λ () (vector->column 'a v #:size 20))))

(test-case "vector->column size out of bounds with offset"
  (define v (build-vector 10 values))
  (check-exn
   exn:uke:column?
   (λ () (vector->column 'a v #:size 10 #:offset 5))))

(test-case "build-column"
  (define s (build-column 'a 10 values))
  (check-match (column-index s) (linear-index 10 0 1))
  (check-equal? (sequence->list s) '(0 1 2 3 4 5 6 7 8 9)))

(test-case "column=?"
  (define s (vector->column 'a #(0 2 4 6 8)))
  (define t
    (column-index-update
     (build-column 'a 10 values)
     (λ (idx)
       (index-select idx even?))))
  (check-true (column=? s t))
  (check-false (column=? s (column-name-update t (λ (old) 'b)))
               "columns with different names are not the same")
  (check-false (column=? (build-column 'a 10 values)
                         (build-column 'a 20 values))
               "columns that are different sizes are not the same")
  (check-false (column=? (build-column 'a 10 values)
                         (build-column 'a 10 (λ (x) (* x 2))))))

(test-case "column*=?"
  (define s (vector->column 'a #(0 2 4 6 8)))
  (define t
    (column-index-update
     (build-column 'a 10 values)
     (λ (idx)
       (index-select idx even?))))
  (define i (make-linear-index 3))
  (define j (make-vector-index #(0 1 2)))
  (check-true (column*=? i s i s))
  (check-true (column*=? i s i t))
  (check-true (column*=? i s j t))
  (check-true (column*=? j s i t))

  (check-false (column*=? i s (make-linear-index 0) s)))

(test-case "column-compact - no change"
  (define s (build-column 'a 10 values #:properties (hash '#:has-prop? #t)))
  (check-true (column-compact? s))
  (check-eq? s (column-compact s))
  (check-true (column=? s (column-compact s)))
  (check-true (column-property-ref s '#:has-prop? #f)))

(test-case "column-compact - shrink"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->column 'a v
                            #:size 5
                            #:properties (hash '#:has-prop? #t)))
  (define t (column-compact s))
  (check-false (column-compact? s))
  (check-true (column-compact? t))
  (check-not-eq? s t)
  (check-true (column=? s t))
  (check-true (column-property-ref t '#:has-prop? #f)))

(test-case "column-compact - offset"
  (define v (vector->immutable-vector (build-vector 10 values)))
  (define s (vector->column 'a v #:offset 5
                            #:properties (hash '#:has-prop? #t)))
  (define t (column-compact s))

  (check-false (column-compact? s))
  (check-true (column-compact? t))
  (check-not-eq? s t)
  (check-true (column=? s t))
  (check-true (column-property-ref t '#:has-prop? #f)))

(test-case "column-compact - shared store"
  (define v (vector->immutable-vector (build-vector 20 values)))
  (define s
    (make-column 'a
                 (make-linear-index 10 0 2)
                 v
                 #:properties (hash '#:has-prop? #t)))
  (define t (column-compact s))
  (check-false (column-compact? s))
  (check-true (column-compact? t))
  (check-not-eq? s t)
  (check-true (column=? s t))
  (check-true (column-property-ref t '#:has-prop? #f)))

(test-case "column-compact - vector index"
  (define s
    (make-column 'a
                 (make-vector-index
                  (build-vector 10 values))
                 (vector->immutable-vector (build-vector 10 values))
                 #:properties (hash '#:has-prop? #t)))
  (define t (column-compact s))
  (check-false (column-compact? s))
  (check-true (column-compact? t))
  (check-not-eq? s t)

  (check-true (column=? s t))
  (check-true (column-property-ref t '#:has-prop? #f)))

(test-case "column-compact - vector index - out of order"
  (define s
    (make-column 'a
                 (make-vector-index
                  (build-vector 10 (λ (i) (- 9 i))))
                 (vector->immutable-vector (build-vector 10 values))
                 #:properties (hash '#:has-prop? #t)))
  (define t (column-compact s))
  (check-false (column-compact? s))
  (check-true (column-compact? t))
  (check-not-eq? s t)
  (check-true (column-property-ref t '#:has-prop? #f)))

(test-case "column-compact - projection"
  (define v
    (vector->immutable-vector (build-vector 10 (λ (i) (cons i (* 2 i))))))
  (define i (make-linear-index 10))
  (define s (make-column 's i v #:projection car))
  (define t (make-column 't i v #:projection cdr))

  (check-false (column-compact? s))
  (check-false (column-compact? t))

  (check-true (column=? s (column-compact s)))
  (check-true (column=? t (column-compact t)))
  (check-not-eq? s (column-compact s))
  (check-not-eq? t (column-compact t)))

(test-case "column-slice"
  (define s (build-column 'a 100 values))
  (check-equal? (sequence->list (column-slice s 0 10))
                '(0 1 2 3 4 5 6 7 8 9))
  (check-equal? (sequence->list (column-slice s 90 10))
                '(90 91 92 93 94 95 96 97 98 99))
  (check-equal? (sequence->list (column-slice s 90))
                '(90 91 92 93 94 95 96 97 98 99)))

(test-case "column-projection"
  (define s (build-column 'a
                          10
                          values
                          #:projection (λ (v) (* v 2))))

  (check-equal? (sequence->list s)
                '(0 2 4 6 8 10 12 14 16 18)))