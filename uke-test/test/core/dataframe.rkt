#lang racket/base

(require racket/sequence
         rackunit
         uke/dataframe
         uke/index
         uke/series
         "../b2t2-tables.rkt")

(test-case "dataframe-reverse-rows linear-index"
  (define df (for/dataframe (a) ([i 10]) i))
  (check-equal? (sequence->list
                 (dataframe-series-ref (dataframe-reverse-rows df) 'a))
                '(9 8 7 6 5 4 3 2 1 0)))

(test-case "dataframe-reverse-rows vector-index"
  (define df0 (for/dataframe (a) ([i 10]) i))
  (define df1 (dataframe-select df0 even?))
  (define df2 (dataframe-reverse-rows df1))
  (check-equal? (sequence->list
                 (dataframe-series-ref df2 'a))
                '(8 6 4 2 0)))

(test-case "dataframe-compact?"
  (define df0 (for/dataframe (a) ([i 10]) i))
  (define df1 (dataframe-select df0 even?))
  (define df2 (dataframe-reverse-rows df1))
  (define df3 (dataframe-compact df2))
  (define df4
    (make-dataframe
     (list (vector->series 'a (build-vector 10 values)))))

  (check-false (dataframe-compact? df0))
  (check-false (dataframe-compact? df1))
  (check-false (dataframe-compact? df2))
  (check-true (dataframe-compact? df3))
  (check-true (dataframe-compact? df4)))

(test-case "dataframe-compact"
  (define df0 (for/dataframe (a) ([i 10]) i))
  (define df1 (dataframe-select df0 even?))
  (define df2 (dataframe-reverse-rows df1))
  (define df3 (dataframe-compact df2))
  (check-equal? (sequence->list (dataframe-series-ref df3 'a))
                (sequence->list (dataframe-series-ref df2 'a))))

(test-case "dataframe-slice"
  (define df0 (for/dataframe (a b) ([i 100]) (values i (* 2 i))))
  (define df1 (dataframe-slice df0 90 10))
  (define df2 (dataframe-slice df0 90))
  (check-equal? (sequence->list (dataframe-series-ref df1 'a))
                '(90 91 92 93 94 95 96 97 98 99))
  (check-equal? (sequence->list (dataframe-series-ref df1 'b))
                '(180 182 184 186 188 190 192 194 196 198))
  (check-equal? (sequence->list (dataframe-series-ref df2 'a))
                '(90 91 92 93 94 95 96 97 98 99))
  (check-equal? (sequence->list (dataframe-series-ref df2 'b))
                '(180 182 184 186 188 190 192 194 196 198)))

(test-case "dataframe-select"
  (define students2
    (dataframe-select students
                      (dataframe-series-lift students
                                             '(age)
                                             (λ (age)
                                               (and (< 12 age) (< age 20))))))
  (check-equal? (sequence->list (dataframe-series-ref students2 'name))
                '("Alice" "Eve")))

(test-case "dataframe-select - reversed"
  (define students2
    (let ([df (dataframe-reverse-rows students)])
      (dataframe-select df
                        (dataframe-series-lift
                         df '(age) (λ (age)
                                     (and (< 12 age) (< age 20)))))))
  (check-equal? (sequence->list (dataframe-series-ref students2 'name))
                '("Eve" "Alice")))

(test-case "dataframe-series-lift - linear - macro"
  (define df0 (for/dataframe (a b) ([i 3]) (values i (* 2 i))))
  (define f (dataframe-series-lift df0 '(a b) list))
  (define tbl
    (for/list ([i (in-indices (dataframe-index df0))]) (f i)))
  (check-equal? tbl '((0 0) (1 2) (2 4))))

(test-case "dataframe-series-lift - linear reversed - macro"
  (define df0 (for/dataframe (a b) ([i 3]) (values i (* 2 i))))
  (define df1 (dataframe-reverse-rows df0))
  (define f (dataframe-series-lift df1 '(a b) list))
  (define tbl
    (for/list ([i (in-indices (dataframe-index df1))]) (f i)))
  (check-equal? tbl '((2 4) (1 2) (0 0))))

(test-case "dataframe-series-lift - linear - procedural"
  (define df0 (for/dataframe (a b) ([i 3]) (values i (* 2 i))))
  (define cols '(a b))
  (define f (dataframe-series-lift df0 cols list))
  (define tbl
    (for/list ([i (in-indices (dataframe-index df0))]) (f i)))
  (check-equal? tbl '((0 0) (1 2) (2 4))))

(test-case "dataframe-series-lift - linear reversed  - procedural"
  (define df0 (for/dataframe (a b) ([i 3]) (values i (* 2 i))))
  (define df1 (dataframe-reverse-rows df0))
  (define cols '(a b))
  (define f (dataframe-series-lift df1 cols list))
  (define tbl
    (for/list ([i (in-indices (dataframe-index df1))]) (f i)))
  (check-equal? tbl '((2 4) (1 2) (0 0))))

(test-case "dataframe-group-index"
  (define groups
    (dataframe-group-index students
                           (dataframe-series-lift students '(name) values)))
  (check-equal? groups (hash "Bob" (list 0)
                             "Alice" (list 1)
                             "Eve" (list 2))))

(test-case "dataframe-left-join"
  (define df-joined
    (dataframe-left-join
     students (dataframe-series-lift students '(name age) list) null
     gradebook (dataframe-series-lift gradebook '(name age) list) '(name age)))
  ;; XXX: better checks
  (check-equal? (dataframe-num-rows df-joined) 3)
  (check-equal? (length (dataframe-series* df-joined)) 9))
