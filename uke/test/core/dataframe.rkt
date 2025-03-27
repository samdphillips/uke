#lang racket/base

(require racket/sequence
         rackunit
         uke/column
         uke/dataframe
         uke/error
         uke/index
         uke/machete
         "../b2t2-tables.rkt")

(test-case "dataframe-reverse-rows linear-index"
  (define df (for/dataframe (a) ([i 10]) i))
  (check-equal? (sequence->list
                 (dataframe-column-ref (dataframe-reverse-rows df) 'a))
                '(9 8 7 6 5 4 3 2 1 0)))

(test-case "dataframe-reverse-rows vector-index"
  (define df0 (for/dataframe (a) ([i 10]) i))
  (define df1 (dataframe-select df0 even?))
  (define df2 (dataframe-reverse-rows df1))
  (check-equal? (sequence->list
                 (dataframe-column-ref df2 'a))
                '(8 6 4 2 0)))

(test-case "dataframe-compact?"
  (define df0 (for/dataframe (a) ([i 10]) i))
  (define df1 (dataframe-select df0 even?))
  (define df2 (dataframe-reverse-rows df1))
  (define df3 (dataframe-compact df2))
  (define df4
    (make-dataframe
     #:index (make-linear-index 10)
     (list (vector->column 'a (build-vector 10 values)))))

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
  (check-equal? (sequence->list (dataframe-column-ref df3 'a))
                (sequence->list (dataframe-column-ref df2 'a))))

(test-case "dataframe-column-ref"
  (define df0 (for/dataframe (a b) ([i 10]) (values i (* 2 i))))
  (check-equal? (sequence->list (dataframe-column-ref df0 'a))
                '(0 1 2 3 4 5 6 7 8 9))
  (check-equal? (dataframe-column-ref df0 'c 'missing)
                'missing)
  (check-exn
   exn:uke:dataframe?
   (lambda ()
     (dataframe-column-ref df0 'c))))

(test-case "dataframe-slice"
  (define df0 (for/dataframe (a b) ([i 100]) (values i (* 2 i))))
  (define df1 (dataframe-slice df0 90 10))
  (define df2 (dataframe-slice df0 90))
  (check-equal? (sequence->list (dataframe-column-ref df1 'a))
                '(90 91 92 93 94 95 96 97 98 99))
  (check-equal? (sequence->list (dataframe-column-ref df1 'b))
                '(180 182 184 186 188 190 192 194 196 198))
  (check-equal? (sequence->list (dataframe-column-ref df2 'a))
                '(90 91 92 93 94 95 96 97 98 99))
  (check-equal? (sequence->list (dataframe-column-ref df2 'b))
                '(180 182 184 186 188 190 192 194 196 198)))

(test-case "dataframe-select"
  (define students2
    (dataframe-select students
                      (dataframe-column-lift students
                                             (λ (age)
                                               (and (< 12 age) (< age 20)))
                                             'age)))
  (check-equal? (sequence->list (dataframe-column-ref students2 'name))
                '("Alice" "Eve")))

(test-case "dataframe-select - reversed"
  (define students2
    (let ([df (dataframe-reverse-rows students)])
      (dataframe-select df
                        (dataframe-column-lift
                         df (λ (age) (and (< 12 age) (< age 20)))
                         'age))))
  (check-equal? (sequence->list (dataframe-column-ref students2 'name))
                '("Eve" "Alice")))

(test-case "dataframe-column-lift - linear"
  (define df0 (for/dataframe (a b) ([i 3]) (values i (* 2 i))))
  (define f (dataframe-column-lift df0 list 'a 'b))
  (define tbl
    (for/list ([i (in-indices (dataframe-index df0))]) (f i)))
  (check-equal? tbl '((0 0) (1 2) (2 4))))

(test-case "dataframe sorting"
  (define df0
    (for/dataframe (x y) ([v 10])
      (if (odd? v) (values v v) (values v #f))))

  (define df1 (~> (df0)
                  dataframe-compact
                  (where (y) _)
                  dataframe-reverse-rows))

  (define df2
    (let* ([f (dataframe-column-lift df1 values 'y)]
           [lt? (λ (i j) (< (f i) (f j)))])
      (dataframe-index-update df1
                              (λ (idx)
                                (index-sort idx lt?)))))
  (check-equal? (sequence->list (dataframe-column-ref df1 'x))
                '(9 7 5 3 1))
  (check-equal? (sequence->list (dataframe-column-ref df2 'x))
                '(1 3 5 7 9)))

(test-case "dataframe-column-lift - linear reversed"
  (define df0 (for/dataframe (a b) ([i 3]) (values i (* 2 i))))
  (define df1 (dataframe-reverse-rows df0))
  (define f (dataframe-column-lift df1 list 'a 'b))
  (define tbl
    (for/list ([i (in-indices (dataframe-index df1))]) (f i)))
  (check-equal? tbl '((2 4) (1 2) (0 0))))

(test-case "dataframe-group-index"
  (define groups
    (dataframe-group-index students
                           (dataframe-column-lift students values 'name)))
  (check-equal? groups (hash "Bob" (list 0)
                             "Alice" (list 1)
                             "Eve" (list 2))))

(test-case "dataframe-left-join"
  (define df-joined
    (dataframe-left-join
     students (dataframe-column-lift students list 'name 'age) null
     gradebook (dataframe-column-lift gradebook list 'name 'age) '(name age)))
  ;; XXX: better checks
  (check-equal? (dataframe-num-rows df-joined) 3)
  (check-equal? (length (dataframe-column* df-joined)) 9))

(test-case "dataframe-reorder-column"
  (check-equal? (for/list ([s (in-list (dataframe-columns students))])
                  (column-name s))
                '(name age color))
  (define s2 (dataframe-reorder-column students '(age name color)))
  (check-equal? (for/list ([s (in-list (dataframe-columns s2))])
                  (column-name s))
                '(age name color))

  (for ([s (in-list (dataframe-columns students))])
    (define n (column-name s))
    (check-true (column=? s (dataframe-column-ref s2 n))
                (format "~a column not the same" n)))

  ; dataframe-reorder-column can also drop (maybe this is not good?)
  (define s3 (dataframe-reorder-column students '(age name)))
  (check-equal? (for/list ([s (in-list (dataframe-columns s3))])
                  (column-name s))
                '(age name))

  (for ([s (in-list (dataframe-columns s3))])
    (define n (column-name s))
    (check-true (column=? s (dataframe-column-ref students n))
                (format "~a column not the same" n))))
