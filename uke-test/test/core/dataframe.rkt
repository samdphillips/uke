#lang racket/base

(require racket/sequence
         rackunit
         uke/dataframe
         uke/series)

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