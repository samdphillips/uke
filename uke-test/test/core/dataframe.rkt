#lang racket/base

(require racket/sequence
         rackunit
         uke/dataframe)

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