#lang racket/base

(require rackunit
         racket/sequence
         uke/dataframe
         uke/index
         uke/sawzall
         uke/test/b2t2-tables)


(test-case "create - add student column"
  (define students2
    (create students [is-teenager (age) (and (< 12 age) (< age 20))]))
  (check-equal? (list #f #t #t)
                (sequence->list (dataframe-series-ref students2 'is-teenager))))

(test-case "create - add rearranged student column"
  (define students2
    (create (dataframe-reverse-rows students) [is-teenager (age) (and (< 12 age) (< age 20))]))
  (check-equal? (list #t #t #f)
                (sequence->list (dataframe-series-ref students2 'is-teenager))))