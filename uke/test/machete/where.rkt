#lang racket/base

(require rackunit
         racket/sequence
         uke/dataframe
         uke/index
         uke/machete
         uke/test/b2t2-tables)

(test-case "where - students"
  (define students2
    (~> (students)
        (where (age) (and (< 12 _) (< _ 20)))))
  (check-equal? (dataframe-num-rows students2) 2)
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'name))
                '("Alice" "Eve")))

(test-case "where - students - rearranged"
  (define students2
    (~> (students)
        dataframe-reverse-rows
        (where (age) (and (< 12 _) (< _ 20)))))
  (check-equal? (dataframe-num-rows students2) 2)
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'name))
                '("Eve" "Alice")))
