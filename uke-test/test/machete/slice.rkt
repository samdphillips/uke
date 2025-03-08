#lang racket/base

(require racket/sequence
         rackunit
         uke/dataframe
         uke/machete
         "../b2t2-tables.rkt")

(test-case "slice - students"
  (define students2
    (~> (students)
        (slice (or name age))))
  (define students3
    (~> (students)
        (slice (not color))))
  (check-false (dataframe-column*-ref students2 'color #f))
  (check-false (dataframe-column*-ref students3 'color #f))
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'name))
                (sequence->list
                 (dataframe-column-ref students3 'name)))
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'age))
                (sequence->list
                 (dataframe-column-ref students3 'age))))

(test-case "slice - students - added column"
  (define students2
    (~> (students)
        (create
         [is-teenager (age) (and (< 12 _) (< _ 20))])
        (slice (or name is-teenager))))
  (check-false (dataframe-column*-ref students2 'color #f))
  (check-false (dataframe-column*-ref students2 'age #f))
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'name))
                '("Bob" "Alice" "Eve"))
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'is-teenager))
                '(#f #t #t)))


(test-case "slice - students - added rearranged column"
  (define students2
    (~> (students)
        dataframe-reverse-rows
        (create
         [is-teenager (age) (and (< 12 _) (< _ 20))])
        (slice (or is-teenager name))))
  (check-false (dataframe-column*-ref students2 'color #f))
  (check-false (dataframe-column*-ref students2 'age #f))
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'name))
                '("Eve" "Alice" "Bob"))
  (check-equal? (sequence->list
                 (dataframe-column-ref students2 'is-teenager))
                '(#t #t #f)))
