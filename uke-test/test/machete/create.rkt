#lang racket/base

(require rackunit
         racket/sequence
         uke/dataframe
         uke/index
         uke/machete
         uke/series
         uke/test/b2t2-tables)

(test-case "create - students - add column"
  (define students2
    (~> (students)
        (create
         [is-teenager (age) (and (< 12 _) (< _ 20))])))
  (check-equal? (sequence->list
                 (dataframe-series-ref students2 'is-teenager))
                (list #f #t #t)))

(test-case "create - students - add rearranged column"
  (define students2
    (~> (students)
        dataframe-reverse-rows
        (create
         [is-teenager (age) (and (< 12 _) (< _ 20))])))
  (check-equal? (sequence->list
                 (dataframe-series-ref students2 'is-teenager))
                (list #t #t #f)))

(test-case "create - students - add property"
  ;; #:type-hint is not a real property (yet)
  (define (~truthy v) (or (and v "true") "false"))
  (define students2
    (~> (students)
        (create
         [is-teenager #:render ~truthy
                      #:type-hint 'boolean
                      (age) (and (< 12 _) (< _ 20))])))
  (check-equal? (series-property-ref
                 (dataframe-series-ref students2 'is-teenager)
                 '#:type-hint)
                'boolean))
