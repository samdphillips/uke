#lang racket/base

(require racket/sequence
         rackunit
         uke/dataframe
         uke/machete
         "../b2t2-tables.rkt")

(test-case "group - students - by column"
  (define students2
    (~> (students)
        (create
         [is-teenager (age) (and (< 12 _) (< _ 20))])
        (group (is-teenager)
               #:aggregate
               (~> (dataframe-column-ref 'name)
                   sequence->list))))

  (check-match
   (let ([kref (λ (i) (dataframe-cell-ref students2 'key i))]
         [gref (λ (i) (dataframe-cell-ref students2 'groups i))])
     (for/list ([i (in-range (dataframe-num-rows students2))])
       (list (kref i) (gref i))))
   (list-no-order
    (list '(#t) '("Alice" "Eve"))
    (list '(#f) '("Bob")))))

(test-case "group - students - derive key"
  (define students2
    (~> (students)
        (group #:key
               (~> (dataframe-cell-ref _ 'age _)
                   (if (and (< 12 _) (< _ 20))
                       'teenager
                       'not-teenager))
               #:aggregate
               (~> (dataframe-column-ref 'name)
                   sequence->list))))

  (check-match
   (let ([kref (λ (i) (dataframe-cell-ref students2 'key i))]
         [gref (λ (i) (dataframe-cell-ref students2 'groups i))])
     (for/list ([i (in-range (dataframe-num-rows students2))])
       (list (kref i) (gref i))))
   (list-no-order
    (list 'teenager '("Alice" "Eve"))
    (list 'not-teenager '("Bob")))))

(test-case "group - students - derive key from column vals"
  (define students2
    (~> (students)
        (group (age)
               #:key
               (if (and (< 12 _) (< _ 20))
                   'teenager
                   'not-teenager)
               #:aggregate
               (~> (dataframe-column-ref 'name)
                   sequence->list))))

  (check-match
   (let ([kref (λ (i) (dataframe-cell-ref students2 'key i))]
         [gref (λ (i) (dataframe-cell-ref students2 'groups i))])
     (for/list ([i (in-range (dataframe-num-rows students2))])
       (list (kref i) (gref i))))
   (list-no-order
    (list 'teenager '("Alice" "Eve"))
    (list 'not-teenager '("Bob")))))