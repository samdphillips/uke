#lang racket/base

(require uke/dataframe
         uke/series)

(provide students)

(define students
  (let ([name  (vector->series 'name  #("Bob" "Alice" "Eve"))]
        [age   (vector->series 'age   #(12 17 13))]
        [color (vector->series 'color #("blue" "green" "red"))])
    (make-dataframe (list name age color))))

