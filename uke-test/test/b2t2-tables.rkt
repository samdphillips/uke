#lang racket/base

(require uke/dataframe
         uke/index
         uke/series)

(provide gradebook
         students)

(define gradebook
  (let ([name    (vector->series 'name #("Bob" "Alice" "Eve"))]
        [age     (vector->series 'age #(12 17 13))]
        [quiz1   (vector->series 'quiz1 #(8 6 7))]
        [quiz2   (vector->series 'quiz2 #(9 8 9))]
        [midterm (vector->series 'midterm #(77 88 84))]
        [quiz3   (vector->series 'quiz3 #(7 8 8))]
        [quiz4   (vector->series 'quiz4 #(9 7 8))]
        [final   (vector->series 'final #(87 85 77))])
    (make-dataframe #:index (make-linear-index 3)
                    (list name age quiz1 quiz2 midterm quiz3 quiz4 final))))

(define students
  (let ([name  (vector->series 'name  #("Bob" "Alice" "Eve"))]
        [age   (vector->series 'age   #(12 17 13))]
        [color (vector->series 'color #("blue" "green" "red"))])
    (make-dataframe #:index (make-linear-index 3) (list name age color))))
