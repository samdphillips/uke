#lang racket/base

(require uke/column
         uke/dataframe
         uke/index)

(provide gradebook
         students)

(define gradebook
  (row-df [name  age quiz1 quiz2 midterm quiz3 quiz4 final]
          "Bob"   12     8     9      77     7     9    87
          "Alice" 17     6     8      88     8     7    85
          "Eve"   13     7     9      84     8     8    77))

(define students
  (row-df [name  age color]
          "Bob"   12 "blue"
          "Alice" 17 "green"
          "Eve"   13 "red"))
