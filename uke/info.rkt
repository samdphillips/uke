#lang info

(define name "uke")
(define collection "uke")
(define version "2025.4.22")
(define deps '("base" "uke-lib" "uke-extra-lib" "uke-machete" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pkg-authors '(samdphillips@gmail.com))
(define scribblings '(("scribblings/uke.scrbl" ())))
(define license 'Apache-2.0)