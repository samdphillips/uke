#lang racket/base

(require math/distributions
         racket/format
         racket/flonum
         (prefix-in c: "cities.rkt"))

(define cities-temps (list->vector c:cities-temps))
(define num-cities (vector-length cities-temps))

(define (once)
  (define i (random num-cities))
  (define rec (vector-ref cities-temps i))
  (define t (flvector-ref (flnormal-sample (cdr rec) 10.0 1) 0))
  (values (car rec) t))

(define (generate n)
  (for ([i n])
    (define-values (city temp) (once))
    (displayln (~a city ";" (~r #:precision '(= 1) temp)))))

(module* main #f
  (random-seed 12777)
  (generate 1000000))
