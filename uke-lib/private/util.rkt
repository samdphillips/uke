#lang racket/base

(provide list->immutable-vector
         sequence->list/length
         vector-valid-position?)

(define (list->immutable-vector vs)
  (apply vector-immutable vs))

(define (sequence->list/length seq)
  (define-values (more? next) (sequence-generate seq))
  (define (loop)
    (cond
      [(more?)
       (define v (next))
       (define-values (vs count) (loop))
       (values (cons v vs) (add1 count))]
      [else
       (values null 0)]))
  (loop))

(define (vector-valid-position? vec pos)
  (and (<= 0 pos) (< pos (vector-length vec))))
