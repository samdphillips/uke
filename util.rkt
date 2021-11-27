#lang racket/base

(provide immutable-vector?
         immutable-hash?
         list->immutable-vector
         sequence->list/length)

(define ((make-immutable-pred pred?) v)
  (and (pred? v) (immutable? v)))

(define immutable-vector? (make-immutable-pred vector?))
(define immutable-hash?   (make-immutable-pred hash?))

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

