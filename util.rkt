#lang racket/base

(provide immutable-vector?
         immutable-hash?)

(define ((make-immutable-pred pred?) v)
  (and (pred? v) (immutable? v)))

(define immutable-vector? (make-immutable-pred vector?))
(define immutable-hash?   (make-immutable-pred hash?))

