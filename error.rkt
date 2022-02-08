#lang racket/base

(provide (struct-out exn:uke)
         raise-uke-error)

(struct exn:uke exn:fail ())

(define (raise-uke-error make-exn name message . args)
  (define final-message
    (format "~a: ~a" name (apply format message args)))
  (define exn
    (make-exn final-message (current-continuation-marks)))
  (raise exn))

