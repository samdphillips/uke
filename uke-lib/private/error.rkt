#lang racket/base

(provide (struct-out exn:uke)
         (struct-out exn:uke:dataframe)
         (struct-out exn:uke:index)
         (struct-out exn:uke:column)
         raise-uke-error)

(struct exn:uke exn:fail ())
(struct exn:uke:dataframe exn:uke ())
(struct exn:uke:index exn:uke ())
(struct exn:uke:column exn:uke ())

(define (raise-uke-error make-exn who message . args)
  (define final-message
    (format "~a: ~a" who (apply format message args)))
  (define exn
    (make-exn final-message (current-continuation-marks)))
  (raise exn))
