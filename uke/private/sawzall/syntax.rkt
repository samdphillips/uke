#lang racket/base

(require (for-template racket/base)
         racket/syntax
         syntax/parse)

(provide create-column-spec)

(define-syntax-class create-column-spec
  (pattern [name:id (binder:id ...) body ...+]
           #:attr (series-id 1) (generate-temporaries #'(binder ...))
           #:attr vec-id  (format-id #'name "~a-store" #'name)
           #:attr func-id (format-id #'name "~a-proc" #'name)
           #:attr func    #'(lambda (binder ...) body ...)))

