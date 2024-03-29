#lang racket/base

(require (for-template racket/base
                       qi)
         racket/syntax
         syntax/parse)

(provide create-column-spec
         slice-spec)

(define-syntax-class create-column-spec
  (pattern [name:id {~seq property:keyword property-expr} ...
                    (in-series-name:id ...) flo]
    #:attr (in-series-var 1)
    (generate-temporaries #'(in-series-name ...))
    #:attr vec-id  (format-id #'name "~a-store" #'name)
    #:attr flow-id (format-id #'name "~a-flow" #'name)
    #:attr flow    #'(flow flo)))

(define-syntax-class slice-spec
  #:literals (or and not)
  #:datum-literals (everything)
  #:attributes (pred?)
  [pattern everything
           #:attr pred? #'(lambda (n) #t)]
  [pattern s:string
           #:attr pred?
           #'(let ([i (string->symbol s)])
               (lambda (n) (eq? i n)))]
  [pattern r:regexp
           #:attr pred?
           #'(lambda (n)
               (regexp-match? r (symbol->string n)))]
  [pattern i:id
           #:attr pred?
           #'(lambda (n) (eq? 'i n))]
  [pattern (or s:slice-spec ...+)
           #:attr pred?
           #'(lambda (n)
               (ormap (lambda (p) (p n))
                      (list s.pred? ...)))]
  [pattern (and s:slice-spec ...+)
           #:attr pred?
           #'(lambda (n)
               (andmap (lambda (p) (p n))
                       (list s.pred? ...)))]
  [pattern (not s:slice-spec)
           #:attr pred?
           #'(lambda (n)
               (not (s.pred? n)))])
