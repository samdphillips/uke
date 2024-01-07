#lang racket

(require (for-syntax syntax/parse)
         racket/require-syntax)

(define-require-syntax data-in
  (syntax-parser
    [(_ name:string)
     #:do [(define basename
             (cadr (regexp-match #px"^([^.]+)\\." (syntax-e #'name))))]
     #:with prefix (string->symbol (string-append basename ":"))
     #'(prefix-in prefix name)]
    [(_ req ...) #'(combine-in (data-in req) ...)]))

(require (data-in
          "africa.rkt"
          "asia.rkt"
          "europe.rkt"
          "north_america.rkt"
          "oceania.rkt"
          "south_america.rkt")
         qi)

(provide cities-temps)

(define cities-temps
  (let* ([all-cities-data
          (sequence-append africa:data
                           asia:data
                           europe:data
                           north_america:data
                           oceania:data
                           south_america:data)])
    (for/list ([record all-cities-data])
      (cons (list-ref record 1)
            (~> (record) (list-ref 2) (list-ref 12) car)))))
