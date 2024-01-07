#lang racket/base

(require data/applicative
         data/monad
         megaparsack
         megaparsack/text
         racket/port
         syntax/strip-context)

(provide (rename-out [r:read-syntax read-syntax]))

(define name/p
  (chain (compose pure list->string)
         (label/p "name/p"
                  (many+/p
                   (or/p letter/p
                         (one-of/p
                          '(#\space #\' #\- #\. #\,)))))))

(define (chars->integer cs)
  (define s
    (cond
      [(list? cs) (list->string cs)]
      [else (string cs)]))
  (string->number s))

(define (integer/p p)
  (chain (compose pure chars->integer) p))

(define (make-number sign? ip fp)
  (* (if sign? 1 -1) (+ ip (/ fp 10))))

(define number/p
  (do [sign <- (or/p (do (char/p #\-) (pure #f))
                     (pure #t))]
    [integer-part <- (integer/p (many+/p digit/p))]
    [fraction-part <- (or/p (do (char/p #\.) (integer/p digit/p))
                            (pure 0))]
    (pure (make-number sign integer-part fraction-part))))

(define newline/p (char/p #\newline))
(define tab/p (char/p #\tab))

(define line/p
  (do [country <- name/p]
    tab/p
    [city <- name/p]
    tab/p
    [temps <- (many/p #:min 13
                      #:max 13
                      #:sep tab/p
                      (do [c <- number/p]
                        newline/p
                        (char/p #\()
                        [f <- number/p]
                        (char/p #\))
                        (pure (list c f))))]
    space/p
    (char/p #\[) (many+/p digit/p) (char/p #\])
    (pure (list country city temps))))

(define data/p
  (do (many/p space/p)
    [data <- (many/p #:sep
                     (try/p (do newline/p (lookahead/p name/p)))
                     line/p)]
    (many/p space/p)
    eof/p
    (pure data)))

(define (r:read-syntax src in)
  (define s (port->string in))
  (define t (parse-result! (parse-string data/p s src)))
  (strip-context
   #`(module m racket/base
       (provide data)
       (define data '#,t))))
