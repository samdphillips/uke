#lang racket/base

(require (for-syntax racket/base)
         racket/unsafe/ops
         syntax/parse/define
         "index.rkt"
         "series.rkt")

(provide (struct-out dataframe)
         dataframe-num-rows
         dataframe-series
         dataframe-series-ref
         dataframe-series*-ref
         dataframe-add-series*
         dataframe-remove-series*
         dataframe-reorder-series
         dataframe-select
         dataframe-cell-ref
         dataframe-cell-ref*
         for/dataframe)

;; XXX: series-metadata access

(struct dataframe (index series*) #:transparent)

;; XXX: actually compute a compatible index
;; XXX: move to index.rkt
(define (compatible-index a-series-list)
  (series-index (car a-series-list)))

(define (make-dataframe a-series-list
                        #:index [an-index (compatible-index a-series-list)])
  (dataframe an-index a-series-list))

(define (dataframe-num-rows df)
  (index-size (dataframe-index df)))

(define (dataframe-series a-dataframe)
  (define an-index (dataframe-index a-dataframe))
  (for/list ([a-series (in-list (dataframe-series* a-dataframe))])
    (series-push-index a-series an-index)))

;; get a series out of a dataframe without pushing an index into it
(define (dataframe-series*-ref df a-series-name)
  (for/first ([a-series (in-list (dataframe-series* df))]
              #:when (equal? (series-name a-series) a-series-name))
    a-series))

(define (dataframe-series-ref df a-series-name)
  (series-push-index (dataframe-series*-ref df a-series-name)
                     (dataframe-index df)))

(define (dataframe-add-series* df . series-to-add)
  (define df-idx (dataframe-index df))
  (define series*
    (for/list ([a-series (in-list (dataframe-series* df))])
      (series-push-index a-series df-idx)))
  (struct-copy dataframe df
               [index   (make-linear-index (index-size df-idx))]
               [series* (append series* series-to-add)]))

(define (dataframe-remove-series* df . series-names)
  (define series*
    (for/list ([a-series (in-list (dataframe-series* df))]
               #:unless (memq (series-name a-series) series-names))
      a-series))
  (struct-copy dataframe df [series* series*]))

(define (dataframe-reorder-series df . series-names)
  (define series*
    (for/list ([name (in-list series-names)])
      (dataframe-series-ref df name)))
  (struct-copy dataframe df [series* series*]))

(define (dataframe-select df pred?)
  (define new-index
    (index-compose
     (dataframe-index df)
     (make-vector-index
      (unsafe-vector*->immutable-vector!
       (for/vector ([i (in-indices (dataframe-index df))]
                    #:when (pred? i)) i)))))
  (struct-copy dataframe df [index new-index]))

(define (dataframe-cell-ref df a-series-name i)
  (define j (index-ref (dataframe-index df) i))
  (series-ref (dataframe-series*-ref df a-series-name) j))

;; A more primitive form of dataframe-cell-ref that avoids series lookup
(define (dataframe-cell-ref* df-index a-series i)
  (series-ref a-series (index-ref df-index i)))

(define-syntax-parse-rule
  (for/dataframe (column-names:id ...) for-clauses body ...+)
  #:with this-syntax this-syntax
  #:do [(define stride (length (syntax-e #'(column-names ...))))]
  #:with (series-v ...) (generate-temporaries #'(column-names ...))
  #:with (ks ...) (for/list ([i (in-range stride)]) #`'#,i)
  #:with stride #`'#,stride
  (let ()
    (define init-rows 16)
    (define s (make-vector (* init-rows stride)))
    (define (build size)
      (define series-v
        (make-series 'column-names (make-linear-index size ks stride) s))
      ...
      (dataframe (make-linear-index size) (list series-v ...)))
    (for/fold/derived this-syntax
      ([i 0] [j 0] [k (sub1 init-rows)] #:result (build j))
      for-clauses
      (call-with-values
       (λ () body ...)
       (λ (column-names ...)
         (vector-set! s (+ i ks) column-names)
         ...
         (define (next k) (values (+ i stride) (add1 j) (sub1 k)))
         (cond
           [(zero? k)
            (define k (ceiling (* 1/2 (add1 j))))
            (define next-s (make-vector (* (+ k j 1) stride)))
            (vector-copy! next-s 0 s)
            (set! s next-s)
            (next k)]
           [else
            (next k)]))))))