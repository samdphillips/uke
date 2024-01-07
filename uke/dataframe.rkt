#lang racket/base

(require (for-syntax racket/base)
         racket/unsafe/ops
         syntax/parse/define
         "index.rkt"
         "series.rkt")

(provide (struct-out dataframe)
         make-dataframe
         dataframe-num-rows
         dataframe-index-update
         dataframe-series*-update
         dataframe-series
         dataframe-series-ref
         dataframe-series*-ref
         dataframe-series-lift
         dataframe-add-series*
         dataframe-remove-series*
         dataframe-reorder-series
         dataframe-reverse-rows
         dataframe-compact?
         dataframe-compact
         dataframe-select
         dataframe-slice
         dataframe-group
         dataframe-cell-ref
         dataframe-cell-ref*
         for/dataframe
         for*/dataframe)

;; XXX: series-metadata access

(struct dataframe (index series*) #:transparent)

;; XXX: actually compute a compatible index
;; XXX: move to index.rkt
;; XXX: this actually make a really wrong index for a slice
(define (compatible-index a-series-list)
  (series-index (car a-series-list)))

(define (make-dataframe a-series-list
                        #:index [an-index (compatible-index a-series-list)])
  (dataframe an-index a-series-list))

(define (dataframe-index-update df f)
  (struct-copy dataframe df [index (f [dataframe-index df])]))

(define (dataframe-series*-update df f)
  (struct-copy dataframe df [series* (f (dataframe-series* df))]))

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

;; XXX handle nicer when series is missing
(define (dataframe-series-ref df a-series-name)
  (series-push-index (dataframe-series*-ref df a-series-name)
                     (dataframe-index df)))

;; XXX procedure-rename
(define (~dataframe-series-lift df series-names f)
  (define idx (dataframe-index df))
  ;; XXX check series names
  (define refs
    (for/list ([n (in-list series-names)])
      (define s (dataframe-series*-ref df n))
      (λ (i) (dataframe-cell-ref* idx s i))))
  (λ (i)
    (apply f (for/list ([ref (in-list refs)]) (ref i)))))

(define-syntax dataframe-series-lift
  (syntax-parser
    [(_ df-expr '(series-names:id ...) proc)
     #:declare df-expr (expr/c #'dataframe?)
     #:with (a-series ...) (generate-temporaries #'(series-names ...))
     #'(let ()
         (define df df-expr.c)
         (define idx (dataframe-index df))
         (define a-series (dataframe-series*-ref df 'series-names))
         ...
         (λ (i)
           (proc (dataframe-cell-ref* idx a-series i) ...)))]
    [_:id #'~dataframe-series-lift]
    [(_ . rest) #'(~dataframe-series-lift . rest)]))

(define (dataframe-add-series* df . series-to-add)
  (define df-idx (dataframe-index df))
  (define series*
    (for/list ([a-series (in-list (dataframe-series* df))])
      (series-push-index a-series df-idx)))
  ;; XXX: this can just be make-dataframe
  (struct-copy dataframe df
               [index   (make-linear-index (index-size df-idx))]
               [series* (append series* series-to-add)]))

(define (dataframe-remove-series* df . series-names)
  (define series*
    (for/list ([a-series (in-list (dataframe-series* df))]
               #:unless (memq (series-name a-series) series-names))
      a-series))
  (struct-copy dataframe df [series* series*]))

;; XXX: dataframe-rename-series

;; XXX: check that this works as intended.
;;      Also should use dataframe-series*-ref
(define (dataframe-reorder-series df . series-names)
  (define series*
    (for/list ([name (in-list series-names)])
      (dataframe-series-ref df name)))
  (struct-copy dataframe df [series* series*]))

(define (dataframe-reverse-rows df)
  (dataframe-index-update df index-reverse))

(define (dataframe-compact? df)
  (and (index-compact? (dataframe-index df))
       (for/and ([s (in-list (dataframe-series* df))])
         (series-compact? s))))

(define (dataframe-compact df)
  (cond
    [(dataframe-compact? df) df]
    [else
     (make-dataframe
      ;; XXX: dataframe-series allocates series and index
      (for/list ([s (in-list (dataframe-series df))])
        (series-compact s)))]))

(define (dataframe-select df pred?)
  (define (select idx0)
    (index-select idx0 pred?))
  (dataframe-index-update df select))

(define (dataframe-slice df start [size (- (dataframe-num-rows df) start)])
  (dataframe-index-update df (λ (idx) (index-slice idx start size))))

;; XXX: name output series
(define (dataframe-group df key-func [aggr-func values])
  (define ((add-index i) v) (cons i v))
  (define groups
    (for/fold ([groups (hash)]) ([i (in-indices (dataframe-index df))])
      (hash-update groups (key-func i) (add-index i) null)))
  (for/dataframe (key groups) ([(k g) (in-immutable-hash groups)])
    (define group-df
      (dataframe-index-update df (λ (idx) (index-pick idx g))))
    (values k (aggr-func group-df))))

(define (dataframe-cell-ref df a-series-name i)
  (define j (index-ref (dataframe-index df) i))
  (series-ref (dataframe-series*-ref df a-series-name) j))

;; A more primitive form of dataframe-cell-ref that avoids series lookup
(define (dataframe-cell-ref* df-index a-series i)
  (series-ref a-series (index-ref df-index i)))

;; XXX dynamic series names are desirable
(begin-for-syntax
  (define-syntax-class series-spec
    [pattern name:id
      #:attr [prop-name 1] '()
      #:attr [prop-expr 1] '()]
    [pattern (name:id {~seq prop-name:keyword prop-expr} ...)])
  (define (make-for/dataframe for-stx)
    (syntax-parser
      [(_ (series:series-spec ...) for-clauses body ...+)
       #:with this-syntax this-syntax
       #:do [(define stride (length (syntax-e #'(series ...))))]
       #:with (series-v ...) (generate-temporaries #'(series.name ...))
       #:with (ks ...) (for/list ([i (in-range stride)]) #`'#,i)
       #:with stride #`'#,stride
       #:with _for/fold for-stx
       #'(let ()
           (define init-rows 16)
           (define (build store size)
             ;; XXX: make store immutable
             (define series-v
               (make-series 'series.name
                            (make-linear-index size ks stride)
                            #:properties
                            (hash {~@ 'series.prop-name series.prop-expr} ...)
                            store))
             ...
             (dataframe (make-linear-index size) (list series-v ...)))
           (_for/fold this-syntax
             ([s (make-vector (* init-rows stride) (void))]
              [i 0] [j 0] [k (sub1 init-rows)]
              #:result (build s j))
             for-clauses
             (call-with-values
              (λ () body ...)
              (λ (series.name ...)
                (vector-set! s (+ i ks) series.name)
                ...
                (define (next s k) (values s (+ i stride) (add1 j) (sub1 k)))
                (cond
                  [(zero? k)
                   (define k (ceiling (* 1/2 (add1 j))))
                   (define next-s (make-vector (* (+ k j 1) stride) (void)))
                   (vector-copy! next-s 0 s)
                   (next next-s k)]
                  [else
                   (next s k)])))))])))

(define-syntax for/dataframe (make-for/dataframe #'for/fold/derived))
(define-syntax for*/dataframe (make-for/dataframe #'for*/fold/derived))