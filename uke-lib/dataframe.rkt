#lang racket/base

(require (for-syntax racket/base)
         racket/unsafe/ops
         syntax/parse/define

         "column.rkt"
         "error.rkt"
         "index.rkt")

(provide (struct-out dataframe)
         make-dataframe
         dataframe-num-rows
         dataframe-index-update
         dataframe-column*-update
         dataframe-columns
         dataframe-column-ref
         dataframe-column*-ref
         dataframe-column-lift
         dataframe-add-column*
         dataframe-remove-column*
         dataframe-reorder-column
         dataframe-reverse-rows
         dataframe-compact?
         dataframe-compact
         dataframe-select
         dataframe-slice
         dataframe-group-index
         dataframe-group
         dataframe-left-join
         dataframe-cell-ref
         dataframe-cell-ref*

         for/dataframe
         for*/dataframe
         row-df
         column-df)

;; XXX: column-metadata access

(struct dataframe (index column*) #:transparent)

;; XXX: actually compute a compatible index
;; XXX: move to index.rkt
;; XXX: this actually make a really wrong index for a slice
#;
(define (compatible-index a-series-list)
  (column-index (car a-series-list)))

(define (make-dataframe col-list
                        #:index an-index)
  (check-index-compatible* 'make-dataframe
                           an-index
                           (for/list ([s (in-list col-list)])
                             (column-index s)))
  (dataframe an-index col-list))

(define (dataframe-index-update df f)
  (struct-copy dataframe df [index (f [dataframe-index df])]))

(define (dataframe-column*-update df f)
  (struct-copy dataframe df [column* (f (dataframe-column* df))]))

(define (dataframe-num-rows df)
  (index-size (dataframe-index df)))

(define (dataframe-columns a-dataframe)
  (define an-index (dataframe-index a-dataframe))
  (for/list ([col (in-list (dataframe-column* a-dataframe))])
    (column-push-index col an-index)))

(define (dataframe-column-ref-failure who col-name)
  (lambda ()
    (raise-uke-error exn:uke:dataframe
                     who
                     "column ~a does not exist in dataframe"
                     col-name)))

;; get a column out of a dataframe without pushing an index into it
(define (dataframe-column*-ref df
                               col-name
                               [failure-result
                                (dataframe-column-ref-failure
                                 'dataframe-column*-ref
                                 col-name)]
                               [success-result values])
  (define found
    (for/first ([col (in-list (dataframe-column* df))]
                #:when (equal? (column-name col) col-name))
      col))
  (cond
    [found => success-result]
    [(procedure? failure-result) (failure-result)]
    [else failure-result]))

(define (dataframe-column-ref df
                              col-name
                              [failure-result
                               (dataframe-column-ref-failure
                                'dataframe-column-ref
                                col-name)])
  (dataframe-column*-ref df
                         col-name
                         failure-result
                         (lambda (s)
                           (column-push-index s (dataframe-index df)))))

(define ~dataframe-column-lift
  (procedure-rename
   (lambda (df col-names f)
     (define idx (dataframe-index df))
     ;; XXX better error for missing column names
     (define refs
       (for/list ([n (in-list col-names)])
         (define s (dataframe-column*-ref df n))
         (λ (i) (dataframe-cell-ref* idx s i))))
     (λ (i)
       (apply f (for/list ([ref (in-list refs)]) (ref i)))))
   'dataframe-column-lift))

(define-syntax dataframe-column-lift
  (syntax-parser
    #:literals (quote)
    [(_ df-expr '(col-names:id ...) proc)
     #:declare df-expr (expr/c #'dataframe?)
     #:with (col ...) (generate-temporaries #'(col-names ...))
     #'(let ()
         (define df df-expr.c)
         (define idx (dataframe-index df))
         ;; XXX better error for missing column names
         (define col (dataframe-column*-ref df 'col-names))
         ...
         (λ (i)
           (proc (dataframe-cell-ref* idx col i) ...)))]
    [_:id #'~dataframe-column-lift]
    [(_ . rest) #'(~dataframe-column-lift . rest)]))

(define (dataframe-add-column* df . cols-to-add)
  (define df-idx (dataframe-index df))
  (define col*
    ;; XXX: same as dataframe-column
    (for/list ([col (in-list (dataframe-column* df))])
      (column-push-index col df-idx)))
  ;; XXX: this can just be make-dataframe
  (struct-copy dataframe df
               [index   (make-linear-index (index-size df-idx))]
               [column* (append col* cols-to-add)]))

(define (dataframe-remove-column* df . col-names)
  (define col*
    (for/list ([col (in-list (dataframe-column* df))]
               #:unless (memq (column-name col) col-names))
      col))
  (struct-copy dataframe df [column* col*]))

;; XXX: dataframe-rename-column

;; XXX: this looks weird
(define (dataframe-reorder-column df col-names)
  (define col*
    (for/list ([name (in-list col-names)])
      (dataframe-column-ref df name)))
  (define (reorder s*)
    (for/list ([name (in-list col-names)])
      (define fail
        (dataframe-column-ref-failure 'dataframe-reorder-column name))
      (or (for/first ([col (in-list s*)]
                      #:when (equal? (column-name col) name))
            col)
          (fail))))
  (dataframe-column*-update df reorder))

(define (dataframe-reverse-rows df)
  (dataframe-index-update df index-reverse))

(define (dataframe-compact? df)
  (and (index-compact? (dataframe-index df))
       (for/and ([s (in-list (dataframe-column* df))])
         (column-compact? s))))

(define (dataframe-compact df)
  (cond
    [(dataframe-compact? df) df]
    [else
     (make-dataframe
      ;; XXX: dataframe-column allocates a column and index (which may be
      ;;      short lived since we then compact it)
      ;; XXX: could reuse index if it is compact
      (for/list ([s (in-list (dataframe-columns df))])
        (column-compact s))
      #:index (make-linear-index
               (index-size (dataframe-index df))))]))

(define (dataframe-select df pred?)
  (define (select idx0)
    (index-select idx0 pred?))
  (dataframe-index-update df select))

(define (dataframe-slice df start [size (- (dataframe-num-rows df) start)])
  (dataframe-index-update df (λ (idx) (index-slice idx start size))))

(define (dataframe-group-index df key-func)
  (define ((add-index i) vs) (cons i vs))
  (for/fold ([groups (hash)]) ([i (in-indices (dataframe-index df))])
    (hash-update groups (key-func i) (add-index i) null)))

(define (dataframe-group df key-func [aggr-func values])
  (define groups (dataframe-group-index df key-func))
  (for/dataframe (key groups) ([(k g) (in-immutable-hash groups)])
    (define group-df
      (dataframe-index-update df (λ (idx) (index-pick idx g))))
    (values k (aggr-func group-df))))

(define (dataframe-left-join dfl keyl removel
                             dfr keyr remover)
  (define groupl (dataframe-group-index dfl keyl))
  (define groupr (dataframe-group-index dfr keyr))

  (define (list->index dfi vs)
    (index-compose dfi (make-vector-index (list->vector (reverse vs)))))
  (define (make-new-column df remove-columns join-index)
    (define dfi (dataframe-index df))
    (for/list ([s (in-list (dataframe-column* df))]
               #:unless (member (column-name s) remove-columns))
      (column-index-update s (λ (idx) (index-compose idx dfi join-index)))))
  (define-values (il ir)
    (for*/fold ([il null]
                [ir null]
                #:result
                (values (list->index (dataframe-index dfl) il)
                        (list->index (dataframe-index dfr) ir)))
               ([(g i*) (in-immutable-hash groupl)]
                #:do [(define j* (hash-ref groupr g '(-1)))]
                [i (in-list i*)]
                [j (in-list j*)])
      (values (cons i il) (cons j ir))))
  (make-dataframe
   #:index (make-linear-index (index-size il))
   (append
    (make-new-column dfl removel il)
    (make-new-column dfr remover ir))))

(define (dataframe-cell-ref df col-name i)
  (define j (index-ref (dataframe-index df) i))
  (column-ref (dataframe-column*-ref df col-name) j))

;; A more primitive form of dataframe-cell-ref that avoids column lookup
(define (dataframe-cell-ref* df-index col i)
  (column-ref col (index-ref df-index i)))

;; XXX dynamic column names are desirable?
(begin-for-syntax
  (define-syntax-class col-spec
    [pattern name:id
      #:attr [prop-name 1] '()
      #:attr [prop-expr 1] '()]
    [pattern (name:id {~seq prop-name:keyword prop-expr} ...)])
  (define (make-for/dataframe for-stx)
    (syntax-parser
      [(_ (col:col-spec ...) for-clauses body ...+)
       #:with this-syntax this-syntax
       #:do [(define stride (length (syntax-e #'(col ...))))]
       #:with (col-v ...) (generate-temporaries #'(col.name ...))
       #:with (ks ...) (for/list ([i (in-range stride)]) #`'#,i)
       #:with stride #`'#,stride
       #:with _for/fold for-stx
       #'(let ()
           (define init-rows 16)
           (define (build store size)
             ;; XXX: make store immutable
             (define col-v
               (make-column 'col
                            (make-linear-index size ks stride)
                            #:properties
                            (hash {~@ 'col.prop-name col.prop-expr} ...)
                            store))
             ...
             (make-dataframe #:index (make-linear-index size)
                             (list col-v ...)))
           (_for/fold this-syntax
             ([s (make-vector (* init-rows stride) (void))]
              [i 0] [j 0] [k (sub1 init-rows)]
              #:result (build s j))
             for-clauses
             (call-with-values
              (λ () body ...)
              (λ (col ...)
                (vector-set! s (+ i ks) col)
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

(define-syntax-parse-rule (row-df [col:col-spec ...] . elems)
  #:do [(define stride (length (syntax-e #'(col ...))))
        (define elems-size (length (syntax-e #'elems)))]
  #:fail-unless (zero? (modulo elems-size stride))
  (format "incorrect number of elements for ~a columns" stride)

  #:with (col-v ...) (generate-temporaries #'(col ...))
  #:with (ks ...) (for/list ([i (in-range stride)]) #`'#,i)
  #:with num-rows #`'#,(quotient elems-size stride)
  #:with stride #`'#,stride

  (let ()
    (define store (vector-immutable . elems))
    (define col-v
      (make-column 'col.name
                   #:properties
                   (hash {~@ 'col.prop-name col.prop-expr} ...)
                   (make-linear-index num-rows ks stride)
                   store)) ...
    (make-dataframe #:index (make-linear-index num-rows)
                    (list col-v ...))))

(define-syntax-parse-rule (column-df [col:col-spec . elems] ...)
  #:with (col-v ...) (generate-temporaries #'(col ...))
  #:do [(define nr
          (for/list ([e* (in-list (syntax-e #'(elems ...)))])
            (length (syntax-e e*))))]
  #:fail-unless (if (null? nr) #t (apply = nr))
  (format "columns are different sizes ~a" nr)
  #:with num-rows #`'#,(if (null? nr) 0 (car nr))

  (let ()
    (define col-v
      (make-column 'col.name
                   #:properties
                   (hash {~@ 'col.prop-name col.prop-expr} ...)
                   (make-linear-index num-rows)
                   (vector-immutable . elems))) ...
    (make-dataframe #:index (make-linear-index num-rows)
                    (list col-v ...))))