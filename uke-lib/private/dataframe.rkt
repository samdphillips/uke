#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
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
         dataframe-column-names
         dataframe-columns
         dataframe-column-ref
         dataframe-column*-ref
         dataframe-column-lift
         dataframe-add-column*
         dataframe-remove-column*
         dataframe-reorder-columns
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

(define (dataframe-columns df)
  (define idx (dataframe-index df))
  (for/list ([col (in-list (dataframe-column* df))])
    (column-push-index col idx)))

(define (dataframe-column-names df)
  (for/list ([col (in-list (dataframe-column* df))])
    (column-name col)))

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
  (define found-col
    (for/first ([col (in-list (dataframe-column* df))]
                #:when (equal? (column-name col) col-name))
      col))
  (cond
    [found-col => success-result]
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

(define-syntax-parse-rule (generate-column-lift df:id
                                                combine:id
                                                col-names:id ...)
  #:with (col ...) (generate-temporaries #'(col-names ...))
  (let ()
    (define idx (dataframe-index df))
    ;; XXX better error for missing column names
    (define col (dataframe-column*-ref df col-names)) ...
    (λ (i) (combine (dataframe-cell-ref* idx col i) ...))))

(define-syntax-parse-rule (generate-column-lifts n:nat)
  #:with (procs ...)
  (for/list ([i (syntax->datum #'n)])
    (define col-names (for/list ([v (add1 i)]) (generate-temporary 'col-name)))
    #`(lambda (df combine #,@col-names)
        (generate-column-lift df combine #,@col-names)))
  (values procs ...))

(define-values
  (dataframe-column-lift1
   dataframe-column-lift2
   dataframe-column-lift3
   dataframe-column-lift4)
  (generate-column-lifts 4))

(define dataframe-column-lift
  (case-lambda
    [(df combine col-name1)
     (dataframe-column-lift1 df combine col-name1)]
    [(df combine col-name1 col-name2)
     (dataframe-column-lift2 df combine col-name1 col-name2)]
    [(df combine col-name1 col-name2 col-name3)
     (dataframe-column-lift3 df combine col-name1 col-name2 col-name3)]
    [(df combine col-name1 col-name2 col-name3 col-name4)
     (dataframe-column-lift4 df combine col-name1 col-name2 col-name3 col-name4)]
    [(df combine . col-names)
     (define idx (dataframe-index df))
     ;; XXX better error for missing column names
     (define refs
       (for/list ([n (in-list col-names)])
         (define s (dataframe-column*-ref df n))
         (λ (i) (dataframe-cell-ref* idx s i))))
     (λ (i)
       (apply combine (for/list ([ref (in-list refs)]) (ref i))))]))

(define (dataframe-add-column* df . add-cols)
  (define df-idx (dataframe-index df))
  (define df-cols
    ;; XXX: same as dataframe-column
    (for/list ([col (in-list (dataframe-column* df))])
      (column-push-index col df-idx)))
  ;; XXX: this can just be make-dataframe
  (struct-copy dataframe df
               [index   (make-linear-index (index-size df-idx))]
               [column* (append df-cols add-cols)]))

(define (dataframe-remove-column* df . remove-cols)
  (define new-cols
    (for/list ([col (in-list (dataframe-column* df))]
               #:unless (memq (column-name col) remove-cols))
      col))
  ;; XXX: dataframe-column*-update
  (struct-copy dataframe df [column* new-cols]))

;; XXX: dataframe-rename-column

(define (dataframe-reorder-columns df col-names)
  (define (reorder cols)
    (define order
      (for/hash ([name (in-list col-names)]
                 [i (in-naturals)])
        (values name i)))
    (define col-vec (make-vector (length col-names) #f))
    (for ([col (in-list cols)]
          #:do [(define col-name (column-name col))]
          #:when (hash-has-key? order col-name))
      (vector-set! col-vec (hash-ref order col-name) col))

    ;; simpler to validate column names here
    (for ([col (in-vector col-vec)]
          [col-name (in-list col-names)])
      (unless col
        ((dataframe-column-ref-failure 'dataframe-reorder-columns col-name))))

    (vector->list col-vec))
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
  (define (select idx)
    (index-select idx pred?))
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
    ;; XXX: short-cut here, instead of making a new dataframe and index the
    ;;      aggr-func takes original dataframe and list of indices
    (values k (aggr-func group-df))))

(define (dataframe-left-join left-df left-key-func left-remove-names
                             right-df right-key-func right-remove-names)
  (define group-left (dataframe-group-index left-df left-key-func))
  (define group-right (dataframe-group-index right-df right-key-func))

  (define (list->index df-idx vs)
    (index-compose df-idx (make-vector-index (list->vector (reverse vs)))))
  (define (make-new-column df remove-column-names join-index)
    (define df-idx (dataframe-index df))
    (for/list ([s (in-list (dataframe-column* df))]
               #:unless (member (column-name s) remove-column-names))
      (column-index-update s (λ (idx) (index-compose idx df-idx join-index)))))
  (define-values (left-idx right-idx)
    (for*/fold ([left-indices null]
                [right-indices null]
                #:result
                (values (list->index (dataframe-index left-df) left-indices)
                        (list->index (dataframe-index right-df) right-indices)))
               ([(g i*) (in-immutable-hash group-left)]
                #:do [(define j* (hash-ref group-right g '(-1)))]
                [i (in-list i*)]
                [j (in-list j*)])
      (values (cons i left-indices) (cons j right-indices))))
  (make-dataframe
   #:index (make-linear-index (index-size left-idx))
   (append
    (make-new-column left-df left-remove-names left-idx)
    (make-new-column right-df right-remove-names right-idx))))

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
       #:with (col-temp ...) (generate-temporaries #'(col.name ...))
       #:with (ks ...) (for/list ([i (in-range stride)]) #`'#,i)
       #:with stride #`'#,stride
       #:with _for/fold for-stx
       #'(let ()
           (define init-rows 16)
           (define (build vec size)
             ;; XXX: make store immutable
             (define col-temp
               (make-column 'col.name
                            (make-linear-index size ks stride)
                            #:properties
                            (hash {~@ 'col.prop-name col.prop-expr} ...)
                            vec))
             ...
             (make-dataframe #:index (make-linear-index size)
                             (list col-temp ...)))
           (_for/fold this-syntax
             ([vec (make-vector (* init-rows stride) (void))]
              [i 0] [j 0] [k (sub1 init-rows)]
              #:result (build vec j))
             for-clauses
             (call-with-values
              (λ () body ...)
              (λ (col-temp ...)
                (vector-set! vec (+ i ks) col-temp)
                ...
                (define (next vec k) (values vec (+ i stride) (add1 j) (sub1 k)))
                (cond
                  [(zero? k)
                   (define k (ceiling (* 1/2 (add1 j))))
                   (define next-vec (make-vector (* (+ k j 1) stride) (void)))
                   (vector-copy! next-vec 0 vec)
                   (next next-vec k)]
                  [else
                   (next vec k)])))))])))

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
