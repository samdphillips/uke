#lang racket/base

(provide (struct-out dataframe)
         dataframe-series
         dataframe-series-ref
         dataframe-num-rows
         dataframe-add-series*
         for/dataframe

         (struct-out series)
         series-ref
         series-push-index
         vector->series

         make-vector-index
         make-linear-index
         index-ref
         index-size
         index-compose

         store-ref)

(require racket/unsafe/ops)

(define-values (prop:index index? index-ops)
  (make-struct-type-property 'index))

;; index-ref
;; index-size
;; index-compose

(define (get-index-op idx slot)
  (vector-ref (index-ops idx) slot))

(define (apply-index-op idx slot . args)
  (apply (get-index-op idx slot) idx args))

(define (check-index-bounds who idx i)
  (unless (and (<= 0 i) (< i (index-size idx)))
    (error who "index out of range for index: ~a" i)))

(define (index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (apply-index-op idx 0 i))

(define (index-size idx)
  (apply-index-op idx 1))

(define (index-compose i0 i1)
  (unless (<= (index-size i1) (index-size i0))
    (error 'index-compose "index sizes incompatible"))
  (cond
    [(get-index-op i0 2) => (λ (f) (f i0 i1))]
    [else (generic-index-compose i0 i1)]))

(define (generic-index-compose i0 i1)
  (define n (index-size i1))
  (define vec
    (unsafe-vector*->immutable-vector!
     (for/vector #:length n ([i (in-range n)])
       (index-ref i0 (index-ref i1 i)))))
  (vector-index vec))

(define (linear-index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (+ (linear-index-offset idx)
     (* (linear-index-stride idx) i)))

(define (linear-index-compose i0 i1)
  (cond
    [(not (linear-index? i1)) (generic-index-compose i0 i1)]
    [else (define o0 (linear-index-offset i0))
          (define s0 (linear-index-stride i0))
          (define o1 (linear-index-offset i1))
          (define s1 (linear-index-stride i1))
          (linear-index (index-size i1)
                        (+ o0 (* s0 o1))
                        (* s0 s1))]))

(struct linear-index (size offset stride)
  #:transparent
  #:property prop:index
  (vector linear-index-ref
          (λ (idx) (linear-index-size idx))
          linear-index-compose))

(define (make-linear-index size [offset 0] [stride 1])
  (linear-index size offset stride))

(define (vector-index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (vector-ref (vector-index-mapping idx) i))

(define (vector-index-size idx)
  (vector-length (vector-index-mapping idx)))

(struct vector-index (mapping)
  #:transparent
  #:property prop:index
  (vector vector-index-ref
          vector-index-size
          #f))

(define (make-vector-index vec)
  (vector-index vec))

(define store-ref  vector-ref)
(define store-size vector-length)

(struct series (name index store)
  #:transparent)

(define (series-ref s i)
  (store-ref
   (series-store s)
   (index-ref (series-index s) i)))

(define (series-push-index s idx)
  (struct-copy series s [index (index-compose (series-index s) idx)]))

(define (vector->series name vec)
  (series name (make-linear-index (vector-length vec)) vec))

(struct dataframe (index series*)
  #:transparent)

(define (dataframe-series df)
  (define idx (dataframe-index df))
  (for/list ([s (in-list (dataframe-series* df))])
    (series-push-index s idx)))

(define (dataframe-series-ref df name)
  (for/first ([a-series (in-list (dataframe-series* df))]
              #:when (equal? name (series-name a-series)))
    (series-push-index a-series (dataframe-index df))))

(define (dataframe-num-rows df)
  (index-size (dataframe-index df)))

(define (dataframe-add-series* df . series-to-add)
  (define df-idx (dataframe-index df))
  (define series*
    (for/list ([a-series (in-list (dataframe-series* df))])
      (series-push-index a-series df-idx)))
  (struct-copy dataframe df
               [index   (make-linear-index (index-size df-idx))]
               [series* (append series* series-to-add)]))

(require (for-syntax racket/base) syntax/parse/define)

(define-syntax-parse-rule
  (for/dataframe (column-names:id ...) for-clauses body ...)
  #:with this-syntax this-syntax
  #:do [(define stride (length (syntax-e #'(column-names ...))))]
  #:with (series-v ...) (generate-temporaries #'(column-names ...))
  #:with (ks ...) (for/list ([i (in-range stride)]) #`'#,i)
  #:with stride #`'#,stride
  (let ()
    (define init-rows 16)
    (define s (make-vector (* init-rows stride)))
    (define (build size)
      (define series-v (series 'column-names (linear-index size ks stride) s))
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

