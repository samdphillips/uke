#lang racket/base

;; index-ref
;; index-size
;; index-compose

(define-values (prop:index index? index-ops)
  (make-struct-type-property 'index))

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
    [(get-index-op i0 2) => (λ (f) (apply f i0 i1))]
    [else (generic-index-compose i0 i1)]))

(define (generic-index-compose i0 i1) (void))

(define (linear-index-ref idx i)
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

(define store-ref  vector-ref)
(define store-size vector-length)

(struct series (name index store)
  #:transparent)

(define (series-ref s i)
  (store-ref
   (series-store s)
   (index-ref (series-index s) i)))

(struct dataframe (index series*)
  #:transparent)

(require (for-syntax racket/base) syntax/parse/define)

(define-syntax-parse-rule
  (for/dataframe (column-names:id ...) for-clauses body ...)
  #:with this-syntax this-syntax
  #:do [(define stride (length (syntax-e #'(column-names ...))))]
  #:with (series-v ...) (generate-temporaries #'(column-names ...))
  #:with (ks ...) (for/list ([i (in-range stride)]) #`'#,i)
  #:with stride #`'#,stride
  (let ()
    (define s (make-vector (* 16 stride)))
    (define (build size)
      (define series-v (series 'column-names (linear-index size ks stride) s))
      ...
      (dataframe (make-linear-index size) (list series-v ...)))
    (for/fold/derived this-syntax
      ([i 0] [j 0] #:result (build j))
      for-clauses
      (call-with-values
       (λ () body ...)
       (λ (column-names ...)
         (vector-set! s (+ i ks) column-names)
         ...
         (values (+ i stride) (add1 j)))))))

(define x
  (for/dataframe (i) ([n 10]) n))

