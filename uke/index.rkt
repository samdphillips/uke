#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/sequence
         racket/unsafe/ops
         racket/vector
         "error.rkt")

(provide prop:index
         index?
         index-ref
         index-size
         index-compose
         index-max-range
         index-compact?
         in-indices

         index-pick
         index-select
         index-extract
         index-slice
         index-reverse
         index-sort

         check-index-compatible*
         exn:uke:index?

         vector-index?
         linear-index?

         make-vector-index

         make-linear-index
         linear-index-offset
         linear-index-stride)

(module* for-test #f
  (provide (struct-out linear-index)))

;; index-ref
;; index-size
;; index-compose
;; XXX eventually need a property guard if this is exported
(define-values (prop:index index? index-ops)
  (make-struct-type-property 'index))

(define (get-index-op idx slot)
  (vector-ref (index-ops idx) slot))

(define (apply-index-op idx slot . args)
  (apply (get-index-op idx slot) idx args))

(define (check-index-bounds who idx i)
  (unless (and (<= 0 i) (< i (index-size idx)))
    (raise-uke-error exn:uke:index who "index out of range for index: ~a" i)))

(define (check-index-compatible who i0 i1)
  (unless (<= (index-max-range i0) (index-size i1))
    (raise-uke-error exn:uke:index
                     who
                     "indexes incompatible: range ~a > size ~a"
                     (index-max-range i0)
                     (index-size i1))))

(define (check-index-compatible* who idx idx*)
  (for ([i (in-list idx*)])
    (check-index-compatible who idx i)))

(define (index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (apply-index-op idx 0 i))

(define (index-size idx)
  (apply-index-op idx 1))

;; XXX if i1 == (linear-index sz 0 1) and (index-size i0) == sz return i0
;; XXX if i1 == (linear-index sz 0 1) and
;;        (vector-index? i0) and
;;        (index-size i0) != sz
;;     only copy relevant part of index instead of recalculating the table.
(define index-compose
  (case-lambda
    [(i0 i1)
     (check-index-compatible 'index-compose i1 i0)
     (cond
       [(get-index-op i0 2) => (λ (f) (f i0 i1))]
       [else (generic-index-compose i0 i1)])]
    [(i0 i1 . rest)
     (apply index-compose (index-compose i0 i1) rest)]))

(define (index-max-range idx)
  (if (zero? (index-size idx))
      -1
      (apply-index-op idx 3)))

;; XXX: what if fixnums represented simple linear indexes with
;;      stride = 1 and offset = 0 ?
(define (index-compact? idx)
  (and (linear-index? idx)
       (zero? (linear-index-offset idx))
       (let ([s (linear-index-stride idx)])
         (or (= 1 s) (= 0 s)))))

;; XXX in-indices should probably work on indexes, series, and
;;     dataframes (better ergonomics)
;; XXX this name is bad
(define in-indices-sequence
  (procedure-rename
   (λ (idx)
     (in-range (index-size idx)))
   'in-indices))

;; XXX this name is bad
(define-sequence-syntax in-indices
  (λ () #'in-indices-sequence)
  (syntax-parser
    [(v:id (in-indices an-index)) #'(v (in-range (index-size an-index)))]
    [_ #f]))

(define (index-pick idx i*)
  (index-extract idx #:indices (sort i* <)))

(define (index-select idx pred?)
  (index-extract idx #:select pred?))

(define (index-extract idx
                       #:indices [i* (in-indices idx)]
                       #:select [pred? (λ (i) #t)]
                       #:transform [xform values])
  (make-vector-index
   (unsafe-vector*->immutable-vector!
    (for/vector ([i i*] #:when (pred? i))
      (xform (index-ref idx i))))))

(define (index-slice i start [size (- (index-size i) start)])
  (index-compose i (make-linear-index size start)))

(define (index-reverse idx)
  (define n (index-size idx))
  (index-compose idx (make-linear-index n (sub1 n) -1)))

(define (index-sort idx lt?)
  (make-vector-index
   (list->vector
    (sort (sequence->list (in-indices-sequence idx)) lt?))))

(define (do-generic-index-compose i0 i1)
  (define vec (make-vector (index-size i1)))
  (for/fold ([max-range -1] #:result (values max-range vec))
            ([i (in-indices i1)])
    (define v (index-ref i0 (index-ref i1 i)))
    (vector-set! vec i v)
    (max max-range v)))

(define (generic-index-compose i0 i1)
  ;; XXX: small indexes (size 0-2) could trivially be converted to
  ;;     linear-indexes, but this optimization probably wouldn't occur much in
  ;;     practice.
  ;; do-generic-index-compose must return a fresh vector
  (define-values (max-range vec)
    (do-generic-index-compose i0 i1))
  (vector-index max-range
                (unsafe-vector*->immutable-vector! vec)))

(define (linear-index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (+ (linear-index-offset idx)
     (* (linear-index-stride idx) i)))

(define (do-linear-index-compose i0 i1)
  (define o0 (linear-index-offset i0))
  (define s0 (linear-index-stride i0))
  (define o1 (linear-index-offset i1))
  (define s1 (linear-index-stride i1))
  (values (index-size i1)
          (+ o0 (* s0 o1))
          (* s0 s1)))

(define (linear-index-compose i0 i1)
  (cond
    [(not (linear-index? i1)) (generic-index-compose i0 i1)]
    [else
     (call-with-values
      (λ () (do-linear-index-compose i0 i1))
      make-linear-index)]))

(define (linear-index-max-range idx)
  (+ (linear-index-offset idx)
     (* (linear-index-stride idx) (linear-index-size idx))))

(struct linear-index (size offset stride)
  #:transparent
  #:property prop:index
  (vector linear-index-ref
          (λ (idx) (linear-index-size idx))
          linear-index-compose
          linear-index-max-range))

(define (make-linear-index size [offset 0] [stride 1])
  (linear-index size offset stride))

(define (vector-index-ref idx i)
  (check-index-bounds 'index-ref idx i)
  (vector-ref (vector-index-mapping idx) i))

(define (vector-index-size idx)
  (vector-length (vector-index-mapping idx)))

(define (-vector-index-max-range idx)
  (vector-index-max-range idx))

(struct vector-index (max-range mapping)
  #:transparent
  #:property prop:index
  (vector vector-index-ref
          vector-index-size
          #f
          -vector-index-max-range))

(define (make-vector-index vec)
  (vector-index (if (zero? (vector-length vec))
                    -1
                    (vector-argmax values vec))
                (vector->immutable-vector vec)))
