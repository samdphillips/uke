#lang racket/base

(require racket/generic
         racket/unsafe/ops
         "util.rkt")

(provide gen:index
         index?
         index-lookup
         index-indices
         index-sequential?
         index-size
         index-compose

         (struct-out seq-identity-index))

(module+ test
  (require racket/sequence
           racket/set
           rackunit))

(define-generics index
  ;; index-lookup :: Index i -> j
  (index-lookup index i)
  ;; index-indices :: Index -> Sequenceof i
  (index-indices index)
  ;; index-size :: Index -> Integer
  (index-size index)
  ;; index-compose :: Index Index -> Index
  (index-compose index other-index)
  ;; index-sequential? :: Index -> Boolean
  (index-sequential? index)

  #:defaults
  ([immutable-vector?
    (define/generic index-lookup^ index-lookup)

    (define (index-lookup vec i)
      (vector-ref vec i))

    (define (index-indices vec)
      (in-range 0 (vector-length vec)))

    (define (index-size vec)
      (vector-length vec))

    (define (index-compose a-vector an-index)
      (cond
        [(seq-identity-index? an-index) a-vector]
        [else
         (define new-pointers
           (for/list ([i (in-vector a-vector)])
             (index-lookup^ an-index i)))
         (apply vector-immutable new-pointers)]))

    (define (index-sequential? vec) #t)]

   [immutable-hash?
    (define/generic index-lookup^ index-lookup)

    (define (index-lookup hsh k)
      (hash-ref hsh k))

    (define (index-indices hsh)
      (in-hash-keys hsh))

    (define (index-size hsh)
      (hash-count hsh))

    (define (index-compose a-hash idx)
      (cond
        [(seq-identity-index? idx) a-hash]
        [else
         (for/hash ([(k v) (in-hash a-hash)])
           (values k (index-lookup^ idx v)))]))

    (define (index-sequential? hsh) #f)]))

(module+ test
  (check-equal? (sequence->list (index-indices (vector-immutable))) null)

  (let ([idx (vector-immutable 3 2 1 0)])
    (check-equal? (sequence->list (index-indices idx)) '(0 1 2 3))
    (check-equal? (index-lookup idx 0) 3)
    (check-equal? (index-lookup idx 1) 2)
    (check-equal? (index-lookup idx 2) 1)
    (check-equal? (index-lookup idx 3) 0))

  (let ([i (vector-immutable 3 2 1 0)]
        [j (vector-immutable 0 1 2 3)])
    (check-equal? (index-compose i i) j)
    (check-equal? (index-compose j j) j))

  (check-equal? (sequence->list (index-indices (hash))) null)
  (let ([idx (hash 'a 0 'b 1 'c 2 'd 3)])
    (check-equal? (for/set ([v (index-indices idx)]) v) (set 'a 'b 'c 'd))
    (check-equal? (index-lookup idx 'd) 3)
    (check-equal? (index-lookup idx 'c) 2)
    (check-equal? (index-lookup idx 'b) 1)
    (check-equal? (index-lookup idx 'a) 0))

  (let ([i (hash 'a 'd 'b 'c 'c 'b 'd 'a)]
        [j (hash 'a 'a 'b 'b 'c 'c 'd 'd)])
    (check-equal? (index-compose i i) j)
    (check-equal? (index-compose j j) j))

  (let ([i (vector-immutable 'a 'b 'c 'd)]
        [j (hash 'a 3 'b 2 'c 1 'd 0)])
    (check-equal? (index-compose i j)
                  (vector-immutable 3 2 1 0))
    (check-equal? (index-compose j i)
                  (hash 'a 'd 'b 'c 'c 'b 'd 'a))))

(struct seq-identity-index (size)
  #:methods gen:index
  [(define/generic index-lookup^ index-lookup)
   (define/generic index-size^   index-size)

   (define (index-lookup id-idx i)
     (unless (and (<= 0 i) (< i (seq-identity-index-size id-idx)))
       (error 'index-lookup "lookup index is out of bounds: ~a" i))
     i)

   (define (index-indices id-idx)
     (in-range (seq-identity-index-size id-idx)))

   (define (index-size id-idx)
     (seq-identity-index-size id-idx))

   (define (index-compose id-idx j)
     (define id-idx-size (seq-identity-index-size id-idx))
     (when (< (index-size^ j) id-idx-size)
       (error 'index-compose
              "indexes are incompatible sizes ~a > ~a"
              id-idx-size
              (index-size^ j)))
     (cond
       [(seq-identity-index? j) id-idx]
       [else
        (unsafe-vector*->immutable-vector!
         (for/vector #:length id-idx-size ([i (in-range id-idx-size)])
           (index-lookup^ j i)))]))

   (define (index-sequential? ii) #t)])

(module+ test
  (let ([idx (seq-identity-index 10)])
    (check-equal? (index-lookup idx 0) 0)
    (check-equal? (index-lookup idx 9) 9)))
