#lang racket/base

(require racket/generic
         racket/unsafe/ops
         "error.rkt"
         "util.rkt")

(provide gen:index
         index?
         index-lookup
         index-inspect
         index-indices
         index-sequential?
         index-size
         index-compose

         exn:uke:index?

         (struct-out seq-identity-index))

(module+ test
  (require racket/sequence
           racket/set
           rackunit))

(struct exn:uke:index exn:uke ())

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
    (define/generic ^index-lookup index-lookup)
    (define/generic ^index-size index-size)

    (define (index-lookup a-vector pos)
      (unless (vector-valid-position? a-vector pos)
        (raise-uke-error exn:uke:index
                        'index-lookup
                        "lookup index is out of bounds: ~a"
                        pos))
      (vector-ref a-vector pos))

    (define (index-indices vec)
      (in-range 0 (vector-length vec)))

    (define (index-size vec)
      (vector-length vec))

    (define (index-compose a-vector an-index)
      (unsafe-vector*->immutable-vector!
        (for/vector #:length (vector-length a-vector) ([i (in-vector a-vector)])
          (^index-lookup an-index i))))

    (define (index-sequential? vec) #t)]

   [immutable-hash?
    (define/generic ^index-lookup index-lookup)

    (define (index-lookup a-hash key)
      (or (hash-ref a-hash key #f)
          (raise-uke-error exn:uke:index
                           'index-lookup
                           "lookup key is not in index: ~a"
                           key)))

    (define (index-indices hsh)
      (in-hash-keys hsh))

    (define (index-size hsh)
      (hash-count hsh))

    (define (index-compose a-hash idx)
      (for/hash ([(k v) (in-hash a-hash)])
        (values k (^index-lookup idx v))))

    (define (index-sequential? hsh) #f)]))

(define (index-inspect i)
  (list 'index (cons 'size (index-size i)) i))

(module+ test
  (check-equal? (sequence->list (index-indices (vector-immutable))) null)

  (test-case "vector-immutable index"
    (define idx (vector-immutable 3 2 1 0))
    (check-equal? (sequence->list (index-indices idx)) '(0 1 2 3))
    (check-true (index-sequential? idx))
    (check-equal? (index-size idx) 4)
    (check-equal? (index-lookup idx 0) 3)
    (check-equal? (index-lookup idx 1) 2)
    (check-equal? (index-lookup idx 2) 1)
    (check-equal? (index-lookup idx 3) 0)
    (check-exn exn:uke:index? (lambda () (index-lookup idx -1)))
    (check-exn exn:uke:index? (lambda () (index-lookup idx 4))))

  (test-case "vector/vector index compose"
    (define i (vector-immutable 3 2 1 0))
    (define j (vector-immutable 0 1 2 3))
    (check-equal? (index-compose i i) j)
    (check-equal? (index-compose j j) j))

  (test-case "hash index"
    (define idx (hash 'a 0 'b 1 'c 2 'd 3))
    (check-equal? (index-size idx) 4)
    (check-equal? (for/set ([v (index-indices idx)]) v) (set 'a 'b 'c 'd))
    (check-false (index-sequential? idx))
    (check-equal? (index-lookup idx 'd) 3)
    (check-equal? (index-lookup idx 'c) 2)
    (check-equal? (index-lookup idx 'b) 1)
    (check-equal? (index-lookup idx 'a) 0)
    (check-exn exn:uke:index? (lambda () (index-lookup idx 'e))))

  (test-case "hash/hash index compose"
    (define i (hash 'a 'd 'b 'c 'c 'b 'd 'a))
    (define j (hash 'a 'a 'b 'b 'c 'c 'd 'd))
    (check-equal? (index-compose i i) j)
    (check-equal? (index-compose j j) j))

  (test-case "vector/hash index compose"
    (define i (vector-immutable 'a 'b 'c 'd))
    (define j (hash 'a 3 'b 2 'c 1 'd 0))
    (check-equal? (index-compose i j)
                  (vector-immutable 3 2 1 0))
    (check-equal? (index-compose j i)
                  (hash 'a 'd 'b 'c 'c 'b 'd 'a))))

(struct seq-identity-index (size)
  #:transparent
  #:methods gen:index
  [(define/generic ^index-lookup index-lookup)
   (define/generic ^index-sequential? index-sequential?)
   (define/generic ^index-size index-size)

   (define (index-lookup id-idx pos)
     (unless (and (<= 0 pos) (< pos (seq-identity-index-size id-idx)))
       (raise-uke-error exn:uke:index
                        'index-lookup
                        "lookup index is out of bounds:\n  index-size: ~a\n  position: ~a"
                        (seq-identity-index-size id-idx)
                        pos))
     pos)

   (define (index-indices id-idx)
     (in-range (seq-identity-index-size id-idx)))

   (define (index-size id-idx)
     (seq-identity-index-size id-idx))

   (define (index-compose id-idx j)
     (unless (^index-sequential? j)
       (raise-uke-error exn:uke:index
                        'index-compose
                        "cannot compose non-sequential index with identity index"))
     (define id-idx-size (seq-identity-index-size id-idx))
     (when (< (^index-size j) id-idx-size)
       (raise-uke-error exn:uke:index
                        'index-compose
                        "indexes are incompatible sizes ~a > ~a"
                        id-idx-size
                        (^index-size j)))
     (cond
       [(seq-identity-index? j) id-idx]
       [else
        (unsafe-vector*->immutable-vector!
         (for/vector #:length id-idx-size ([i (in-range id-idx-size)])
           (^index-lookup j i)))]))

   (define (index-sequential? ii) #t)])

(module+ test
  (test-case "seq-identity-index"
    (define idx (seq-identity-index 10))
    (check-true (index-sequential? idx))
    (check-equal? (index-size idx) 10)
    (check-equal? (sequence->list (index-indices idx))
                  '(0 1 2 3 4 5 6 7 8 9))
    (check-equal? (index-lookup idx 0) 0)
    (check-equal? (index-lookup idx 9) 9))

  (test-case "seq-identity-index oob lookup"
    (define idx (seq-identity-index 10))
    (check-exn exn:uke:index? (lambda () (index-lookup idx -1)))
    (check-exn exn:uke:index? (lambda () (index-lookup idx 10)))
    (check-exn exn:uke:index? (lambda () (index-lookup idx 100))))

  (test-case "identity/identity same index compose"
    (define i (seq-identity-index 10))
    (define j (index-compose i i))
    (check-equal? (index-size j) 10))

  (test-case "identity/identity smaller index compose"
    (define i (seq-identity-index 10))
    (define j (seq-identity-index 20))
    (define k (index-compose i j))
    (check-equal? (index-size k) 10)
    (check-exn exn:uke:index? (lambda () (index-compose j i))))

  (test-case "identity/vector index compose"
    (define i (seq-identity-index 3))
    (define j (vector-immutable 2 1 0))

    (define ij (index-compose i j))
    (check-true (index-sequential? ij))
    (check-equal? (index-size ij) 3)
    (check-equal? (for/list ([t (index-indices ij)])
                    (index-lookup ij t))
                  '(2 1 0))

    (define ji (index-compose j i))
    (check-true (index-sequential? ji))
    (check-equal? (index-size ji) 3)
    (check-equal? (for/list ([t (index-indices ji)])
                    (index-lookup ji t))
                  '(2 1 0)))

  (test-case "identity/vector smaller index compose"
    (define i (seq-identity-index 3))
    (define j (vector-immutable 3 2 1 0))

    (define ij (index-compose i j))
    (check-equal? (index-size ij) 3)
    (check-equal? (for/list ([t (index-indices ij)])
                    (index-lookup ij t))
                  '(3 2 1))

    (check-exn exn:uke:index? (lambda () (index-compose j i))))

  (test-case "identity/hash index compose errors"
    (define i (seq-identity-index 3))
    (define j (hash 'a 0 'b 1 'c 2))
    (check-exn exn:uke:index? (lambda () (index-compose i j))))

    )
