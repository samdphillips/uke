#lang racket/base

(require racket/unsafe/ops
         racket/vector
         "index.rkt"
         "store.rkt"
         "util.rkt")

(provide series?
         series-name
         series-index
         series-store
         series-ref
         series-push-index
         series-inspect
         ->series
         hash->series
         vector->series
         sequence->series)

(define (series-inspect s)
  (list 'series
        (cons 'name (series-name s))
        (index-inspect (series-index s))
        (cons 'store (series-store s))))

(struct series (name index store) #:transparent)

(define (series-ref a-series i)
  (store-ref (series-store a-series) (index-lookup (series-index a-series) i)))

(define (->series name seq)
  (cond
    [(hash? seq)   (hash->series name seq)]
    [(vector? seq) (vector->series name seq)]
    [else (sequence->series name seq)]))

(define (hash->series name hsh)
  (define store (make-vector (hash-count hsh)))
  (define index
    (for/fold ([index (hash)])
              ([(k v) (in-hash hsh)]
               [i (in-naturals)])
      (vector-set! store i v)
      (hash-set index k i)))
  (series name index (vector->immutable-vector store)))

;; XXX An index could be used to just use subset of the elements?  Also it
;; could handle a slice?  Probably better than current interface.
(define (vector->series name a-vector [size #f])
  (define series-size   (or size (vector-length a-vector)))
  (define series-vector
    (if (immutable? a-vector)
        a-vector
        (unsafe-vector*->immutable-vector!
          (if size
              (vector-copy a-vector 0 size)
              (vector-copy a-vector)))))
  (series name (seq-identity-index series-size) series-vector))

(define (sequence->series name seq)
  (define-values (store len)
    (sequence->list/length seq))
  (series name (seq-identity-index len) (list->immutable-vector store)))

(define (series-push-index a-series an-index)
  (define new-index (index-compose an-index (series-index a-series)))
  (struct-copy series a-series [index new-index]))
