#lang racket/base

(require racket/generic
         "index.rkt"
         "util.rkt")

(define-generics series
  ;; series-name :: Series -> String / #f
  (series-name series)
  ;; series-index :: Series -> Index
  (series-index series)
  ;; Series -> Vector
  (series-store series)
  ;; Series K -> V
  (series-ref series k)

  #:fallbacks
  [(define/generic series-index^ series-index)
   (define/generic series-store^ series-store)
   (define (series-ref s i)
     (define j (index-lookup (series-index^ s) i))
     (vector-ref (series-store^ s) j))])

(struct basic-series (name index store)
  #:methods gen:series
  [(define (series-name s) (basic-series-name s))
   (define (series-index s) (basic-series-index s))
   (define (series-store s) (basic-series-store s))])

(define (->series name seq)
  (cond
    [(hash? seq) (->series/hash name seq)]
    [else (->series/seq name seq)]))

(define (->series/hash name hsh)
  (define store (make-vector (hash-count hsh)))
  (define index
    (for/fold ([index (hash)])
              ([(k v) (in-hash hsh)]
               [i (in-naturals)])
      (vector-set! store i v)
      (hash-set index k i)))
  (basic-series name index (vector->immutable-vector store)))

(define (->series/seq name seq)
  (define-values (store len)
    (sequence->list/length seq))
  (basic-series name
                (seq-identity-index len)
                (list->immutable-vector store)))

