#lang typed/racket

(require math/distributions)

(require/typed "cities.rkt"
               [cities-temps (Listof (Pairof String Real))])


(define num-cities (length cities-temps))

(: cities-dists (Vectorof (Pairof String Real-Dist)))
(define cities-dists
  (let ([vec : (Vectorof (U #f (Pairof String Real-Dist)))
             (make-vector num-cities #f)])
    (let repeat ([i 0] [ct cities-temps])
      (cond
        [(null? ct) (cast vec (Vectorof (Pairof String Real-Dist)))]
        [else
         (define cur (car ct))
         (vector-set! vec i (cons (car cur) (normal-dist (cdr cur) 10)))
         (repeat (add1 i) (cdr ct))]))))

(: once (-> (Values String Real)))
(define (once)
  (define i (random num-cities))
  (define rec (vector-ref cities-dists i))
  (define t (sample (cdr rec)))
  (values (car rec) t))

(: generate (Integer -> Void))
(define (generate n)
  (for ([i n])
    (define-values (city temp) (once))
    (displayln (~a city ";" (~r #:precision '(= 1) temp)))))

(module* main #f
  (random-seed 12777)
  (generate 1000000000))