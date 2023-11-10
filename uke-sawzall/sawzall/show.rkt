#lang racket/base

(require uke/dataframe
         uke/index
         uke/series
         uni-table)

(provide show)

(define (show df #:nrows [nrows 10] #:widths [widths '(35)])
  (define table
    (let ()
      (define idx (dataframe-index df))
      (define series (dataframe-series df))
      (cons
       (for/list ([col (in-list series)])
         (series-name col))
       (for/list ([n (in-range nrows)]
                  [i (in-range (index-size idx))])
         (for/list ([col (in-list series)])
           (series-ref col i))))))
  (print-uni-table table
                   #:col-widths
                   (map list widths)
                   #:table-border
                   '(heavy solid)
                   #:row-align
                   '((wrap))
                   #:col-borders '((light))
                   #:row-borders '((light))))
