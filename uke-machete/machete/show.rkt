#lang racket/base

(require uke/column
         uke/dataframe
         uke/index
         uni-table)

(provide show)

(define (show df #:nrows [nrows 10] #:widths [widths null])
  (define table
    (let ()
      (define idx (dataframe-index df))
      (define cols (dataframe-columns df))
      (cons
       (for/list ([col (in-list cols)])
         (column-name col))
       (for/list ([n (in-range nrows)]
                  [i (in-range (index-size idx))])
         (for/list ([col (in-list cols)])
           (column-render-cell col (column-ref col i)))))))
  (print-uni-table table
                   #:col-widths
                   (map list widths)
                   #:table-border
                   '(heavy solid)
                   #:row-align
                   '((wrap))
                   #:col-borders '((light))
                   #:row-borders '((light))))
