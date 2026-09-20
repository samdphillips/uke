#lang racket/base

(require uke/column
         uke/dataframe
         (submod uke/private/dataframe for-dataframe-write)
         uke/index
         uni-table)

(provide show
         default-show-rows)

(define (dataframe->table nrows df)
  (define idx (dataframe-index df))
  (define cols (dataframe-columns df))
  (cons
   (for/list ([col (in-list cols)])
     (column-name col))
   (for/list ([n (in-range nrows)]
              [i (in-range (index-size idx))])
     (for/list ([col (in-list cols)])
       (column-render-cell col (column-ref col i))))))

(define (show df #:nrows [nrows (default-show-rows)] #:widths [widths null])
  (define table (dataframe->table nrows df))
  (print-uni-table table
                   #:col-widths
                   (map list widths)
                   #:table-border
                   '(heavy solid)
                   #:row-align
                   '((wrap))
                   #:col-borders '((light))
                   #:row-borders '((light))))

(define default-show-rows (make-parameter 10))

(define (dataframe-write df outp mode)
  (parameterize ([current-output-port outp])
    (show df)))

(install-dataframe-write! dataframe-write)
