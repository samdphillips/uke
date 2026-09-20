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

(define (do-show showf widths tbl)
  (showf tbl
         #:col-widths
         (map list widths)
         #:table-border
         '(heavy solid)
         #:row-align
         '((wrap))
         #:col-borders '((light))
         #:row-borders '((light))))

(module* pict #f
  (require uni-table/pict)
  (provide show-pict)
  (define (show-pict df #:nrows [nrows 10] #:widths [widths null])
    (define tbl (dataframe->table nrows df))
    (do-show uni-table->pict widths tbl)))

(define (show df #:nrows [nrows (default-show-rows)] #:widths [widths null])
  (define tbl (dataframe->table nrows df))
  (do-show print-uni-table widths tbl))

(define default-show-rows (make-parameter 10))

(define (dataframe-write df outp mode)
  (parameterize ([current-output-port outp])
    (show df)))

(install-dataframe-write! dataframe-write)