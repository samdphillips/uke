#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         qi
         uke/dataframe)

(provide (for-space qi group))

(define-qi-syntax-parser group
  [(_group (col-names:id ...)
           {~optional {~seq #:key key-flo}
                      #:defaults
                      ([key-flo #'collect])}
           {~optional {~seq #:aggregate aggr-flo}
                      #:defaults
                      ([aggr-flo #'values])})
   #'(group #:key
            (~> (-< (dataframe-cell-ref _ 'col-names _) ...) key-flo)
            #:aggregate aggr-flo)]
  [(_group #:key key-flo
           {~optional {~seq #:aggregate aggr-flo}
                      #:defaults
                      ([aggr-flo #'values])})
   #'(esc (λ (df)
            (dataframe-group df
                             (λ (i) (~> (df i) key-flo))
                             (flow aggr-flo))))])