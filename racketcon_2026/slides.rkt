#lang at-exp slideshow

(require slideshow/code)

(define (string-blank? v)
  (and (string? v)
       (or (string=? "" v)
           (string=? "\n" v))))

(define (tt/d d v)
  (let ([size (+ (or (current-tt-font-size) (current-font-size)) d)])
    (parameterize ([current-tt-font-size size])
      (tt v))))

(slide
 @titlet{Uke: Immutable Dataframes for Racket}
 @t{Sam Phillips})

(slide
 #:title "Me"
 (vc-append
  (hc-append
   (rotate @t{Sysadmin} (/ pi 2))
   (scale-to-fit (bitmap "bio_photo.jpg") (/ client-w 4) (/ client-h 4))
   (rotate (vc-append (t "Engineering") (t "Manager")) (/ pi -2)))
  @t{Machine Data & Observability})
 'next
 (item
  (vl-append
   @tt{bash}
   @tt/d[-10]{< /var/log/syslog grep -i 'ERROR' \}
   @tt/d[-10]{    | awk -F T '{ print $1 }' | uniq -c}))
 'next
 (item
  (vl-append
   @t{Splunk}
   @tt/d[-10]{index="main" ERROR | stats count by host})))

(slide
 #:title "History"
 @t{RacketCon 2021}
 @item{Slicing Tabular Data with Sawzall}
 @item{Qi: A Functional, Flow-Oriented DSL})

(slide
 #:title "Uke Core: Index"
 @item{Generic Functions}
 (subitem (code (index-ref index? nat?) -> nat?))
 (subitem (code (index-size index?) -> nat?))
 (subitem (code (index-compose index? index?)
                -> index?))
 @item{Concrete Types}
 @subitem{@tt{linear-index}: start, size, and stride}
 @subitem{@tt{vector-index}: immutable vector})

(slide
 #:title "Uke Core: Column"
 (item (code name : symbol?))
 (item (code index : index?))
 (item (code store : store? (code:comment "immutable-vector?")))
 'next
 (item (code projection : (or/c #false
                                (any/c -> any))))
 (item (code properties : (hash/c keyword? any/c))))

(slide
 #:title "Uke Core: Dataframe"
 (item (code index : index?))
 (item (code columns : (listof column?))))

(slide
 #:title "Machete"
 @t{Qi extension library for working with Dataframes.}
 (item
  (code (create [dest-column (src-column) qi-floe] ...)))
 (item
  (code (slice spec ...)))
 (item
  (code (where (src-column ...) qi-predicate-floe))))

(slide
 #:title "Demo 1"
 @t{What U.S. Counties with the same names are the closest?})

(slide
 #:title "Demo 2"
 (vl-append
  @t{For syntax}
  (code
   (for/dataframe (column-spec ...) for-clause
     value-body ...))
  (item
   (vl-append
    (hbl-append (code column-spec) @t{:})
    (hbl-append (code identifier) @t{|} (code [identifier properties ...]))))))

(slide
 #:title "Demo 2"
 @t{Some web log analytics})

(slide
 #:title "Future work"
 @item{Improve ergonomics}
 @item{Handle N/A values}
 @item{Rhombus support}
 @item{Uke 2})

#|
* Introduction
  * Me
    * Stuff I do
    * Splunk etc
  * Uke (brief)
* History
  * Racket Con 2021
    * Qi and Sawzall
* Demo 1
  * Counties
* Design 1
  * Core
    * Dataframe
    * Column
    * Index
    * Store
  * Builds on uni-table and Qi
* Demo 2
  * Server logs
* Design 2
  * Machete
    * Layered on Qi
* Future
  * Ergonomics
  * N/A values
  * Optimizations
  * Rhombus
  * Uke2
|#