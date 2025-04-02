#lang scribble/manual

@(require (for-label racket
                     uke))

@title{uke}
@author[(author+email "Sam Phillips" "samdphillips@gmail.com")]

@(when (equal? ".github/workflows/docs.yml" (getenv "GITHUB_WORKFLOW"))
   @para{@bold{WARNING!}  This documentation is for the development version of
              @racket[uke].  Release documentation is at
              @(let ([x "https://docs.racket-lang.org/uke/index.html"])
                 (link x x)).})

@section{Reference}

@defmodule[uke #:no-declare]

Contains all bindings from @racketmodname[uke/dataframe],
@racketmodname[uke/column], and @racketmodname[uke/index].

@subsection{Dataframe}
@defmodule[uke/dataframe #:no-declare]
@declare-exporting[uke/dataframe uke]

@defproc[(dataframe? [v any/c]) boolean?]

@defproc[(make-dataframe [cols (listof column?)]
                         [#:index index index?])
         dataframe?]

@defproc[(dataframe-num-rows [df dataframe?]) nonnegative-integer?]

@defproc[(dataframe-index-update [df dataframe?]
                                 [update/f (-> index? index?)])
         dataframe?]

@defproc[(dataframe-column*-update
          [df dataframe?]
          [update/f (-> (listof column?) (listof column?))])
         dataframe?]

@defproc[(dataframe-columns [df dataframe?]) (listof column?)]

@defproc[(dataframe-column-ref [df dataframe?]
                               [col-name symbol?]
                               [fail (-> any)]) any]

@defproc[(dataframe-column*-ref [df dataframe?]
                                [col-name symbols?]
                                [fail (-> any)]
                                [success (-> column? any)]) any]

@defproc[(dataframe-column-lift [df dataframe?]
                                [combine (-> any/c ... any)]
                                [col-name symbol?] ...)
         (-> nonnegative-integer? any)]

@defproc[(dataframe-add-column* [df dataframe?]
                                [cols column?] ...)
         dataframe?]

@defproc[(dataframe-remove-column* [df dataframe?]
                                   [col-name symbol?] ...)
         dataframe?]

@defproc[(dataframe-reorder-column [df dataframe?]
                                   [col-names (listof symbol?)])
         dataframe?]

@defproc[(dataframe-reverse-rows [df dataframe?]) dataframe?]

@defproc[(dataframe-compact? [df dataframe?]) boolean?]

@defproc[(dataframe-compact [df dataframe?]) dataframe?]

@defproc[(dataframe-select [df dataframe?]
                           [choose? (-> nonnegative-integer? boolean?)])
         dataframe?]

@defproc[(dataframe-slice [df dataframe?]
                          [start nonnegative-integer?]
                          [size nonnegative-integer?
                                (- (dataframe-num-rows df) start)])
         dataframe?]

@defproc[(dataframe-group-index [df dataframe?]
                                [key/f (-> nonnegative-integer? any/c)])
         (hash/c any/c (listof nonnegative-integer?))]

@defproc[(dataframe-group [df dataframe?]
                          [key/f (-> nonnegative-integer? any/c)]
                          [aggr/f (-> dataframe? any/c) values])
         dataframe?]

@defproc[(dataframe-left-join [dfl dataframe?]
                              [keyl (-> nonnegative-integer? any/c)]
                              [removel (listof symbol?)]
                              [dfr dataframe?]
                              [keyr (-> nonnegative-integer? any/c)]
                              [remover (listof symbol?)])
         dataframe?]
ss
@defproc[(dataframe-cell-ref [df dataframe?]
                             [col-name symbol?]
                             [i nonnegative-integer?]) any/c]

@defproc[(dataframe-cell-ref* [df-index index?]
                              [col-name symbol?]
                              [i nonnegative-integer?]) any/c]

@deftogether[
  [@defform[(for/dataframe (column ...+) (for-clause ...)
              body-or-break ... body)]
   @defform[(for*/dataframe (column ...+) (for-clause ...)
              body-or-break ... body)
             #:grammar [(column column-name
                                (column-name column-property ...))
                        (column-property
                         (code:line property-name property-value))]]]]

@defform[(row-df (column ...+) elems ...)]

@defform[(column-df (column elems ...) ...+)]

@subsection{Column}
@defmodule[uke/column #:no-declare]
@declare-exporting[uke/column uke #:packages ("uke-lib")]

@defproc[(column? [v any/c]) boolean?]

@defproc[(make-column [name symbol?]
                      [index index?]
                      [store store?]
                      [#:properties props (hash/c keyword? any/c) (hash)]
                      [#:projection proj (or/c (-> any/c any/c) #f) #f]) any]

@defproc[(column=? [col1 column?]
                   [col2 column?])
         boolean?]

@defproc[(column*=? [idx1 index?]
                    [col1 column?]
                    [idx2 index?]
                    [col2 column?])
         boolean?]

@defproc[(column-name [column column?]) symbol?]

@defproc[(column-size [column column?]) nonnegative-integer?]

@defproc[(column-index [column column?]) index?]

@defproc[(column-properties [column column?]) (hash/c keyword? any/c)]

@defproc[(column-projection [column column?]) (or/c (-> any/c any/c) #f)]

@defproc[(column-store [column column?]) store?]

@defproc[(column-name-update [column column?]
                             [update-name (-> symbol? symbol?)])
         column?]

@defproc[(column-index-update [column column?]
                              [update-index (-> index? index?)])
         column?]

@defproc[(column-properties-update [column column?]
                                   [update-properties
                                    (-> (hash/c keyword? any)
                                        (hash/c keyword? any))])
         column?]

@defproc[(column-projection-update [column column?]
                                   [update-projection
                                    (-> (or/c (-> any/c any/c) #f)
                                        (or/c (-> any/c any/c) #f))])
         column?]

@defproc[(column-ref [column column?]
                     [i nonnegative-integer?])
         any/c]

@defproc[(column-property-ref [column column?]
                              [property-name keyword?]
                              [default failure-result/c #f])
         any/c]

@defproc[(column-push-index [column column?]
                            [index index?])
         column?]

@defproc[(column-compact? [column column?]) boolean?]

@defproc[(column-compact [column column?]) column?]

@defproc[(column-slice [column column?]
                       [start nonnegative-integer?]
                       [size nonnegative-integer? (- (column-size col) start)])
         columns?]

@defproc[(column-render-cell [column column?]
                             [v any/c]) string?]

@subsubsection{More Column Constructors}

@defproc[(build-column [name symbol?]
                       [size nonnegative-integer?]
                       [build (-> nonnegative-integer? any/c)]
                       [#:properties props (hash/c keyword? any/c) (hash)]
                       [#:projection proj (or/c (-> any/c any/c) #f) #f])
         column?]

@defproc[(->column [name symbol?]
                   [sequence sequence?]
                   [#:properties props (hash/c keyword? any/c) (hash)]
                   [#:projection proj (or/c (-> any/c any/c) #f) #f])
         column?]

@defproc[(vector->column [name symbol?]
                         [vec vector?]
                         [#:properties props (hash/c keyword? any/c) (hash)]
                         [#:projection proj (or/c (-> any/c any/c) #f) #f])
         column?]

@defproc[(sequence->column [name symbol?]
                           [sequence sequence?]
                           [#:properties props (hash/c keyword? any/c) (hash)]
                           [#:projection proj (or/c (-> any/c any/c) #f) #f])
         column?]

@subsection{Index}
@defmodule[uke/index #:no-declare]
@declare-exporting[uke/index uke #:packages ("uke-lib")]

@defproc[(index? [v any/c]) boolean?]

@defproc[(index-size [idx index?]) nonnegative-integer?]

@defproc[(index-compose [i1 index?]
                        [i2 index?] ...) index?]

@defproc[(index-max-range [idx index?]) nonnegative-integer?]

@defproc[(index-compact?) any/c]

@defproc[(index-ref [idx index?]
                    [nonnegative-integer? i])
         nonnegative-integer?]

@defproc[(in-indices [idx index?]) (sequence/c nonnegative-integer?)]

@defproc[(index-pick [idx index?]
                     [i* (sequence/c nonnegative-integer?)])
         index?]

@defproc[(index-select [idx index?]
                       [pred? (-> nonnegative-integer? boolean?)])
         index?]

@defproc[(index-extract [idx index?]
                        [#:indices indices
                         (sequence/c nonnegative-integer?)
                         (in-indices idx)]
                        [#:select select/f
                         (-> nonnegative-integer? boolean?)
                         (λ (i) #true)]
                        [#:transform transform/f
                         (-> nonnegative-integer? nonnegative-integer?)
                         values])
         index?]

@defproc[(index-slice [idx index?]
                      [start nonnegative-integer?]
                      [size nonnegative-integer? (- (index-size idx) start)])
         index?]

@defproc[(index-reverse [idx index?]) index?]

@defproc[(index-sort [idx index?]
                     [lt? (-> nonnegative-integer? nonnegative-integer?
                              boolean?)])
         index?]

@defproc[(vector-index? [v any/c]) boolean?]

@defproc[(make-vector-index [vec vector?]) vector-index?]

@defproc[(linear-index? [v any/c]) boolean?]

@defproc[(make-linear-index [size nonnegative-integer?]
                            [offset nonnegative-integer? 0]
                            [stride integer? 1])
         linear-index?]

@defproc[(linear-index-offset [idx linear-index?]) nonnegative-integer?]

@defproc[(linear-index-stride [idx linear-index?]) integer?]

@subsection{Store}
@defmodule[uke/store #:no-declare]
@declare-exporting[uke/store uke #:packages ("uke-lib")]

@defproc[(store? [v any/c]) boolean?]

@defproc[(store-length [s store?]) nonnegative-integer?]

@defproc[(store-ref [s store?]
                    [i nonnegative-integer?])
         any/c]

@defproc[(store-copy [s store?]) store?]

@defproc[(store-append [s1 store?]
                       [s2 store?])
         store?]

@subsection{Exceptions}
@defmodule[uke/error #:no-declare]
@declare-exporting[uke/error uke #:packages ("uke-lib")]

@deftogether[(@defstruct[(uke:exn exn:fail) [] #:omit-constructor]
               @defstruct[(uke:exn:dataframe uke:exn) [] #:omit-constructor]
               @defstruct[(uke:exn:column uke:exn) [] #:omit-constructor]
               @defstruct[(uke:exn:index uke:exn) [] #:omit-constructor])]

@include-section["changelog.scrbl"]
