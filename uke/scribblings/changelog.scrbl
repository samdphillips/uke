#lang scribble/manual

@require[(for-label uke uke/machete)]

@title{Changelog}

@section{2026.xx.yy}
Release date: 2026/XX/YY
@itemlist[
 @item{
  Repair result values from @racket[for/dataframe], @racket[store-append], and @racket[store-copy].
  Previously these returned mutable values and not proper @racket[store?] values.}]

@section{2025.4.22}
Release date: 2025/04/22
@itemlist[
 @item{Base level documentation of core API completed}
 @item{Expose @racketmodname{uke/store} library}
 @item{Remove @racket[dataframe-column-lift] inlining}
 @item{Use more efficient projection for @racket[csv->dataframe]}]

@section{2025.3.14}
Release date: 2025/03/14
@itemlist[
 @item{Initial Racket Package release}]
