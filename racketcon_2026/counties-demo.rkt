#lang racket

;; data from https://simplemaps.com/data/us-counties

(require uke
         uke/machete
         uke/csv
         "utils.rkt")

(default-column-render-cell ~s)

#|

What US counties with the same name are closest?

|#

(define df-all-counties
  (call-with-input-file "uscounties.csv"
    csv->dataframe))

(dataframe-num-rows df-all-counties)

(define df-counties-latlng
  (~> (df-all-counties)

      ;; drop unneeded columns
      (slice
       (not (or county_ascii county_full county_fips state_name population)))
      (create
       [lat (lat) string->number]
       [lng (lng) string->number])))

#|
(define cll-county (dataframe-column-lift df-counties-latlng values 'county))
(cll-county 0)
(cll-county 1)
(cll-county 2)
|#

(define df-counties-latlng-joined
  (let ([df-cll-right (dataframe-column-rename-append df-counties-latlng 2)])
    (dataframe-left-join df-counties-latlng (dataframe-column-lift df-counties-latlng values 'county) null
                         df-cll-right (dataframe-column-lift df-cll-right values 'county2) '(county2))))


(define df-counties-latlng-uniq
  (~> (df-counties-latlng-joined)
      (where (state_id state_id2) (~> equal? NOT))
      (create [dedup-key (county state_id state_id2) (~> list (sort string<?))])
      (create [dup (dedup-key)
                   (esc (let ([seen (mutable-set)])
                          (define (seen? v)
                            (set-member? seen v))
                          (define (add! v)
                            (set-add! seen v))
                          (λ (v)
                            (cond
                              [(seen? v) #t]
                              [else
                               (add! v) #f]))))])
      (where (dup) NOT)
      (slice (not (or dedup-key dup)))))

(define df-counties-closest
  (~> (df-counties-latlng-uniq)
      (create [distance (lat lng lat2 lng2) great-circle-distance])
      (dataframe-sort 'distance <)))

(define df-counties-farthest
  (~> (df-counties-closest)
      dataframe-reverse-rows))
