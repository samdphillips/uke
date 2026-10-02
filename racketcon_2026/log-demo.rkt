#lang racket

(require uke
         uke/machete
         uke/machete/show
         gregor
         "utils.rkt")

#;
(define df-log-raw
  (for*/dataframe ([raw-logs #:render ~.s])
    ([fname (directory-list "fewmets-logs" #:build? #t)]
     [line (gz-file->lines fname)])
    line))

#;
(define df-log
  (for*/dataframe ([src #:render-width 20]
                   [datetime
                    #:render-width 26
                    #:render moment->iso8601/tzid]
                   request-method request-path response-code
                   response-size referer
                   [user-agent #:render-width 30])
    ([fname (directory-list "fewmets-logs" #:build? #t)]
     [line (gz-file->lines fname)])
    (match line
      [(pregexp apache-log-line-pat
                (list _ src _user datetime request response-code
                      response-size referer user-agent))
       (match-define
         (or (pregexp http-request-pat (list _ request-method request-path))
             (app (λ (_) (values #f #f)) request-method request-path))
         request)
       (values src
               (convert-apache-log-time datetime)
               request-method request-path
               response-code
               (string->number response-size)
               referer
               user-agent)]
      [_
       (values #f #f #f #f #f #f #f)])))

;; top 404 error requests
#;
(define df-not-found
  ;; get-response-code : nonnegative-integer? -> string?
  (let* ([get-response-code (dataframe-column-lift df-log values 'response-code)])
    (dataframe-select df-log (λ (i) (equal? "404" (get-response-code i))))))

#;
(define df-not-found
  (~> (df-log)
      (where (response-code) (equal? "404"))))

;; same stores
#;#;#;
(dataframe-num-rows df-log)
(dataframe-num-rows df-not-found)

(eq? (column-store (dataframe-column*-ref df-log 'response-code #f values))
     (column-store (dataframe-column*-ref df-not-found 'response-code #f values)))

;; (classify-user-agent s) -> (or/c 'bot 'tablet 'mobile 'desktop)
#;
(define df-not-found-count-by-type
  (~> (df-not-found)
      (group (user-agent)
             #:key classify-user-agent
             #:aggregate dataframe-num-rows)
      (dataframe-column-rename '([key . user-type] [groups . count]))))

;; "real" users page visits
#;
(define df-page-visits
  (~> (df-log)
      (where (response-code) (~> (regexp-match? #px"[45].." _) NOT))
      (where (user-agent) (~> classify-user-agent (equal? 'desktop)))
      (create
       [request-path (request-path) normalize-url])
      (group (request-path) #:key values #:aggregate dataframe-num-rows)
      (dataframe-column-rename '([key . request-path] [groups . count]))
      (dataframe-sort 'count >)))

;; top time of day visitors
#;
(define-values (df-log-hour-visits df-log-dow-visits)
  (~> (df-log)
      (create [hour (datetime) ->hours]
              [dow (datetime) ->wday])
      (-< (~> (group (hour) #:key values #:aggregate dataframe-num-rows)
              (dataframe-column-rename '([key . hour] [groups . count]))
              (dataframe-sort 'hour <))
          (~> (group (dow) #:key values #:aggregate dataframe-num-rows)
              (dataframe-column-rename '([key . dow] [groups . count]))
              (dataframe-sort 'dow <)))))