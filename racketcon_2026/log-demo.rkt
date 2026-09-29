#lang racket

(require uke
         uke/machete/show
         gregor
         "utils.rkt")

(define log-line-pat
  #px"^(\\S+) \\S+ (\\S+) \\[([^]]+)\\] \"([^\"]+)\" (\\d+) (\\d+|-) \"([^\"]*)\" \"([^\"]*)\"")

(define request-pat #px"^(\\S+) (\\S+)")

#;
(define log-lines
  (sequence-fold
   (λ (s fname) (sequence-append s (gz-file->lines fname)))
   null
   (directory-list "fewmets-logs" #:build? #t)))

#;
(define raw-log-column
  (sequence->column 'raw-log
                    log-lines
                    #:properties
                    (hash '#:render ~.s)))
#;
(define df0
  (make-dataframe
   (list raw-log-column)))

#;
df0

#;
(dataframe-num-rows df0)


#;
(define match-column
  (column-projection-update (column-name-update raw-log-column (λ (_) 'match-column))
                            (λ (_f)
                              (λ (s)
                                (define m (regexp-match log-line-pat s))
                                (and m (list->vector (cdr m)))))))

#;
(define df1
  (dataframe-column*-update df0 (λ (cols) (append cols (list match-column)))))

#;
(define match-column
  (column-compact
   (column-projection-update (column-name-update raw-log-column (λ (_) 'match-column))
                             (λ (_f)
                               (λ (s)
                                 (define m (regexp-match log-line-pat s))
                                 (and m (list->vector (cdr m))))))))

#;
(define df1
  (dataframe-column*-update df0 (λ (_cols) (list match-column))))

;#;
(define (convert-apache-log-time s)
  (define (num i j)
    (string->number (substring s i j)))
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define mon
      (match (substring s 3 6)
        ["Jan" 1]
        ["Feb" 2]
        ["Mar" 3]
        ["Apr" 4]
        ["May" 5]
        ["Jun" 6]
        ["Jul" 7]
        ["Aug" 8]
        ["Sep" 9]
        ["Oct" 10]
        ["Nov" 11]
        ["Dec" 12]))
    (define tz
      (* (if (equal? "-" (substring s 21 22)) -60 60)
         (+ (* 60 (num 22 24)) (num 24 26))))
    (moment (num 7 11)
            mon
            (num 0 2)
            (num 12 14)
            (num 15 17)
            (num 18 20)
            #:tz tz)))

#;
(define df1
  (for*/dataframe ([raw-logs #:render ~.s])
    ([fname (directory-list "fewmets-logs" #:build? #t)]
     [line (gz-file->lines fname)])
    line))

(define df1
  (for*/dataframe ([src
                    #:render-width 20]
                   [datetime
                    #:render-width 26
                    #:render moment->iso8601/tzid]
                   request response-code response-size referer
                   [user-agent
                    #:render-width 30])
    ([fname (directory-list "fewmets-logs" #:build? #t)]
     [line (gz-file->lines fname)])
    (match line
      [(pregexp log-line-pat
                (list _ src _user datetime request response-code
                      response-size referer user-agent))
       (values src
               (convert-apache-log-time datetime)
               request
               response-code
               (string->number response-size)
               referer
               user-agent)]
      [_
       (values #f #f #f #f #f #f #f)])))


;; top 404 error requests

(define df2
  ;; get-response-code : nonnegative-integer? -> string?
  (let* ([get-response-code (dataframe-column-lift df1 values 'response-code)])
    (dataframe-select df1 (λ (i) (equal? "404" (get-response-code i))))))

(dataframe-num-rows df1)
(dataframe-num-rows df2)

(eq? (column-store (dataframe-column*-ref df1 'response-code #f values))
     (column-store (dataframe-column*-ref df2 'response-code #f values)))

;; "real" users

;; top time of day visitors