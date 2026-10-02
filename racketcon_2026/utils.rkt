#lang racket

(require uke
         file/gunzip
         gregor)

(provide (all-defined-out))

(define (dataframe-column-rename df renames)
  (define (do-rename1 old-name)
    (match (assq old-name renames)
      [(cons _ new-name) new-name]
      [#f old-name]))
  (define (do-rename* cols)
    (for/list ([col (in-list cols)])
      (column-name-update col do-rename1)))
  (dataframe-column*-update df do-rename*))

(define (dataframe-column-rename-append df s)
  (define renames
    (for/list ([orig-name (in-list (dataframe-column-names df))])
      (cons orig-name (string->symbol (~a orig-name s)))))
  (dataframe-column-rename df renames))

(define (dataframe-sort df col-name lt?)
  (define col (dataframe-column*-ref df col-name))
  (define (sort-index dfi)
    (define (cell-ref i)
      (dataframe-cell-ref* dfi col i))
    (index-sort dfi (λ (i j) (lt? (cell-ref i) (cell-ref j)))))
  (dataframe-index-update df sort-index))

(define ((make-coding-reader coder [who #f]) input)
  (define-values (from-pipe to-pipe) (make-pipe))
  (define err #f)
  (define (save-err e) (set! err e))
  (define (do-read buf)
    (when err
      (raise err))
    (define read-size
      (read-bytes! buf from-pipe))
    read-size)
  (define (do-close)
    (close-input-port from-pipe)
    (kill-thread thd)
    (close-input-port input))
  (define thd
    (thread
     (lambda ()
       (with-handlers ([exn:fail? save-err])
         (coder input to-pipe)
         (close-output-port to-pipe)))))
  (make-input-port who do-read #f do-close))

(define gunzip-port (make-coding-reader gunzip-through-ports 'gunzip-port))

(define (gz-file->lines gz-file)
  (call-with-input-file gz-file
    (λ (inp-gz) (port->lines (gunzip-port inp-gz)))))

(error-print-width 160)

(define apache-log-line-pat
  #px"^(\\S+) \\S+ (\\S+) \\[([^]]+)\\] \"([^\"]+)\" (\\d+) (\\d+|-) \"([^\"]*)\" \"([^\"]*)\"")

(define http-request-pat #px"^(\\S+) (\\S+)")

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

;; https://www.movable-type.co.uk/scripts/gis-faq-5.1.html
;; Earth radius assuming sphere of WGS84 ellipsoid
(define Rkm 6371.009)
(define (great-circle-distance lt0 ln0 lt1 ln1)
  (define rlt0 (degrees->radians lt0))
  (define rln0 (degrees->radians ln0))
  (define rlt1 (degrees->radians lt1))
  (define rln1 (degrees->radians ln1))

  (define dlt (- rlt1 rlt0))
  (define dln (- rln1 rln0))
  (define a
    (+ (expt (sin (/ dlt 2)) 2)
       (* (cos rlt0)
          (cos rlt1)
          (expt (sin (/ dln 2)) 2))))
  (define c
    (* 2 (asin (min 1 (sqrt a)))))
  (* Rkm c))