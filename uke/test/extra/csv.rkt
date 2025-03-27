#lang racket/base

(require rackunit
         racket/match
         racket/port
         uke
         uke/machete
         uke/csv
         uke/test/b2t2-tables)

(define basic-csv
  #<<CSV
name,age,color
Bob,12,blue
Alice,17,green
Eve,13,red
CSV
  )

;; students/age column is numbers
;; csv column will be strings unless converted
(define students2
  (~> (students)
      (create [age (age) number->string])))

(test-case "basic csv - with extracted column names"
  (define df
    (call-with-input-string basic-csv
      csv->dataframe ))

  (check-true (column=? (dataframe-column-ref df 'name)
                        (dataframe-column-ref students2 'name))
              "name column same")
  (check-true (column=? (dataframe-column-ref df 'age)
                        (dataframe-column-ref students2 'age))
              "age column same")
  (check-true (column=? (dataframe-column-ref df 'color)
                        (dataframe-column-ref students2 'color))
              "color column same"))

(test-case "basic csv - with external column names"
  (define df
    (call-with-input-string basic-csv
      (λ (in)
         (dataframe-slice
          (csv->dataframe in #:column-names '(nombre anos colores)) 1))))

  ;; Rename columns from the students2 data set so the test can just use
  ;; `column=?`
  (define students3
    (dataframe-column*-update students2
                              (λ (col*)
                                (for/list ([col (in-list col*)])
                                  (define (rename col-name)
                                    (case col-name
                                      [(name) 'nombre]
                                      [(age) 'anos]
                                      [(color) 'colores]))
                                  (column-name-update col rename)))))

  (check-true (column=? (dataframe-column-ref df 'nombre)
                        (dataframe-column-ref students3 'nombre))
              "name column same")
  (check-true (column=? (dataframe-column-ref df 'anos)
                        (dataframe-column-ref students3 'anos))
              "age column same")
  (check-true (column=? (dataframe-column-ref df 'colores)
                        (dataframe-column-ref students3 'colores))
              "color column same"))

(test-case "basic csv - with generated column names"
  (define df
    (call-with-input-string basic-csv
      (λ (in)
        (dataframe-slice
         (csv->dataframe in #:column-names #f) 1))))

  (match-define (list name-col age-col color-col)
    (for/list ([col (dataframe-column* df)]) (column-name col)))

  ;; Rename columns from the students2 data set so the test can just use
  ;; `column=?`
  (define students3
    (dataframe-column*-update students2
                              (λ (col*)
                                (for/list ([col (in-list col*)])
                                  (define (rename col-name)
                                    (case col-name
                                      [(name) name-col]
                                      [(age) age-col]
                                      [(color) color-col]))
                                  (column-name-update col rename)))))

  (check-true (column=? (dataframe-column-ref df name-col)
                        (dataframe-column-ref students3 name-col))
              "name column same")
  (check-true (column=? (dataframe-column-ref df age-col)
                        (dataframe-column-ref students3 age-col))
              "age column same")
  (check-true (column=? (dataframe-column-ref df color-col)
                        (dataframe-column-ref students3 color-col))
              "color column same"))