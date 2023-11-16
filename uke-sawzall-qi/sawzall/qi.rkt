#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in s: uke/sawzall)
         qi)

(provide (for-space qi create group-with slice where)
         (rename-out [s:show show]))

(begin-for-syntax
  (define-syntax-class create-column-spec
    (pattern [name:id {~seq property:keyword property-flow} ... (binder:id ...) flow])))

(define-qi-syntax-rule (create column:create-column-spec ...)
  (esc (λ (df)
         (s:create df
                   [column.name
                    {~@ column.property (flow column.property-flow)} ...
                    (column.binder ...)
                    (~> (column.binder ...) column.flow)]
                   ...))))

(define-qi-syntax-rule (group-with series-name:id ...)
  (esc (λ (df) (s:group-with df series-name ...))))

(define-qi-syntax-rule (slice s)
  (esc (λ (df) (s:slice df s))))

(define-qi-syntax-rule (where (binder:id ...) flow)
  (esc (λ (df)
         (s:where df (binder ...) (~> (binder ...) flow)))))