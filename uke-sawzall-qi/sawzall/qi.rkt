#lang racket/base

(require (for-syntax syntax/parse)
         (prefix-in s: uke/sawzall)
         qi)

(provide (for-space qi create slice where)
         (rename-out [s:show show]))

(begin-for-syntax
  (define-syntax-class create-column-spec
    (pattern [name:id (binder:id ...) flow])))

(define-qi-syntax-rule (create column:create-column-spec ...)
  (esc (λ (df)
         (s:create df
                   [column.name
                    (column.binder ...)
                    (~> (column.binder ...) column.flow)]
                   ...))))

(define-qi-syntax-rule (slice s)
  (esc (λ (df) (s:slice df s))))

(define-qi-syntax-rule (where (binder:id ...) flow)
  (esc (λ (df)
         (s:where df (binder ...) (~> (binder ...) flow)))))