#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/lib/function-header)
         racket/contract
         syntax/parse/define)

(provide (contract-out
          [rename make-transient transient
                  (-> transient-target? transient?)]
          [persistent! (-> transient? any)])
         prop:transient-target
         resolve-transient
         transient-key
         transient-of/c

         define-transient-proc
         transient-case)

(struct transient (key [subject #:mutable]) #:transparent)

#|

Note: this design is relatively simple.  A possibly better and more general
design would have involve a few more abstract types (implemented as properties)
and the concrete `transient` type.  These types would provide additional
operations for moving to/from a transient type.

|#

(define-values (prop:transient-target transient-target? get-transient-key-ref)
  (make-struct-type-property 'transient-target
                             (λ (field info)
                               (define ref (list-ref info 3))
                               (unless (member field (list-ref info 5))
                                 (error 'prop:transient-subject
                                        "field must be immutable"))
                               (λ (s) (ref s field)))))

(define (transient-of/c ctc)
  (and/c transient?
         (property/c transient-subject ctc)))

(define-syntax-parse-rule (transient-case t v [mut-body ...+] [immut-body ...+])
  #:declare t (expr/c #'transient?)
  #:declare v (expr/c #'transient-target?)
  (let* ([subject v.c]
         [subject-key ((get-transient-key-ref subject) subject)]
         [trans-key (transient-key t.c)])
    (if (eq? subject-key trans-key)
        (begin mut-body ...)
        (begin immut-body ...))))

(define-syntax-parse-rule (define-transient-proc fhead:function-header
                            body ...+)
  #:with (t s . rest) #'fhead.params
  (define fhead.name
    (letrec ([proc
              (case-lambda
                [(t . rest)
                 (set-transient-subject! t
                                         (proc t (transient-subject t) . rest))
                 t]
                [fhead.params body ...])])
      (procedure-rename proc 'fhead.name))))

(define make-transient
  (procedure-rename
   (λ (v) (transient (gensym) v))
   'transient))

(define (persistent! t)
  (define subj (transient-subject t))
  (set-transient-subject! t #f)
  subj)

(define (resolve-transient v)
  (if (transient? v)
      (transient-subject v)
      v))
