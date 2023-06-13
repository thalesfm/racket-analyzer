#lang racket

(provide base-type?
         type-ctor?
         define-base-type
         define-type-constructor
         type-out)

(require racket/provide-syntax
         syntax/parse/define
         (for-syntax racket/syntax))

(struct base-type (name) #:transparent)
(struct type-ctor (name args) #:transparent)

(define-for-syntax (format-pred-id id)
  (format-id id "~a?" (syntax-e id)))

(define-for-syntax (stx-length=? stx n)
  (eq? (length (syntax->list stx)) n))

(define-syntax-parser define-base-type
  [(_ type:id)
   #:with pred (format-pred-id #'type)
   #'(begin
       (define type (base-type 'type))
       (define (pred v) (eq? v type)))])

(define-syntax-parser define-type-constructor
  [(_ ctor:id #:arity arity:exact-positive-integer)
   #:with pred (format-pred-id #'ctor)
   #'(begin
       (define-match-expander ctor
         (syntax-parser
           [(_ pat:expr (... ...))
            #:fail-when (not (stx-length=? #'(pat (... ...)) arity))
                        "arity mismatch"
            #'(type-ctor 'ctor (list pat (... ...)))])
         (syntax-parser
           [(_ arg:expr (... ...))
            #:fail-when (not (stx-length=? #'(arg (... ...)) arity))
                        "arity mismatch"
            #'(type-ctor 'ctor (list arg (... ...)))]))
       (define (pred v)
         (and (type-ctor? v)
              (eq? (type-ctor-name v) 'id))))])

(define-provide-syntax type-out
  (syntax-parser
    [(_ type)
     #:with pred (format-pred-id #'type)
     #'(combine-out type pred)]))
