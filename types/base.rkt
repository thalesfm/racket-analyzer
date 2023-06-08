#lang racket

(provide (struct-out type) define-type type-out)

(require racket/provide-syntax
         (for-syntax racket/syntax syntax/parse))

(struct type (ctor-id args)
        #:transparent)

(define-for-syntax (format-pred-id id)
  (format-id id "~a?" (syntax-e id)))

(define-syntax define-type
  (syntax-parser
    [(_ id:id)
     #:with id? (format-pred-id #'id)
     #'(begin
         (define id (type 'id null))
         (define (id? v) (and (type? v) (eq? (type-ctor-id v) 'id))))]
    [(_ (id:id arg-id:id ...))
     #:with id? (format-pred-id #'id)
     #'(begin
         (define (id arg-id ...) (type 'id (list arg-id ...)))
         (define (id? v) (and (type? v) (eq? (type-ctor-id v) 'id))))]))

(define-provide-syntax type-out
  (syntax-parser
    [(_ type-id)
     #:with type-id? (format-pred-id #'type-id)
     #'(combine-out type-id type-id?)]))
