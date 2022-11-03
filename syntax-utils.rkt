#lang racket

(provide (all-defined-out))

(begin-for-syntax
  (define stop-ids (list #'define #'let #'letrec)))

(define-syntax (expand-expression stx)
  (syntax-case stx ()
    [(_ exp) #`(quote #,(local-expand #'exp 'expression stop-ids))]))

(define-syntax (expand-top-level stx)
  (syntax-case stx ()
    [(_ exp) #`(quote #,(local-expand #'exp 'top-level stop-ids))]))

(define-syntax (expand-module stx)
  (syntax-case stx ()
    [(_ exp) #`(quote #,(local-expand #'exp 'module stop-ids))]))

(define-syntax (expand-module-begin stx)
  (syntax-case stx ()
    [(_ exp) #`(quote #,(local-expand #'exp 'module-begin stop-ids))]))