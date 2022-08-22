#lang racket

(provide (all-defined-out))

(require (for-syntax racket/syntax syntax/parse))

(define-syntax my-define-values
  (syntax-parser 
    [(my-define-values (id:id ...) expr)
     #:with (my-id ...)
            (map (λ (id) (format-id id "my-~a" (syntax-e id)))
                 (syntax->list #'(id ...)))
     #'(define-values (my-id ...) expr)]))