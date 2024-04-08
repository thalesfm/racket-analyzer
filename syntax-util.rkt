#lang racket

(provide var const primop)

(require syntax/parse)

(define (identifier-binding-local? id)
  (eq? (identifier-binding id) 'lexical))

(define (identifier-binding-module? id)
   (list? (identifier-binding id)))

(define-syntax-class var
  (pattern id:id #:when (identifier-binding-local? #'id)))

(define-syntax-class const
  #:literals (quote)
  (pattern (quote datum)
           #:attr value (syntax-e #'datum)))

(define-syntax-class primop
  (pattern id:id #:when (identifier-binding-module? #'id)
           #:attr value (namespace-variable-value (syntax-e #'id) #t)))
