#lang racket

(provide
 (contract-out
  [environment? (-> any/c boolean?)]
  [make-empty-environment (-> environment?)]
  [make-base-environment (-> environment?)]
  [bind (-> environment? (or/c symbol? syntax?) any/c void)]
  [lookup (-> environment? (or/c symbol? syntax?) any/c)]
))

(define (->symbol v)
  (cond
    [(symbol? v) v]
    [(syntax? v) (syntax->datum v)]
    [else (error)]))

(struct environment (dict) #:constructor-name make-environment)

(define (make-empty-environment)
  (make-environment (hash)))

(define (make-base-environment)
  (make-environment (hash '+ + '- - '* * '/ /)))

(define (bind env sym value)
  (define dict (environment-dict env))
  (make-environment (dict-set dict (->symbol sym) value)))

(define (lookup env sym)
  (define dict (environment-dict env))
  (dict-ref dict (->symbol sym)))
