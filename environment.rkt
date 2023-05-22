#lang racket

(provide
 environment?
 (contract-out
  [make-empty-environment (-> environment?)]
  [make-base-environment (-> environment?)]
  [environment-copy (-> environment? environment?)]
  [bind (-> environment? (or/c symbol? syntax?) any/c void)]
  [bind! (-> environment? (or/c symbol? syntax?) any/c void)]
  [lookup (-> environment? (or/c symbol? syntax?) any/c)]
))

(define (->symbol v)
  (cond
    [(symbol? v) v]
    [(syntax? v) (syntax->datum v)]
    [else (error)]))

(struct environment (dict) #:constructor-name make-environment)

(define (make-empty-environment)
  (make-environment (make-hash)))

(define (make-base-environment)
  (make-environment (make-hash (list (cons '+ +)
                                     (cons '- -)
                                     (cons '* *)
                                     (cons '/ /)
                                     (cons '= =)))))

(define (environment-copy env)
  (make-environment (hash-copy env)))

;; TODO: Highly inneficient, optimize this!!
(define (bind env sym value)
  (define dict (environment-dict env))
  (define copy (hash-copy dict))
  (dict-set! copy (->symbol sym) value)
  (make-environment copy))

(define (bind! env sym value)
  (define dict (environment-dict env))
  (dict-set! dict (->symbol sym) value))

(define (lookup env sym)
  (define dict (environment-dict env))
  (dict-ref dict (->symbol sym)))
