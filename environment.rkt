#lang racket

(provide
 environment?
 (contract-out
  [make-empty-environment (-> environment?)]
  [make-base-environment (-> environment?)]
  [bind (-> environment? (or/c symbol? syntax?) any/c void)]
  [rebind! (-> environment? (or/c symbol? syntax?) any/c void)]
  [lookup (-> environment? (or/c symbol? syntax?) any/c)]
))

(struct environment (assoc-lst) #:constructor-name make-environment)

(define (->symbol v)
  (cond
    [(symbol? v) v]
    [(syntax? v) (syntax->datum v)]
    [else (raise-argument-error 'v "(or/c symbol? syntax?)" v)]))

(define (massoc v lst [is-equal? equal?])
  (findf (lambda (p) (is-equal? (mcar p) v)) lst))

(define (make-empty-environment)
  (make-environment null))

(define (make-base-environment)
  (define base-assoc-list
    (list (mcons '+ +)
          (mcons '- -)
          (mcons '* *)
          (mcons '/ /)
          (mcons '= =)))
  (make-environment base-assoc-list))

(define (bind env sym value)
  (define assoc-lst (environment-assoc-lst env))
  (define pair (mcons (->symbol sym) value))
  (make-environment (cons pair assoc-lst)))

(define (rebind! env sym value)
  (define assoc-lst (environment-assoc-lst env))
  (define pair (massoc (->symbol sym) assoc-lst))
  (set-mcdr! pair value))

(define (lookup env sym)
  (define assoc-lst (environment-assoc-lst env))
  (define pair (massoc (->symbol sym) assoc-lst))
  (mcdr pair))
