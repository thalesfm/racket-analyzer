#lang racket

(provide
 environment?
 (contract-out
  [make-empty-environment (-> environment?)]
  [bind (-> environment? identifier? any/c environment?)]
  [rebind! (-> environment? identifier? any/c void?)]
  [lookup (-> environment? identifier? any/c)]
  [environment-lub (-> environment? environment? environment?)]
))

(require racket/hash
         "types.rkt")

(define environment? hash?)

(define (make-empty-environment)
  (hash))

(define (bind Γ id v)
  (hash-set Γ (syntax-e id) (box v)))

(define (rebind! Γ id v)
  (set-box! (hash-ref Γ (syntax-e id)) v))

(define (lookup Γ id)
  (define box (hash-ref Γ (syntax-e id) #f))
  (if box (unbox box) Bot))

(define (environment-lub env1 env2)
  (hash-union env1 env2 #:combine lub))
