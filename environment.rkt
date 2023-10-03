#lang racket

(provide make-empty-environment
         environment-set
         environment-ref
         environment-union)

(require racket/hash)

(define (make-empty-environment)
  (hasheq))

(define (coerce-symbol v)
  (if (identifier? v) (syntax-e v) v))

(define (environment-set env id v)
  (unless (or (identifier? id) (symbol? id))
    (raise-argument-error 'environment-set "(or/c identifier? symbol?)" id))
  (hash-set env (coerce-symbol id) v))

(define (environment-ref env id . maybe-failure-result)
  (unless (or (identifier? id) (symbol? id))
    (raise-argument-error 'environment-set "(or/c identifier? symbol?)" id))
  (apply hash-ref env (coerce-symbol id) maybe-failure-result))

(define (environment-union env1 env2 #:combine combine)
  (hash-union env1 env2 #:combine combine))
