#lang racket

(provide make-empty-environment
         make-captured-environment
         environment-set
         environment-ref)

(define (raise-key-error name id)
  (raise-arguments-error name "no value for for id" "id" id))

(define (make-empty-environment)
  (hasheq))

(define (make-captured-environment env ids failure-result)
  (for/hasheq ([id ids])
    (values id (environment-ref env id failure-result))))

(define (->symbol v)
  (if (identifier? v) (syntax-e v) v))

(define (environment-set env id v)
  (unless (or (identifier? id) (symbol? id))
    (raise-argument-error 'environment-set "(or/c identifier? symbol?)" id))
  (hash-set env (->symbol id) v))

(define (environment-ref env id failure-result)
  (unless (or (identifier? id) (symbol? id))
    (raise-argument-error 'environment-set "(or/c identifier? symbol?)" id))
  (hash-ref env (->symbol id) failure-result))

#;(define (environment-lub env1 env2)
  (hash-union env1 env2 #:combine lub))
