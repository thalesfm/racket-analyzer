#lang racket

(provide make-empty-environment
         environment-set
         environment-ref)

#;(require syntax/id-table)

(define (make-empty-environment)
  (hasheq))

(define (environment-set env id v)
  (unless (identifier? id)
    (raise-argument-error 'environment-set "identifier?" id))
  (hash-set env (syntax-e id) v))

(define ((raise-key-error id))
  (raise-arguments-error 'environment-ref "no value for for id" "id" id))

(define (environment-ref env id [failure-result (raise-key-error id)])
  (unless (identifier? id)
    (raise-argument-error 'environment-set "identifier?" id))
  (hash-ref env (syntax-e id) failure-result))

#;(define (environment-lub env1 env2)
  (hash-union env1 env2 #:combine lub))
