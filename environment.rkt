#lang racket

(provide make-empty-environment
         bind
         rebind!
         lookup
         environment-lub)

(require racket/hash
         syntax/id-table
         "types.rkt")

(define (make-empty-environment)
  (make-immutable-free-id-table))

(define (bind Γ id v)
  (dict-set Γ id (box v)))

(define (rebind! Γ id v)
  (set-box! (dict-ref Γ id) v))

(define (lookup Γ id)
  (define box (dict-ref Γ id #f))
  (if box (unbox box) Bot))

(define (environment-lub env1 env2)
  (hash-union env1 env2 #:combine lub))
