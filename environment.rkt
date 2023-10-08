#lang racket

(provide make-empty-environment
         environment-set
         environment-ref
         environment<=?
         environment-lub)

(require racket/hash
         "ordering.rkt")

(struct environment (hash)
        #:methods gen:dict
        [(define (dict-ref dict key
                           [default (lambda () (error))])
           (environment-ref dict key default))
         (define (dict-set dict key val)
           (environment-set dict key val))]
        #:methods gen:dcpo
        [(define (gen-<=? dcpo other)
           (if (environment? other) (environment<=? dcpo other) #f))
         (define (gen-lub dcpo other)
           (if (environment? other) (environment-lub dcpo other) T))])

(define (make-empty-environment)
  (environment (hasheq)))

(define (coerce-symbol v)
  (if (identifier? v) (syntax-e v) v))

(define (environment-set env id v)
  (unless (or (identifier? id) (symbol? id))
    (raise-argument-error 'environment-set "(or/c identifier? symbol?)" id))
  (define hash (environment-hash env))
  (environment (hash-set hash (coerce-symbol id) v)))

(define (environment-ref env id . maybe-failure-result)
  (unless (or (identifier? id) (symbol? id))
    (raise-argument-error 'environment-set "(or/c identifier? symbol?)" id))
  (define hash (environment-hash env))
  (apply hash-ref hash (coerce-symbol id) maybe-failure-result))

(define (environment<=? env1 env2)
  (define hash1 (environment-hash env1))
  (define hash2 (environment-hash env2))
  (define vars (set-union (hash-keys hash1) (hash-keys hash2)))
  (for/and ([var (in-list vars)])
    (<=? (hash-ref hash1 var ⊥)
         (hash-ref hash2 var ⊥))))

(define (environment-lub env1 env2)
  (define hash1 (environment-hash env1))
  (define hash2 (environment-hash env2))
  (environment (hash-union hash1 hash2 #:combine lub)))
