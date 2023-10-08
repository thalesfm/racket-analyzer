#lang racket

(provide make-empty-environment
         environment-set
         environment-ref
         environment<=?
         environment-lub)

(require racket/hash
         syntax/id-set
         syntax/id-table
         "domain.rkt")

(struct environment (table)
        #:constructor-name make-environment)

(define (make-empty-environment)
  (make-environment (make-immutable-free-id-table)))

(define (environment-set env id v)
  (define table (environment-table env))
  (make-environment (free-id-table-set table id v)))

(define (environment-ref env id . maybe-failure-result)
  (define table (environment-table env))
  (apply free-id-table-ref table id maybe-failure-result))

(define (environment-keys env)
  (free-id-table-keys (environment-table env)))

(define (environment<=? domain env0 env1)
  (define ids (set-union (immutable-free-id-set (environment-keys env0))
                         (immutable-free-id-set (environment-keys env1))))
  (for/and ([id (in-free-id-set ids)])
    (<=? domain
         (environment-ref env0 id (⊥ domain))
         (environment-ref env1 id (⊥ domain)))))

(define (environment-lub domain env0 env1)
  (define ids (set-union (immutable-free-id-set (environment-keys env0))
                         (immutable-free-id-set (environment-keys env1))))
  (for/fold ([acc (make-empty-environment)])
            ([id (in-free-id-set ids)])
    (environment-set acc id
                     (lub domain
                          (environment-ref env0 id (⊥ domain))
                          (environment-ref env1 id (⊥ domain))))))
