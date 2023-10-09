#lang racket

(provide make-empty-environment
         environment-set
         environment-ref
         environment-keys
         environment<=?
         environment-lub)

(require syntax/id-set
         syntax/id-table
         "common.rkt")

(struct environment (table)
        #:constructor-name make-environment)

(define (make-empty-environment)
  (make-environment (make-immutable-bound-id-table)))

(define (environment-set env id v)
  (define table (environment-table env))
  (make-environment (bound-id-table-set table id v)))

(define (environment-ref env id . maybe-failure-result)
  (define table (environment-table env))
  (apply bound-id-table-ref table id maybe-failure-result))

(define (environment-keys env)
  (bound-id-table-keys (environment-table env)))

(define (environment<=? env1 env2 recur-proc)
  (define ids (set-union (immutable-bound-id-set (environment-keys env1))
                         (immutable-bound-id-set (environment-keys env2))))
  (for/and ([id (in-bound-id-set ids)])
    (recur-proc (environment-ref env1 id ⊥)
                (environment-ref env2 id ⊥))))

(define (environment-lub env1 env2 recur-proc)
  (define ids (set-union (immutable-bound-id-set (environment-keys env1))
                         (immutable-bound-id-set (environment-keys env2))))
  (for/fold ([acc (make-empty-environment)])
            ([id (in-bound-id-set ids)])
    (define v (recur-proc (environment-ref env1 id ⊥)
                          (environment-ref env2 id ⊥)))
    (environment-set acc id v)))
