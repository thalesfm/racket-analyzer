#lang racket/base

(provide
 (rename-out
  [make-immutable-bound-id-table make-environment]
  [bound-id-table-ref environment-ref]
  [bound-id-table-set environment-set]))

(require syntax/id-table)

;; TODO: Implement `environment-union`
#|
(define (environment-union ρ ρ′ #:combine combine)
  (for/fold ([ρ″ (make-environment)])
            ([id (in-list (dict-keys ρ))])
    (environment-set
      ρ″
      (if (dict-hash-key? ρ′ id)
          (combine )

    (define d  (force (environment-ref ρ  id)))
    (define d′ (force (environment-ref ρ′ id ⊥)))
    (define d″ (recur-proc d d′))
    (environment-set ρ″ id d″)))
|#