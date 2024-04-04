#lang racket

(provide make-environment
         lookup
         extend
         extend*)

(require syntax/id-table)

(define make-environment make-immutable-bound-id-table)
(define lookup bound-id-table-ref)
(define extend bound-id-table-set)

(define (extend* ρ id-list v-list)
  (for/fold ([ρ′ ρ])
            ([id (in-syntax id-list)]
             [v  (in-list v-list)])
    (extend ρ′ id v)))

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