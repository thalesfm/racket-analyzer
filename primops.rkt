#lang racket

(provide make-base-environment)

(require syntax/strip-context
        "environment.rkt"
        "ordering.rkt"
        "types.rkt")

(define (constant? v)
  (if (Pairof? v)
      (and (Literal? (car v))
           (Literal? (cdr v)))
      (Literal? v)))

(define (lift proc)
  (lambda args
    (if (andmap constant? args)
        (datum->type (apply proc (map Literal-value args)))
        T)))

(define (make-base-environment)
  (let* ([Γ (make-empty-environment)]
         [Γ (environment-set Γ (strip-context #'+) (lift +))]
         [Γ (environment-set Γ (strip-context #'-) (lift -))]
         [Γ (environment-set Γ (strip-context #'*) (lift *))]
         [Γ (environment-set Γ (strip-context #'/) (lift /))]
         [Γ (environment-set Γ (strip-context #'=) (lift =))]
         [Γ (environment-set Γ (strip-context #'map) (lift map))]
         [Γ (environment-set Γ (strip-context #'read) (λ () T))]
         [Γ (environment-set Γ (strip-context #'error) (λ () ⊥))])
    Γ))
