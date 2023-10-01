#lang racket

(provide make-base-environment)

(require syntax/strip-context
        "environment.rkt"
         "types.rkt")

(define (constant? v)
  (if (pair? v)
      (and (literal-type? (car v))
           (literal-type? (cdr v)))
      (literal-type? v)))

(define (lift proc)
  (lambda args
    (if (andmap constant? args)
        (apply proc args)
        Top)))

(define (make-base-environment)
  (let* ([Γ (make-empty-environment)]
         [Γ (environment-set Γ (strip-context #'+) (lift +))]
         [Γ (environment-set Γ (strip-context #'-) (lift -))]
         [Γ (environment-set Γ (strip-context #'*) (lift *))]
         [Γ (environment-set Γ (strip-context #'/) (lift /))]
         [Γ (environment-set Γ (strip-context #'=) (lift =))]
         [Γ (environment-set Γ (strip-context #'read) (λ () Top))]
         [Γ (environment-set Γ (strip-context #'error) (λ () Bot))])
    Γ))
