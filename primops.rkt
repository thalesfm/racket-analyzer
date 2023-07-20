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
         [Γ (bind Γ (strip-context #'+) (lift +))]
         [Γ (bind Γ (strip-context #'-) (lift -))]
         [Γ (bind Γ (strip-context #'*) (lift *))]
         [Γ (bind Γ (strip-context #'/) (lift /))]
         [Γ (bind Γ (strip-context #'=) (lift =))]
         [Γ (bind Γ (strip-context #'read) (λ () Top))]
         [Γ (bind Γ (strip-context #'error) (λ () Bot))])
    Γ))
