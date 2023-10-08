#lang racket

(provide make-base-environment)

(require "environment.rkt"
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
         [Γ (environment-set Γ #'+ (lift +))]
         [Γ (environment-set Γ #'- (lift -))]
         [Γ (environment-set Γ #'* (lift *))]
         [Γ (environment-set Γ #'/ (lift /))]
         [Γ (environment-set Γ #'= (lift =))]
         [Γ (environment-set Γ #'map (lift map))]
         [Γ (environment-set Γ #'read (λ () T))]
         [Γ (environment-set Γ #'error (λ () ⊥))])
    Γ))
