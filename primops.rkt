#lang racket

(provide make-base-environment)

(require "environment.rkt"
         "types.rkt")

(define (constant? v)
  (if (pair? v)
      (and (literal-type? (car v))
           (literal-type? (cdr v)))
      (literal-type? v)))

(define (lift proc)
  (lambda args
    (if (andmap constant? args) (apply proc args) Top)))

(define (make-base-environment)
  (let* ([Γ (make-empty-environment)]
         [Γ (bind Γ #'+ (lift +))]
         [Γ (bind Γ #'+ (lift +))]
         [Γ (bind Γ #'- (lift -))]
         [Γ (bind Γ #'* (lift *))]
         [Γ (bind Γ #'/ (lift /))]
         [Γ (bind Γ #'= (lift =))]
         [Γ (bind Γ #'read (λ () Top))]
         [Γ (bind Γ #'error (λ () Bot))])
    Γ))
