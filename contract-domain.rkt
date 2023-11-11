#lang racket

(require reacket/contract/combinator
         "abstract-eval.rkt")

; Like `or/c` but flattens nested contracts
#;(define (or*/c . contracts)
  (error "not implemented"))

(define (syntax->contract stx)
  (define c (coerce-contract/f (syntax-e stx)))
  (if c c ⊥))

(define (infer-contract expr)
  (parameterize ([property-from-syntax syntax->contract]
                 [property-stronger? contract-stronger?]
                 [property-combine or/c])
    (abstract-eval-syntax expr (make-base-namespace))))
