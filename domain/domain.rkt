#lang racket

(provide)

(require racket/generic)

(define-generics domain
  (<=? domain other)
  (lub domain other))

;(struct domain (member? from-datum to-datum <=? lub))

(struct abstract-constant (value)
  #:methods gen:domain
  [(define (<=? k1 k2)
     (eq? k1 k2))
   (define (lub k1 k2)
     (error))])

#;(define check-datum
  (disjoin boolean? number? char? string? symbol?))

#;(define (datum->domain)
  (error))
