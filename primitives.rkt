#lang racket

(provide make-primitive-namespace)

(require "domain.rkt")

(define (make-primitive-namespace)
  (define nsp (make-base-namespace))
  (define (set-constant-value! sym v)
    (namespace-set-variable-value! sym v #t nsp #t))
  ; (set-constant-value! '+ ...)
  ; (set-constant-value! '- ...)
  ; (set-constant-value! '* ...)
  ; (set-constant-value! '/ ...)
  ; (set-constant-value! '= ...)
  (set-constant-value! 'read (lambda () T))
  (set-constant-value! 'error (lambda () (⊥ "error")))
  nsp)