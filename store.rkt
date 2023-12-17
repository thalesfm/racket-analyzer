#lang racket

(provide σ-ref σ-set!)

(define σ (make-ephemeron-hasheq))

(define (σ-ref key . maybe-failure-result)
  (apply hash-ref σ key maybe-failure-result))

(define (σ-set! key v)
  (hash-set! σ key v))