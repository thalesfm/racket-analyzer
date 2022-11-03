#lang racket

(provide (all-defined-out))

(define (partial-eval stx)
  (local-expand stx 'expression '(define)))