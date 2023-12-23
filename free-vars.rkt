;; This module provides a memoized version of `free-vars` from base

#lang racket/base

(provide free-vars)

(require (prefix-in base: syntax/free-vars))

(define memo (make-hasheq))

(define (free-vars expr-stx)
  (or (hash-ref memo expr-stx #f)
      (let ([fv (base:free-vars expr-stx)])
        (hash-set! memo expr-stx fv)
        fv)))