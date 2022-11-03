#lang racket/base


; New syntax bindings replace old ones, so all-except-out is not neccessary below
(provide (all-defined-out)
         (all-from-out racket/base))

; Replacing syntax bindings doesn't seem to work since macros re-export identifiers
(require (for-syntax racket/base)
         (prefix-in r5rs: r5rs))

(define-syntax (define stx)
  (syntax-case stx ()
    [(define id expr) #'(r5rs:define id expr)]))

(define-syntax (let stx)
  (syntax-case stx ()
    [(let body ...) #'(r5rs:let body ...)]))