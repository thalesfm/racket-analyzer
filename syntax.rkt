#lang racket

(require syntax/parse)

(provide literal lambda-expr literal-set conventions)

(define-literal-set literal-set
  (lambda quote if let letrec))

(define-conventions conventions
  [var id]
  [lit literal]
  [lam lambda-expr]
  [#rx"(^|-)expr$" expr])

(define-syntax-class atomic-datum
  (pattern (~fail #:when (list? (syntax-e this-syntax)))))

; TODO: Improve error message when failing to match
(define-syntax-class literal
  #:description "literal"
  #:no-delimit-cut
  #:literals (quote)
  (pattern datum:atomic-datum)
  (pattern (quote ~! datum)))

(define-syntax-class lambda-expr
  #:description "lambda expression"
  #:no-delimit-cut
  #:literals (lambda)
  (pattern (lambda ~! (arg-id:id ...) body:expr)))
