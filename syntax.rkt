#lang racket

(require syntax/parse)

(provide literal lambda-expr syntax-literals syntax-conventions)

(define-syntax-class atomic-datum
  (pattern (~fail #:when (list? (syntax-e this-syntax)))))

#;(define-syntax-class datum
  #:datum-literals (#%datum)
  (pattern (#%datum . datum)))

; TODO: Improve error message when failing to match
(define-syntax-class literal
  #:description "literal"
  #:datum-literals (quote)
  (pattern (~or datum:atomic-datum (quote ~! datum))))

(define-syntax-class lambda-expr
  #:description "lambda expression"
  (pattern ((~datum lambda) ~! (arg-id:id ...) body:expr)))

(define-literal-set syntax-literals
  #:datum-literals (lambda quote if let letrec)
  ())

(define-conventions syntax-conventions
  [var id] [lit literal] [lam lambda-expr] [#rx"expr" expr])
