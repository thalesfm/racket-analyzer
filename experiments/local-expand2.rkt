#lang racket

(require (for-syntax syntax/parse))

(define-syntax (y stx)
  (syntax-case stx ()
    [y (identifier? (syntax y)) #'10]))

(define-for-syntax (subs/10 expr x)
  (define/syntax-parse ((~literal let-values) ()
                         ((~literal let-values) () new-expr))
    (local-expand #`(let-syntax ([#,x (lambda (stx) #'y)]) #,expr)
                  'expression
                  '()))
  #'new-expr)

(begin-for-syntax
  (define e #'(+ x 3))
  (define x (cadr (syntax-e e)))
  (define new-e (subs/10 e x))
  (displayln (syntax->datum new-e)))
