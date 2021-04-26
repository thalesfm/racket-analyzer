#lang racket

#| Version 1 |# #|
(define-syntax (m stx)
  (syntax-case stx ()
    [(_ expr)
     (local-expand
      #'(let-syntax ([x (lambda (stx) #'10)]) expr)
      'expression
      '())]))

(m (+ (x) 3))
|#

#| Version 2 |# #|
(require (for-syntax racket/syntax syntax/parse))

(define-for-syntax (substitute e sub)
  (syntax-parse sub
    [(x y)
     #:with x- (generate-temporary #'x)
     #:with ((~literal #%plain-lambda) _
              ((~literal let-values) ()
                ((~literal let-values) ()
                  expanded-e)))
            (local-expand
             #`(lambda (x-)
                 (let-syntax ([x (make-rename-transformer #'x-)])
                   #,e))
             'expression '())
     #'(quote expanded-e)]))
 
(begin-for-syntax
  (define e #'(+ x 10))
  (define x (cadr (syntax-e e)))
  (define y #'y)
  (define new-e (substitute e #'(x y)))
  (displayln new-e))
|#

#| Version 3 |#
(require (for-syntax syntax/parse))

(define-for-syntax (subs/10 expr x)
  (define/syntax-parse ((~literal let-values) ()
                         ((~literal let-values) () new-expr))
    (local-expand #`(let-syntax ([#,x (lambda (stx) #'10)]) #,expr)
                  'expression
                  '()))
  #'new-expr)

(begin-for-syntax
  (define e #'(+ x 3))
  (define x (cadr (syntax-e e)))
  (define new-e (subs/10 e x))
  (displayln (syntax->datum new-e)))
