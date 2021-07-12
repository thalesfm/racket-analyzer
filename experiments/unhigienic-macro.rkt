#lang racket

(provide (all-defined-out))

; Here, (datum->syntax stx 'return) works as expected while
; (datum->syntax #f 'return) yields an unbound identifier error.

(define-syntax (define/return stx)
  (syntax-case stx ()
    [(_ head body)
     (with-syntax ([return (datum->syntax stx 'return)])
       #'(define head (let/cc return body)))]))
