#lang racket

(provide free-vars)

(require syntax/parse
         syntax/stx)

;; TODO: Cache results as syntax properties
(define (free-vars stx)
  (syntax-parse stx
    #:datum-literals (quote lambda if let letrec)
    [var:id (set (syntax-e #'var))]
    [datum #:when (not (list? (syntax-e #'datum))) (set)]
    [(quote ~! _) (set)]
    [(lambda ~! (arg-id:id ...) body:expr)
     (define arg-ids (list->set (stx-map syntax-e #'(arg-id ...))))
     (set-subtract (free-vars #'body) arg-ids)]
    [(if ~! expr1:expr expr2:expr expr3:expr)
     (set-union (free-vars #'expr1)
                (free-vars #'expr2)
                (free-vars #'expr3))]
    [(let ~! ([var:id val-expr:expr] ...) body)
     (define bound-vars (list->set (stx-map syntax-e #'(var ...))))
     (apply set-union
            (set-subtract (free-vars #'body) bound-vars)
            (stx-map free-vars #'(val-expr ...)))]
    [(letrec ~! ([var:id val-expr:expr] ...) body)
     (define bound-vars (list->set (stx-map syntax-e #'(var ...))))
     (set-subtract
       (apply set-union
              (free-vars #'body)
              (stx-map free-vars #'(val-expr ...)))
       bound-vars)]
    [(expr:expr ...)
     (apply set-union (stx-map free-vars #'(expr ...)))]))
