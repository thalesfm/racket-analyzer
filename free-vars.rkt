#lang racket

(provide free-vars)

(require syntax/parse
         syntax/stx
         "syntax.rkt")

(define memo
  (make-thread-cell (make-weak-hasheq)))

(define (free-vars stx)
  (hash-ref! (thread-cell-ref memo)
             stx
             (lambda () (compute-free-vars stx))))

(define (compute-free-vars stx)
  (syntax-parse stx
    #:conventions (syntax-conventions)
    #:literal-sets (syntax-literals)
    [var (set (syntax-e #'var))]
    [lit (set)]
    [lam
     (define arg-ids (list->set (stx-map syntax-e #'(lam.arg-id ...))))
     (set-subtract (free-vars #'lam.body) arg-ids)]
    [(if ~! expr1 expr2 expr3)
     (set-union (free-vars #'expr1)
                (free-vars #'expr2)
                (free-vars #'expr3))]
    [(let ~! ([var:id val-expr] ...) body)
     (define bound-vars (list->set (stx-map syntax-e #'(var ...))))
     (apply set-union
            (set-subtract (free-vars #'body) bound-vars)
            (stx-map free-vars #'(val-expr ...)))]
    [(letrec ~! ([var:id val-expr] ...) body)
     (define bound-vars (list->set (stx-map syntax-e #'(var ...))))
     (set-subtract
       (apply set-union
              (free-vars #'body)
              (stx-map free-vars #'(val-expr ...)))
       bound-vars)]
    [(expr ...)
     (apply set-union (stx-map free-vars #'(expr ...)))]))
