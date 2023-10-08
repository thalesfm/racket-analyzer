#lang racket

(provide free-vars)

(require syntax/id-set
         syntax/parse
         syntax/stx
         "syntax.rkt")

(define memo
  (make-thread-cell (make-weak-hasheq)))

(define (free-vars stx)
  (hash-ref! (thread-cell-ref memo)
             stx
             (lambda () (compute-free-vars stx))))

(define (compute-free-vars stx)
  (define id-set immutable-free-id-set)
  (syntax-parse stx
    #:conventions (conventions)
    #:literal-sets (literal-set)
    [var (id-set (list #'var))]
    [lit:literal (id-set)]
    [lam:lambda-expr
     (define arg-ids (id-set (syntax->list #'(lam.arg-id ...))))
     (set-subtract (free-vars #'lam.body) arg-ids)]
    [(if ~! expr1 expr2 expr3)
     (set-union (free-vars #'expr1)
                (free-vars #'expr2)
                (free-vars #'expr3))]
    [(let ~! ([var val-expr] ...) body)
     (define bound-vars (id-set (syntax->list #'(var ...))))
     (apply set-union
            (set-subtract (free-vars #'body) bound-vars)
            (stx-map free-vars #'(val-expr ...)))]
    [(letrec ~! ([var val-expr] ...) body)
     (define bound-vars (id-set (syntax->list #'(var ...))))
     (set-subtract
       (apply set-union
              (free-vars #'body)
              (stx-map free-vars #'(val-expr ...)))
       bound-vars)]
    [(expr ...)
     (apply set-union (stx-map free-vars #'(expr ...)))]))
