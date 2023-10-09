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
    #:literal-sets (kernel-literals)
    [id (id-set (list #'id))]
    [(#%plain-lambda (id ...) body)
     (define arg-ids (id-set (syntax->list #'(id ...))))
     (set-subtract (free-vars #'body) arg-ids)]
    [(case-lambda . _) (error "case-lambda not supported")]
    [(if ~! expr1 expr2 expr3)
     (set-union (free-vars #'expr1)
                (free-vars #'expr2)
                (free-vars #'expr3))]
    [(begin . _) (error "begin not supported")]
    [(begin0 . _) (error "begin0 not supported")]
    [(let-values ~! ([(id) val-expr] ...) body)
     (define bound-vars (id-set (syntax->list #'(id ...))))
     (apply set-union
            (set-subtract (free-vars #'body) bound-vars)
            (stx-map free-vars #'(val-expr ...)))]
    [(letrec-values ~! ([(id) val-expr] ...) body)
     (define bound-vars (id-set (syntax->list #'(id ...))))
     (set-subtract
       (apply set-union
              (free-vars #'body)
              (stx-map free-vars #'(val-expr ...)))
       bound-vars)]
    [(set! . _) (error "set! not supported")]
    [(quote datum) (id-set)]
    [(quote-syntax . _) (error "quote-syntax not supported")]
    [(with-continuation-mark . _) (error "with-continuatio-mark not supported")]
    [(#%plain-app expr ...)
     (apply set-union (stx-map free-vars #'(expr ...)))]
    [(#%top . _) (error "#%top not supported")]
    [(#%variable-reference . _) (error "#%variable-reference not supported")]))
