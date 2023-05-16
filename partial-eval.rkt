#lang racket

(provide partial-eval)

(require syntax/id-table syntax/stx)

(struct closure (arg-ids expr env))

(define make-empty-environment make-immutable-bound-id-table)
(define bind dict-set)
(define lookup dict-ref)

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr)))

(define (partial-eval-syntax stx)
  (peval stx (make-empty-environment)))

(define (literal? stx)
  (define v (syntax->datum stx))
  (or (boolean? v) (number? v)))

(define (peval stx env)
  (syntax-case* stx (quote lambda if let +) module-or-top-identifier=?
    [id (identifier? #'id) (lookup env #'id)]
    [datum (literal? #'datum) (syntax->datum #'datum)]
    [(quote datum) (syntax->datum #'datum)]
    [(lambda (arg-id ...) expr)
     (closure (syntax->list #'(arg-id ...)) #'expr env)]
    [(if pred true-expr false-expr)
     (peval (if (peval #'pred env) #'true-expr #'false-expr) env)]
    [(let ([id expr]) body) (identifier? #'id)
     (peval #'body (bind env #'id (peval #'expr env)))]
    [(+ expr1 expr2) (+ (peval #'expr1 env) (peval #'expr2 env))]
    [(proc-expr arg-expr ...)
     (let ([proc (peval #'proc-expr env)]
           [args (map (lambda (stx) (peval stx env)) (syntax->list #'(arg-expr ...)))])
       (partial-apply proc args))]))

(define (partial-apply proc args)
  (define eval-env
    (for/fold ([env (closure-env proc)])
              ([arg-id (in-list (closure-arg-ids proc))]
               [arg (in-list args)])
      (bind env arg-id arg)))
  (peval (closure-expr proc) eval-env))
