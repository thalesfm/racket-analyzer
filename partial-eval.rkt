#lang racket

(provide partial-eval)

;; TODO: Primitives should evaluate to values as expressions
;;       (i.e. not only when applied to arguments)
;; TODO: Add primitives to environment?
;; TODO: Implement multiple bindings for let forms

(require syntax/stx)

(struct closure (arg-ids body env))

(define (make-empty-environment) (hash))
(define bind dict-set)
(define lookup dict-ref)

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr)))

(define (partial-eval-syntax stx)
  (peval stx (make-empty-environment)))

(define (literal? stx)
  (define v (syntax->datum stx))
  (or (boolean? v) (number? v)))

(define primitive-dict
  (hash '+ + '- - '* * '/ /))

(define (primitive? sym)
  (dict-has-key? primitive-dict sym))

(define (apply-primitive sym args)
  (define proc (dict-ref primitive-dict sym))
  (apply proc args))

(define (peval stx env)
  (syntax-case* stx (quote lambda if let) module-or-top-identifier=?
    [id (identifier? #'id) (lookup env (syntax->datum #'id))]
    [datum (literal? #'datum) (syntax->datum #'datum)]
    [(quote datum) (syntax->datum #'datum)]
    [(lambda (id ...) body)
     (closure (map syntax->datum (syntax->list #'(id ...))) #'body env)]
    [(if expr1 expr2 expr3)
     (if (peval #'expr1 env)
         (peval #'expr2 env)
         (peval #'expr3 env))]
    [(let ([id expr]) body) (identifier? #'id)
     (let ([new-env (bind env (syntax->datum #'id) (peval #'expr env))])
       (peval #'body new-env))]
    [(prim arg-expr ...) (and (identifier? #'prim) (primitive? (syntax->datum #'prim)))
     (let ([args (map (lambda (stx) (peval stx env)) (syntax->list #'(arg-expr ...)))])
       (apply-primitive (syntax->datum #'prim) args))]
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
  (peval (closure-body proc) eval-env))
