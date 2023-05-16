#lang racket

(provide partial-eval)

;; TODO: Implement multiple bindings for let forms
;; TODO: Implement letrec

(require syntax/stx)

(struct closure (arg-ids body env))

(define (make-empty-environment) (hash))

(define (make-base-environment)
  (hash '+ + '- - '* * '/ /))

(define (bind env sym value)
  (if (syntax? sym)
      (bind env (syntax->datum sym) value)
      (dict-set env sym value)))

(define (lookup env sym)
  (if (syntax? sym)
      (lookup env (syntax->datum sym))
      (dict-ref env sym)))

(define (literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum) (number? datum)))

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr) (make-base-environment)))

(define (partial-eval-syntax stx env)
  (syntax-case* stx (quote lambda if let) module-or-top-identifier=?
    [id (identifier? #'id) (lookup env #'id)]
    [datum (literal? #'datum) (syntax->datum #'datum)]
    [(quote datum) (syntax->datum #'datum)]
    [(lambda (id ...) body)
     (closure (syntax->list #'(id ...)) #'body env)]
    [(if pred then else)
     (if (partial-eval-syntax #'pred env)
         (partial-eval-syntax #'then env)
         (partial-eval-syntax #'else env))]
    [(let ([id expr]) body) (identifier? #'id)
     (let ([new-env (bind env #'id (partial-eval-syntax #'expr env))])
       (partial-eval-syntax #'body new-env))]
    [(proc-expr arg-expr ...)
     (let ([proc (partial-eval-syntax #'proc-expr env)]
           [args (map (lambda (stx) (partial-eval-syntax stx env))
                      (syntax->list #'(arg-expr ...)))])
       (partial-apply proc args))]))

(define (partial-apply proc args)
  (cond
    [(procedure? proc) (apply proc args)]
    [(closure? proc)
     (define eval-env
       (for/fold ([env (closure-env proc)])
                 ([arg-id (in-list (closure-arg-ids proc))]
                  [arg (in-list args)])
         (bind env arg-id arg)))
     (partial-eval-syntax (closure-body proc) eval-env)]))
