#lang racket

(provide partial-eval)

;; TODO: Implement multiple bindings for let forms
;; TODO: Implement letrec

(require syntax/stx "environment.rkt")

(struct closure (arg-ids body environment))

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
    ;; TODO: Check if there are no duplicate identifiers
    [(let ([id val-expr] ...) body) (andmap identifier? (syntax->list #'(id ...)))
     (let ([new-env (for/fold ([env env])
                              ([id (in-list (syntax->list #'(id ...)))]
                               [val-expr (in-list (syntax->list #'(val-expr ...)))])
                      (bind env id (partial-eval-syntax val-expr env)))])
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
       (for/fold ([env (closure-environment proc)])
                 ([arg-id (in-list (closure-arg-ids proc))]
                  [arg (in-list args)])
         (bind env arg-id arg)))
     (partial-eval-syntax (closure-body proc) eval-env)]))
