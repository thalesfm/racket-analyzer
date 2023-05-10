#lang racket

(provide partial-eval)

(require syntax/id-table)

(struct closure (arg-ids expr env) #:transparent)

(define make-empty-environment make-immutable-bound-id-table)
(define bind-id dict-set)
(define lookup-id dict-ref)

(define-namespace-anchor anchor)

(define (partial-eval expr)
  (parameterize ([current-namespace (namespace-anchor->namespace anchor)])
    (partial-eval-syntax (datum->syntax #f expr))))

(define (partial-eval-syntax stx)
  (peval (expand stx) (make-empty-environment)))

(define (lam? stx)
  (define lam (car (syntax->list (cadr (syntax->list (expand '(lambda (x) x)))))))
  (and (identifier? stx) (free-identifier=? stx lam)))

(define (app? stx)
  (define app (car (syntax->list (expand '(f x)))))
  (and (identifier? stx) (free-identifier=? stx app)))

(define (exp? stx)
  (define exp #'#%expression)
  (and (identifier? stx) (free-identifier=? stx exp)))

(define (peval stx env)
  (syntax-case stx (if quote let-values +)
    [(exp expr) (exp? #'exp) (peval #'expr env)]
    [id (identifier? #'id) (lookup-id env #'id)]
    [(lam (arg-id ...) expr) (lam? #'lam)
     (closure (syntax->list #'(arg-id ...)) #'expr env)]
    [(if pred true-expr false-expr)
     (peval (if (peval #'pred env) #'true-expr #'false-expr) env)]
    [(quote datum) (syntax->datum #'datum)]
    [(let-values ([(id) expr]) body) (identifier? #'id)
     (peval #'body (bind-id env #'id (peval #'expr env)))]
    [(app + expr1 expr2) (app? #'app)
     (+ (peval #'expr1 env) (peval #'expr2 env))]
    [(app proc-expr arg-expr ...) (app? #'app)
     (let ([proc (peval #'proc-expr env)]
           [args (map (lambda (stx) (peval stx env)) (syntax->list #'(arg-expr ...)))])
       (partial-apply proc args))]))

(define (partial-apply proc args)
  (define eval-env
    (for/fold ([env (closure-env proc)])
              ([arg-id (in-list (closure-arg-ids proc))]
               [arg (in-list args)])
      (bind-id env arg-id arg)))
  (peval (closure-expr proc) eval-env))
