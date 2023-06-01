#lang racket

(provide partial-eval)

(require syntax/stx "environment.rkt" "domain.rkt")

(struct closure (arg-id-list body environment))

(define-syntax (seq stx)
  (syntax-case stx (=>)
    [(seq expr => proc-expr)
     #'(let ([v expr]) (if (Nothing? v) Nothing (proc-expr v)))]
    [(seq expr ... last-expr)
     #'(if (or (Nothing? expr ...)) Nothing last-expr)]))

(define (literal? stx)
  (abstract? (syntax->datum stx)))

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr) (make-base-environment)))

(define (bind-multiple env ids values)
  (foldl (lambda (id value env) (bind env id value)) env ids values))

(define (partial-eval-syntax stx env)
  (syntax-case* stx (quote lambda if let letrec) module-or-top-identifier=?
    [id (identifier? #'id) (lookup env #'id)]

    [datum (literal? #'datum) (syntax->datum #'datum)]
    [(quote datum) (syntax->datum #'datum)]

    [(lambda (id ...) body)
     (closure (syntax->list #'(id ...)) #'body env)]

    [(if test-expr then-expr else-expr)
     (let ([test-v (partial-eval-syntax #'test-expr env)])
       (cond
         [(eq? test-v Nothing) Nothing]
         [(<=? test-v True) (partial-eval-syntax #'then-expr env)]
         [(eq? test-v #f) (partial-eval-syntax #'else-expr env)]
         [(eq? test-v Any) (lub (partial-eval-syntax #'then-expr env)
                                (partial-eval-syntax #'else-expr env))]))]

    [(let ([id val-expr]) body)
     (identifier? #'id)
     (let ([val (partial-eval-syntax #'val-expr env)])
       (seq val (partial-eval-syntax #'body (bind env #'id val))))]

    [(letrec ([id val-expr]) body)
     (identifier? #'id)
     (let* ([new-env (bind env #'id Nothing)]
            [val (partial-eval-syntax #'val-expr new-env)])
       (rebind! new-env #'id val)
       (seq val (partial-eval-syntax #'body new-env)))]

    [(proc-expr arg-expr ...)
     (seq (partial-eval-syntax #'proc-expr env) => (λ (proc)
       (seq (partial-eval-syntax* #'(arg-expr ...) env) => (λ (args)
         (partial-apply proc args)))))]
))

(define (partial-eval-syntax* lst-stx env)
  (for/foldr ([lst null])
             ([stx (in-syntax lst-stx)])
    (seq lst (seq (partial-eval-syntax stx env) =>
               (λ (v) (cons v lst))))))

(define (partial-apply proc args)
  (cond
    [(procedure? proc) (apply proc args)]
    [(closure? proc)
     (partial-eval-syntax
      (closure-body proc)
      (bind-multiple (closure-environment proc)
                     (closure-arg-id-list proc)
                     args))]))
