#lang racket

(provide partial-eval)

(require syntax/stx "environment.rkt" "domain.rkt")

(struct closure (arg-id-list body environment))

;; TODO: Implement as syntax with short-circuit semantics
(define (seq . vs)
  (if (ormap Nothing? vs) Nothing (last vs)))

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

    ;; TODO: Short-circuit when proc or any of the args evaluate to Nothing
    [(proc-expr arg-expr ...)
     (let ([proc (partial-eval-syntax #'proc-expr env)]
           [args (map (lambda (stx) (partial-eval-syntax stx env))
                      (syntax->list #'(arg-expr ...)))])
       (seq (apply seq (cons proc args)) (partial-apply proc args)))]
))

(define (partial-apply proc args)
  (cond
    [(procedure? proc) (apply proc args)]
    [(closure? proc)
     (partial-eval-syntax
      (closure-body proc)
      (bind-multiple (closure-environment proc)
                     (closure-arg-id-list proc)
                     args))]))
