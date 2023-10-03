#lang racket

(provide abstract-eval)

(require syntax/parse
         "environment.rkt"
         "free-vars.rkt"
         "primops.rkt"
         "types.rkt")

(struct closure (lambda environment) #:prefab)

(define (closure-arg-ids clo)
  (syntax-case (closure-lambda clo) ()
    [(_ arg-ids _) #'arg-ids]))

(define (closure-body-expr clo)
  (syntax-case (closure-lambda clo) ()
    [(_ _ body) #'body]))

(define (capture env vars)
  (for/fold ([acc (make-empty-environment)])
            ([var vars])
    (environment-set acc var (environment-ref env var ⊥))))

(define (abstract-eval expr)
  (abstract-eval-syntax (datum->syntax #f expr)
                        (make-base-environment)))

(define (abstract-eval-syntax stx env)
  (syntax-parse stx
    #:datum-literals (quote lambda if let letrec)
    [var:id (environment-ref env #'var ⊥)]
    [datum #:when (literal-type? (syntax-e #'datum))
     (syntax-e #'datum)]
    [(quote ~! datum) (syntax-e #'datum)]
    [(~and lambda-expr (lambda ~! (_arg-id:id ...) _body:expr))
     (closure #'lambda-expr (capture env (free-vars #'lambda-expr)))]
    [(if ~! expr1:expr expr2:expr expr3:expr)
     (define v1 (abstract-eval-syntax #'expr1 env))
     (cond
      [(⊥? v1) ⊥]
      [(T? v1) (lub (abstract-eval-syntax #'expr2 env)
                    (abstract-eval-syntax #'expr3 env))]
      [(not (eq? v1 #f)) (abstract-eval-syntax #'expr2 env)]
      [(eq? v1 #f)       (abstract-eval-syntax #'expr3 env)])]
    [(let ~! ([var:id val-expr:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-syntax val-expr env)))
     (define (env-prime)
       (for/fold ([acc env])
                 ([var (in-syntax #'(var ...))]
                  [val (in-stream vals)])
         (environment-set acc var val)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else (abstract-eval-syntax #'body (env-prime))])]
    [(letrec ~! ([var:id val-expr:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (define env-prime
       (for/fold ([acc env])
                 ([var (in-syntax #'(var ...))])
         (environment-set acc var (make-placeholder ⊥))))
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-syntax val-expr env-prime)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else
       (for ([var (in-syntax #'(var ...))]
             [val (in-stream vals)])
         (placeholder-set! (environment-ref env-prime var) val))
       (abstract-eval-syntax #'body (make-reader-graph env-prime))])]
    [(proc-expr:expr arg-expr:expr ...)
     (define proc (abstract-eval-syntax #'proc-expr env))
     (define args
       (for/stream ([arg-expr (in-syntax #'(arg-expr ...))])
         (abstract-eval-syntax arg-expr env)))
     (cond
      [(or (⊥? proc) (stream-ormap ⊥? args)) ⊥]
      [else (abstract-apply proc (stream->list args))])]))

(define (abstract-apply proc args)
  (cond
   [(procedure? proc) (apply proc args)]
   [(closure? proc)
    (define env-prime
      (for/fold ([acc (closure-environment proc)])
                ([arg-id (in-syntax (closure-arg-ids proc))]
                 [arg (in-list args)])
        (environment-set acc arg-id arg)))
    (abstract-eval-syntax (closure-body-expr proc) env-prime)]))
