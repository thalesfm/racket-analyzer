#lang racket

(provide abstract-eval
         (struct-out closure))

(require syntax/parse
         "environment.rkt"
         "free-vars.rkt"
         "primops.rkt"
         "syntax.rkt"
         "types.rkt")

(struct closure (lambda environment) #:prefab)

; TODO: Check performance impact of using `syntax-parse` here instead of
;       `syntax-case` (or simply `(cadr (syntax->list lambda-stx)`
(define (closure-arg-ids clo)
  (syntax-parse (closure-lambda clo)
    [lam:lambda-expr #'(lam.arg-id ...)]))

; TODO: Check performance impact of using `syntax-parse` here instead of
;       `syntax-case` (or simply `(caddr (syntax->list lambda-stx)`
(define (closure-body-expr clo)
  (syntax-parse (closure-lambda clo)
    [lam:lambda-expr #'lam.body]))

(define (closure-lub clo1 clo2)
  (cond
   [(eq? (closure-lambda clo1) (closure-lambda clo2))
    (closure (closure-lambda clo1)
             (environment-lub (closure-environment clo1)
                              (closure-environment clo2)))]
   [else T]))

(define (capture env vars)
  (for/fold ([acc (make-empty-environment)])
            ([var vars])
    (environment-set acc var (environment-ref env var ⊥))))

(define (environment-lub env1 env2)
  (environment-union env1 env2 #:combine lub))

(define (lub v1 v2)
  (cond
   [(eq? v1 v2) v1]
   [(and (closure? v1) (closure? v2)) (closure-lub v1 v2)]
   [(or (closure? v1) (closure? v2)) T]
   [else (type-lub v1 v2)]))

(define (abstract-eval expr)
  (abstract-eval-syntax (datum->syntax #f expr)
                        (make-base-environment)))

(define (syntax->value stx)
  (define v (syntax-e stx))
  (if (literal-type? v) v 'error))

(define (abstract-eval-syntax stx env)
  (syntax-parse stx
    #:conventions (syntax-conventions)
    #:literal-sets (syntax-literals)
    [var (environment-ref env #'var ⊥)]
    [lit
     #:do [(define value (syntax->value (attribute lit.datum)))]
     #:fail-unless (not (eq? value 'error)) "expected literal"
     value]
    [lam (closure #'lam (capture env (free-vars #'lam)))]
    [(if ~! test-expr then-expr else-expr)
     (define test-val (abstract-eval-syntax #'test-expr env))
     (cond
      [(⊥? test-val) ⊥]
      [(T? test-val) (lub (abstract-eval-syntax #'then-expr env)
                          (abstract-eval-syntax #'else-expr env))]
      [(not (eq? test-val #f)) (abstract-eval-syntax #'then-expr env)]
      [(eq? test-val #f)       (abstract-eval-syntax #'else-expr env)])]
    [(let ~! ([var val-expr] ...) body)
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
    [(letrec ~! ([var val-expr] ...) body)
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
    [(proc-expr arg-expr ...)
     (define proc (abstract-eval-syntax #'proc-expr env))
     (define args
       (for/stream ([arg-expr (in-syntax #'(arg-expr ...))])
         (abstract-eval-syntax arg-expr env)))
     (cond
      [(or (⊥? proc) (stream-ormap ⊥? args)) ⊥]
      [else (abstract-apply proc (stream->list args))])]))

(define (abstract-apply proc args)
  (cond
   [(T? proc) T]
   [(procedure? proc) (apply proc args)]
   [(closure? proc)
    (define env-prime
      (for/fold ([acc (closure-environment proc)])
                ([arg-id (in-syntax (closure-arg-ids proc))]
                 [arg (in-list args)])
        (environment-set acc arg-id arg)))
    (abstract-eval-syntax (closure-body-expr proc) env-prime)]
  [else ⊥]))
