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

(define (closure-lub clo1 clo2)
  (cond
   [(eq? (closure-lambda clo1) (closure-lambda clo2))
    (closure (closure-lambda clo1)
             (environment-lub (closure-environment clo1)
                              (closure-environment clo2)))]
   [else T]))

(define (environment-lub env1 env2)
  (environment-union env1 env2 #:combine lub))

(define (lub v1 v2)
  (cond
   [(eq? v1 v2) v1]
   [(and (closure? v1) (closure? v2)) (closure-lub v1 v2)]
   [(or (closure? v1) (closure? v2)) T]
   [else (type-lub v1 v2)]))

(define (abstract-eval expr)
  (define stx
    (namespace-syntax-introduce (datum->syntax #f expr)
                                (make-base-namespace)))
  (define env (make-base-environment))
  (abstract-eval-syntax stx env))

(define (syntax->value stx)
  (define v (syntax-e stx))
  (if (literal-type? v) v 'error))

(define (abstract-eval-syntax stx env [trace (hasheq)])
  (syntax-parse stx
    #:conventions (conventions)
    #:literal-sets (literal-set)
    [var (environment-ref env #'var ⊥)]
    [lit:literal
     #:do [(define value (syntax->value (attribute lit.datum-stx)))]
     #:fail-unless (not (eq? value 'error)) "expected literal"
     value]
    [lam:lambda-expr
     (define captured-env
       (for/fold ([acc (make-empty-environment)])
                 ([var (free-vars #'lam)])
         (environment-set acc var (environment-ref env var ⊥))))
     (closure #'lam captured-env)]
    [(if ~! test-expr then-expr else-expr)
     (define test-val (abstract-eval-syntax #'test-expr env trace))
     (cond
      [(⊥? test-val) ⊥]
      [(T? test-val) (lub (abstract-eval-syntax #'then-expr env trace)
                          (abstract-eval-syntax #'else-expr env trace))]
      [(not (eq? test-val #f)) (abstract-eval-syntax #'then-expr env trace)]
      [(eq? test-val #f)       (abstract-eval-syntax #'else-expr env trace)])]
    [(let ~! ([var val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-syntax val-expr env trace)))
     (define (env-prime)
       (for/fold ([acc env])
                 ([var (in-syntax #'(var ...))]
                  [val (in-stream vals)])
         (environment-set acc var val)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else (abstract-eval-syntax #'body (env-prime) trace)])]
    [(letrec ~! ([var val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (define env-prime
       (for/fold ([acc env])
                 ([var (in-syntax #'(var ...))])
         (environment-set acc var (make-placeholder ⊥))))
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-syntax val-expr env-prime trace)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else
       (for ([var (in-syntax #'(var ...))]
             [val (in-stream vals)])
         (placeholder-set! (environment-ref env-prime var) val))
       (abstract-eval-syntax #'body (make-reader-graph env-prime) trace)])]
    [(proc-expr arg-expr ...)
     (define proc (abstract-eval-syntax #'proc-expr env trace))
     (define args
       (for/stream ([arg-expr (in-syntax #'(arg-expr ...))])
         (abstract-eval-syntax arg-expr env trace)))
     (cond
      [(or (⊥? proc) (stream-ormap ⊥? args)) ⊥]
      [else (abstract-apply proc (stream->list args) trace)])]))

(define (abstract-apply proc args trace)
  (cond
   [(T? proc) T]
   [(procedure? proc) (apply proc args)]
   [(closure? proc)
    (define/syntax-parse lam:lambda-expr
      (closure-lambda proc))
    (define old-args (hash-ref trace #'lam args))
    (define new-args (map lub args old-args))
    (define new-trace (hash-set trace #'lam new-args))
    (define env-prime
      (for/fold ([acc (closure-environment proc)])
                ([arg-id (in-syntax #'(lam.arg-id ...))]
                 [arg (in-list new-args)])
        (environment-set acc arg-id arg)))
    (abstract-eval-syntax #'lam.body env-prime new-trace)]
  [else ⊥]))
