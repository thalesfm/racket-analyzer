#lang racket

(provide abstract-eval
         (struct-out closure))

(require syntax/parse
         "closure.rkt"
         "environment.rkt"
         "free-vars.rkt"
         "ordering.rkt"
         "primops.rkt"
         "syntax.rkt"
         "types.rkt")

(define (abstract-eval expr [namespace (current-namespace)])
  (define stx (namespace-syntax-introduce (datum->syntax #f expr) namespace))
  (define env (make-base-environment))
  (abstract-eval-syntax stx env))

(define (abstract-eval-syntax stx env [trace (hasheq)])
  (syntax-parse stx
    #:conventions (conventions)
    #:literal-sets (literal-set)
    [var (environment-ref env #'var ⊥)]
    [lit:literal (datum->type (syntax->datum #'lit.datum))]
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
      [(not (equal? test-val (datum->type #f)))
       (abstract-eval-syntax #'then-expr env trace)]
      [(equal? test-val (datum->type #f))
       (abstract-eval-syntax #'else-expr env trace)])]
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

(define (iterate-fixpoint proc init)
  (define result (proc init))
  (if (<=? result init)
      result
      (iterate-fixpoint proc result)))

(define (abstract-apply proc args trace)
  (cond
   [(T? proc) T]
   [(procedure? proc) (apply proc args)]
   [(closure? proc)
    (define/syntax-parse lam:lambda-expr
      (closure-lambda proc))
    (match-define (cons prev-args result)
      (hash-ref trace #'lam (cons #f ⊥)))
    (cond
     [(and prev-args (andmap <=? args prev-args)) result]
     [else
      (define new-args
        (if prev-args (map lub args prev-args) args))
      (define env-prime
        (for/fold ([acc (closure-environment proc)])
                  ([arg-id (in-syntax #'(lam.arg-id ...))]
                   [arg (in-list new-args)])
          (environment-set acc arg-id arg)))
      (iterate-fixpoint
        (lambda (result)
          (define new-trace (hash-set trace #'lam (cons new-args result)))
          (abstract-eval-syntax #'lam.body env-prime new-trace))
        ⊥)])] ; Maybe use previous result instead of ⊥?
  [else ⊥]))
