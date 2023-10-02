#lang racket

(provide abstract-eval)

(require syntax/parse
         "environment.rkt"
         "free-vars.rkt"
         "primops.rkt"
         "types.rkt")

(define (literal? stx)
  (let ([v (syntax-e stx)])
    (and (type? v) (literal-type? v))))

(struct closure (lambda-stx environment)
        #:prefab)

(define (closure-arg-ids clo)
  (syntax-case (closure-lambda-stx clo) ()
    [(_ (id ...) _) #'(id ...)]))

(define (closure-body-stx clo)
  (syntax-case (closure-lambda-stx clo) ()
    [(_ (_ ...) body) #'body]))

(define-syntax-class lambda
  (pattern ((~datum lambda) ~! (id:id ...) body:expr)))

(define (capture-vars env vars)
  (for/fold ([acc (make-empty-environment)])
            ([var vars])
    (environment-set acc var (environment-ref env var ⊥))))

(define (abstract-eval expr)
  (abstract-eval-syntax (datum->syntax #f expr)
                        (make-base-environment)))

(define (abstract-eval-syntax stx env)
  (syntax-parse stx
    #:datum-literals (quote if let letrec)

    [var:id (environment-ref env #'var ⊥)]
    [datum #:when (literal? #'datum) (syntax-e #'datum)]
    [(quote ~! datum) (syntax-e #'datum)]

    [lambda-expr:lambda
     (closure #'lambda-expr (capture-vars env (free-vars #'lambda-expr)))]

    [(if ~! expr1:expr expr2:expr expr3:expr)
     (define v1 (abstract-eval-syntax #'expr1 env))
     (cond
       [(⊥? v1) ⊥]
       [(T? v1)
        (lub (abstract-eval-syntax #'expr2 env)
             (abstract-eval-syntax #'expr3 env))]
       [(not (eq? v1 #f))
        (abstract-eval-syntax #'expr2 env)]
       [(eq? v1 #f)
        (abstract-eval-syntax #'expr3 env)])]

    [(let ~! ([var:id val-expr:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (let/ec break
       (define env-prime
         (for/fold ([acc env])
                   ([var (in-syntax #'(var ...))]
                    [val-expr (in-syntax #'(val-expr ...))])
           (let ([v (abstract-eval-syntax val-expr env)])
             (if (⊥? v)
                 (break ⊥)
                 (environment-set acc var v)))))
       (abstract-eval-syntax #'body env-prime))]

    [(letrec ~! ([var:id val-expr:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (define env-prime
       (for/fold ([acc env])
                 ([var (in-syntax #'(var ...))])
         (environment-set acc var (make-placeholder ⊥))))
     (let/ec break
       (for ([var (in-syntax #'(var ...))]
             [val-expr (in-syntax #'(val-expr ...))])
         (let ([v (abstract-eval-syntax val-expr env-prime)])
           (if (⊥? v)
               (break ⊥)
               (placeholder-set! (environment-ref env-prime var) v))))
       (abstract-eval-syntax #'body (make-reader-graph env-prime)))]

    [(proc-expr:expr arg-expr:expr ...)
     (let/ec break
       (define proc (abstract-eval-syntax #'proc-expr env))
       (when (⊥? proc)
         (break ⊥))
       (define args
         (for/list ([arg-expr (in-syntax #'(arg-expr ...))])
           (let ([v (abstract-eval-syntax arg-expr env)])
             (if (⊥? v)
                 (break ⊥)
                 v))))
       (abstract-apply proc args))]))

(define (abstract-apply proc args)
  (match proc
    [(? procedure?) (apply proc args)]
    [(? closure?)
     (abstract-eval-syntax
       (closure-body-stx proc)
       (for/fold ([env (closure-environment proc)])
                 ([arg-id (in-syntax (closure-arg-ids proc))]
                  [arg (in-list args)])
         (environment-set env arg-id arg)))]))
