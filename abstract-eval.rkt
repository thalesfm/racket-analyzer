#lang racket

(provide abstract-eval)

(require syntax/parse
         "environment.rkt"
         "primops.rkt"
         "seq.rkt"
         "types.rkt")

;; (define result? (disjoin value? ⊥?))
;; (call/seq (-> result? (-> value? result?) result?))

(define (literal? stx)
  (let ([v (syntax-e stx)])
    (and (type? v) (literal-type? v))))

(struct closure (lambda-stx environment)
        #:prefab)

(define (closure-arg-ids clo)
  (syntax-case (closure-lambda-stx clo) ()
    [(lambda (id ...) body) #'(id ...)]))

(define (closure-body-stx clo)
  (syntax-case (closure-lambda-stx clo) ()
    [(lambda (id ...) body) #'body]))

(define-syntax-class lambda
  (pattern ((~datum lambda) ~! (id:id ...) body:expr)))

(define (abstract-eval expr)
  (abstract-eval-syntax (datum->syntax #f expr)
                        (make-base-environment)))

;; FIXME: Avoid using ~datum in patterns if possible
(define (abstract-eval-syntax stx env)
  (syntax-parse stx

    [id:id
     (environment-ref env #'id Bot)]

    [datum
     #:when (literal? #'datum)
     (syntax-e #'datum)]

    [((~datum quote) ~! datum)
     (syntax-e #'datum)]

    [lam:lambda
     (closure #'lam env)]

    [((~datum if) ~! test:expr then:expr else:expr)
     (let/seq ([test-v (abstract-eval-syntax #'test env)])
       (cond
         [(type<=? test-v Truthy) (abstract-eval-syntax #'then env)]
         [(eq? test-v #f) (abstract-eval-syntax #'else env)]
         [(eq? test-v Top) (lub (abstract-eval-syntax #'then env)
                                (abstract-eval-syntax #'else env))]))]

    [((~datum let) ~! ([x:id e:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(x ...)))
                 "duplicate identifier"
     (define inner-env
       (for/fold ([env env])
                 ([x (in-syntax #'(x ...))]
                  [e (in-syntax #'(e ...))])
         (let/seq ([v (abstract-eval-syntax e env)])
           (environment-set env x v))))
     (seq inner-env (abstract-eval-syntax #'body inner-env))]

    [((~datum letrec) ~! ([x:id e:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(x ...)))
                 "duplicate identifier"
     (define inner-env
       (for/fold ([env env])
                 ([x (in-syntax #'(x ...))])
         (environment-set env x (make-placeholder Bot))))
     (seq
       (for/seq ([x (in-syntax #'(x ...))]
                 [e (in-syntax #'(e ...))])
         (let/seq ([v (abstract-eval-syntax e inner-env)])
           (placeholder-set! (environment-ref inner-env x) v)))
       (abstract-eval-syntax #'body (make-reader-graph inner-env)))]

    [(proc-expr:expr arg-expr:expr ...)
     (let/seq ([proc (abstract-eval-syntax #'proc-expr env)]
               [args (abstract-eval-syntaxes #'(arg-expr ...) env)])
       (abstract-apply proc args))]
))

(define (abstract-eval-syntaxes stx-list env)
  (for/foldr ([lst null])
             ([stx (in-syntax stx-list)])
    (seq lst (let/seq ([v (abstract-eval-syntax stx env)])
               (cons v lst)))))

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
