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

(define (abstract-eval expr)
  (abstract-eval-syntax (datum->syntax #f expr) (make-base-environment)))

;; FIXME: Avoid using ~datum in patterns if possible
(define (abstract-eval-syntax stx env)
  (syntax-parse stx

    [id:id
     (lookup env #'id)]

    [datum
     #:when (literal? #'datum)
     (syntax-e #'datum)]

    [((~datum quote) ~! datum)
     (syntax-e #'datum)]

    [((~datum lambda) ~! (id:id ...) body)
     (Closure #'(id ...) #'body env)]

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
     (define inner-Γ
       (for/fold ([Γ env])
                 ([x (in-syntax #'(x ...))]
                  [e (in-syntax #'(e ...))])
         (let/seq ([v (abstract-eval-syntax e Γ)])
           (bind Γ x v))))
     (seq inner-Γ (abstract-eval-syntax #'body inner-Γ))]

    [((~datum letrec) ~! ([x:id e:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(x ...)))
                 "duplicate identifier"
     (define inner-Γ
       (for/fold ([Γ env])
                 ([x (in-syntax #'(x ...))])
         (bind Γ x Bot)))
     (seq
       (for/seq ([x (in-syntax #'(x ...))]
                 [e (in-syntax #'(e ...))])
         (let/seq ([v (abstract-eval-syntax e inner-Γ)])
           (rebind! inner-Γ x v)))
       (abstract-eval-syntax #'body inner-Γ))]

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
    [(Closure arg-id-list body-stx captured-env)
     (abstract-eval-syntax
       body-stx
       (for/fold ([env captured-env])
                 ([arg-id (in-syntax arg-id-list)]
                  [arg (in-list args)])
         (bind env arg-id arg)))]))
