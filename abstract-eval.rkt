#lang racket

(provide abstract-eval)

(require syntax/parse
         "environment.rkt"
         "primops.rkt"
         "seq.rkt"
         "types.rkt")

(struct closure (arg-id-list body environment))

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
     (syntax->datum #'datum)]

    [((~datum quote) datum)
     (syntax->datum #'datum)]

    [((~datum lambda) ~! (id:id ...) body)
     (closure #'(id ...) #'body env)]

    [((~datum if) ~! test:expr then:expr else:expr)
     (let/seq ([test-v (abstract-eval-syntax #'test env)])
       (cond
         [(type<=? test-v Truthy) (abstract-eval-syntax #'then env)]
         [(eq? test-v #f) (abstract-eval-syntax #'else env)]
         [(eq? test-v Top) (lub (abstract-eval-syntax #'then env)
                                (abstract-eval-syntax #'else env))]))]

    [((~datum let) ~! ([id:id expr:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (let/seq ([vs (abstract-eval-syntaxes #'(expr ...) env)])
       (abstract-eval-syntax #'body (bind* env #'(id ...) vs)))]

    [((~datum letrec) ~! ([id:id expr:expr] ...) body)
     (let ([new-env (fresh* env #'(id ...))])
       (seq (for/seq ([id (in-syntax #'(id ...))]
                      [expr (in-syntax #'(expr ...))])
              (let/seq ([v (abstract-eval-syntax expr new-env)])
                (bind! new-env id v)))
            (abstract-eval-syntax #'body new-env)))]

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
  (cond
    [(procedure? proc) (apply proc args)]
    [(closure? proc)
     (abstract-eval-syntax
      (closure-body proc)
      (bind* (closure-environment proc)
             (closure-arg-id-list proc)
             args))]))
