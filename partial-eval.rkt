#lang racket

(provide partial-eval)

(require syntax/parse
         "environment.rkt"
         "seq.rkt"
         "types.rkt")

(struct closure (arg-id-list body environment))

;; (define result? (disjoin value? ⊥?))
;; (call/seq (-> result? (-> value? result?) result?))

(define (literal? stx)
  (abstract? (syntax->datum stx)))

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr) (make-base-environment)))

;; FIXME: Avoid using ~datum in patterns if possible
(define (partial-eval-syntax stx env)
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
     (let/seq ([test-v (partial-eval-syntax #'test env)])
       (cond
         [(<=? test-v True) (partial-eval-syntax #'then env)]
         [(eq? test-v #f) (partial-eval-syntax #'else env)]
         [(eq? test-v Any) (lub (partial-eval-syntax #'then env)
                                (partial-eval-syntax #'else env))]))]

    [((~datum let) ~! ([id:id expr:expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (let/seq ([vs (partial-eval-syntaxes #'(expr ...) env)])
       (partial-eval-syntax #'body (bind* env #'(id ...) vs)))]

    [((~datum letrec) ~! ([id:id expr:expr] ...) body)
     (let ([new-env (fresh* env #'(id ...))])
       (seq (for/seq ([id (in-syntax #'(id ...))]
                      [expr (in-syntax #'(expr ...))])
              (let/seq ([v (partial-eval-syntax expr new-env)])
                (bind! new-env id v)))
            (partial-eval-syntax #'body new-env)))]

    [(proc-expr:expr arg-expr:expr ...)
     (let/seq ([proc (partial-eval-syntax #'proc-expr env)]
               [args (partial-eval-syntaxes #'(arg-expr ...) env)])
       (partial-apply proc args))]
))

(define (partial-eval-syntaxes stx-list env)
  (for/foldr ([lst null])
             ([stx (in-syntax stx-list)])
    (seq lst (let/seq ([v (partial-eval-syntax stx env)])
               (cons v lst)))))

(define (partial-apply proc args)
  (cond
    [(procedure? proc) (apply proc args)]
    [(closure? proc)
     (partial-eval-syntax
      (closure-body proc)
      (bind* (closure-environment proc)
             (closure-arg-id-list proc)
             args))]))
