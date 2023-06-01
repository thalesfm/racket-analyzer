#lang racket

(provide partial-eval)

(require (for-syntax racket/syntax
                     syntax/for-body
                     syntax/parse)
         syntax/parse
         syntax/stx
         "environment.rkt"
         "domain.rkt")

(struct closure (arg-id-list body environment))

(define-syntax (seq stx)
  (syntax-case stx ()
    [(_ expr ... final-expr)
     #'(if (or (Nothing? expr) ...) Nothing final-expr)]))

(define-syntax (let/seq stx)
  (syntax-case stx ()
    [(let/seq () body0 body ...)
     #'(let () body0 body ...)]
    [(let/seq ([id expr] . rest) body0 body ...)
     (with-syntax ([temp-id (generate-temporary #'id)])
       #'(let ([temp-id expr])
           (seq temp-id
                (let/seq rest
                  (let ([id temp-id]) body0 body ...)))))]))

(define-syntax (for/seq stx)
  (syntax-parse stx
    [(for/seq (for-clause ...) body-or-break ... body)
     #:with ((pre-body ...) (post-body ...))
            (split-for-body this-syntax #'(body-or-break ... body))
     #'(for/fold/derived this-syntax
                         ([accum (void)])
                         (for-clause ...)
         pre-body ...
         (seq accum (let () post-body ...)))]))

(define (literal? stx)
  (abstract? (syntax->datum stx)))

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr) (make-base-environment)))

(define (bind-multiple env id-list vs)
  (for/fold ([env env])
            ([id (in-syntax id-list)]
             [v (in-list vs)])
    (bind env id v)))

(define (create-locations env id-list)
  (for/fold ([env env])
            ([id (in-syntax id-list)])
    (bind env id Nothing)))

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
     (let/seq ([vs (partial-eval-syntaxes #'(expr ...) env)])
       (partial-eval-syntax #'body (bind-multiple env #'(id ...) vs)))]

    [((~datum letrec) ~! ([id:id expr:expr] ...) body)
     (let ([new-env (create-locations env #'(id ...))])
       (seq (for/seq ([id (in-syntax #'(id ...))]
                      [expr (in-syntax #'(expr ...))])
              (let/seq ([v (partial-eval-syntax expr new-env)])
                (rebind! new-env id v)))
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
      (bind-multiple (closure-environment proc)
                     (closure-arg-id-list proc)
                     args))]))
