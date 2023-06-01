#lang racket

(provide partial-eval)

(require (for-syntax racket/syntax)
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

(define (literal? stx)
  (abstract? (syntax->datum stx)))

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr) (make-base-environment)))

(define (bind-multiple env ids values)
  (foldl (lambda (id value env) (bind env id value)) env ids values))

(define (partial-eval-syntax stx env)
  (syntax-case* stx (quote lambda if let letrec) module-or-top-identifier=?
    [id (identifier? #'id) (lookup env #'id)]

    [datum (literal? #'datum) (syntax->datum #'datum)]
    [(quote datum) (syntax->datum #'datum)]

    [(lambda (id ...) body)
     (closure (syntax->list #'(id ...)) #'body env)]

    [(if test-expr then-expr else-expr)
     (let/seq ([test-v (partial-eval-syntax #'test-expr env)])
       (cond
         [(<=? test-v True) (partial-eval-syntax #'then-expr env)]
         [(eq? test-v #f) (partial-eval-syntax #'else-expr env)]
         [(eq? test-v Any) (lub (partial-eval-syntax #'then-expr env)
                                (partial-eval-syntax #'else-expr env))]))]

    [(let ([id val-expr]) body)
     (identifier? #'id)
     (let/seq ([val (partial-eval-syntax #'val-expr env)])
       (partial-eval-syntax #'body (bind env #'id val)))]

    [(letrec ([id val-expr]) body)
     (identifier? #'id)
     (let ([new-env (bind env #'id Nothing)])
       (let/seq ([val (partial-eval-syntax #'val-expr new-env)])
         (rebind! new-env #'id val)
         (partial-eval-syntax #'body new-env)))]

    [(proc-expr arg-expr ...)
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
