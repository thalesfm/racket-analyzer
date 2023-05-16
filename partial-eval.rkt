#lang racket

(provide partial-eval)

;; TODO: Implement multiple bindings for let forms
;; TODO: Implement letrec

(require syntax/stx "environment.rkt")

(struct closure (arg-id-list body environment))

(define (literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum) (number? datum)))

(define (partial-eval expr)
  (partial-eval-syntax (datum->syntax #f expr) (make-base-environment)))

(define (bind-multiple env ids values)
  (foldl (lambda (id value env) (bind env id value)) env ids values))

(define (partial-eval-syntax stx env)
  (syntax-case* stx (quote lambda if let) module-or-top-identifier=?
    [id (identifier? #'id) (lookup env #'id)]
    [datum (literal? #'datum) (syntax->datum #'datum)]
    [(quote datum) (syntax->datum #'datum)]
    [(lambda (id ...) body)
     (closure (syntax->list #'(id ...)) #'body env)]
    [(if test-expr then-expr else-expr)
     (if (partial-eval-syntax #'test-expr env)
         (partial-eval-syntax #'then-expr env)
         (partial-eval-syntax #'else-expr env))]
    ;; TODO: Check if there are no duplicate identifiers
    [(let ([id val-expr] ...) body)
     (andmap identifier? (syntax->list #'(id ...)))
     (partial-eval-syntax
      #'body
      (bind-multiple env
                     (syntax->list #'(id ...))
                     (map (lambda (stx) (partial-eval-syntax stx env))
                          (syntax->list #'(val-expr ...)))))]
    [(proc-expr arg-expr ...)
     (partial-apply (partial-eval-syntax #'proc-expr env)
                    (map (lambda (stx) (partial-eval-syntax stx env))
                         (syntax->list #'(arg-expr ...))))]))

(define (partial-apply proc args)
  (cond
    [(procedure? proc) (apply proc args)]
    [(closure? proc)
     (partial-eval-syntax
      (closure-body proc)
      (bind-multiple (closure-environment proc)
                     (closure-arg-id-list proc)
                     args))]))
