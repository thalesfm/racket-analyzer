#lang racket

(provide abstract-eval)

(require syntax/parse
         "abstract-value.rkt"
         "environment.rkt"
         "primop.rkt"
         "syntax-util.rkt")

;; (module->namespace 'racket/base)

(define (abstract-eval top-level-form [namespace (current-namespace)])
  (abstract-eval-kernel-syntax
   (expand
    (namespace-syntax-introduce (datum->syntax #f top-level-form) namespace))))

;; TODO: Convert kernel syntax to the language used in the equations?

(define (abstract-eval-kernel-syntax expr [ρ (make-environment)])
  (define eval^ abstract-eval-kernel-syntax)
  (syntax-parse expr
    #:literal-sets (kernel-literals)

    ;; FIXME: Re-entrant on bad `letrec`
    [x:var (force (lookup ρ #'x))]

    [k:const (attribute k.value)]

    [o:primop (get-primop (syntax-e #'o))]

    [(#%plain-app expr0 expr ...)
     (define proc (eval^ #'expr0 ρ))
     (define args
       (for/list ([expr (in-syntax #'(expr ...))])
         (eval^ expr ρ)))
     (cond
       [(or (procedure? proc) (closure? proc)) (abstract-apply proc args)]
       [else ⊥])]

    [(#%plain-lambda (x ...) expr)
     (lambda args
       (define ρ′ (extend* ρ #'(x ...) args))
       (abstract-eval-kernel-syntax #'expr ρ′))]

    [(if expr0 expr1 expr2)
     (define d (eval^ #'expr0 ρ))
     (cond
      [(eq? d #t) (eval^ #'expr1 ρ)]
      [(eq? d #f) (eval^ #'expr2 ρ)]
      [(eq? d  T) (lub (eval^ #'expr1 ρ) (eval^ #'expr2 ρ))]
      [(eq? d  ⊥) ⊥])]

    [(let-values ([(x:id) expr] ...) expr0)
     (eval^ #'(#%plain-app (#%plain-lambda (x ...) expr0) expr ...) ρ)]

    ;; TODO: Make more similar to equation (single clause?)
    [(letrec-values ([(x:id) expr] ...) expr0)
     (define v-list
       (for/list ([x (in-syntax #'(x ...))]
                  [expr (in-syntax #'(expr ...))])
         (delay (eval^ expr ρ′))))
     (define ρ′ (extend* ρ #'(x ...) v-list))
     (cond
      [(ormap (lambda (v) (eq? (force v) ⊥)) v-list) ⊥]
      [else (eval^ #'expr0 ρ′)])]))

;; TODO: Fix infinite loop
(define (abstract-apply proc args)
  (cond
   [(ormap ⊥? args) ⊥]
   [(procedure? proc)
    (apply proc args)]
   [else ⊥]))