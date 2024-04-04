#lang racket

(provide abstract-eval
         abstract-eval-syntax)

(require syntax/parse
         "abstract-value.rkt"
         "environment.rkt"
         "primitives.rkt")

;; TODO: Rename `namespace`
(define (abstract-eval top-level-form [namespace primitives-namespace])
  (parameterize ([current-namespace namespace])
    (abstract-eval-syntax
     (namespace-syntax-introduce
      (datum->syntax #f top-level-form)))))

;; TODO: Rename `namespace`
(define (abstract-eval-syntax top-level-form [namespace primitives-namespace])
  (parameterize ([current-namespace namespace])
    (abstract-eval-kernel-syntax (expand top-level-form))))

;; TODO: Convert kernel syntax to the language used in the equations?

;; Locally bound identifier
(define-syntax-class var
  (pattern (~and id:id (~fail #:unless (eq? (identifier-binding #'id) 'lexical)))))

(define-syntax-class const
  #:literals (quote)
  (pattern (quote datum) #:attr value (syntax-e #'datum)))

;; TODO: Don´t produce proc directly?
;; Identifier with top-level binding
(define-syntax-class primop
  (pattern id:id #:attr proc (namespace-variable-value (syntax-e #'id) #t)))

(define (abstract-eval-kernel-syntax expr [ρ (make-environment)])
  (define eval^ abstract-eval-kernel-syntax)
  (syntax-parse expr
    #:literal-sets (kernel-literals)

    ;; FIXME: Re-entrant on bad `letrec`
    [x:var (force (environment-ref ρ #'x))]

    [k:const (attribute k.value)]

    [o:primop (attribute o.proc)]

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
       (define ρ′ (environment-set* ρ #'(x ...) args))
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
     (define ρ′ (environment-set* ρ #'(x ...) v-list))
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