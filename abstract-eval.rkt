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

    ;; TODO: If possible, remove promise-related stuff
    [x:var
     (define d (environment-ref ρ #'x))
     (cond
      [(and (promise? d) (not (promise-forced? d))) ⊥]
      [else (force d)])]

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

    ;; TODO: Should produce a procedure
    [(#%plain-lambda (x ...) expr)
     (define (proc . args)
       (define ρ′
         (for/fold ([ρ′ ρ])
                   ([x (in-syntax #'(x ...))]
                    [d (in-list args)])
           (environment-set ρ′ x d)))
       (abstract-eval-kernel-syntax #'expr ρ′))
     (make-closure this-syntax ρ)]

    ;; TODO: Remove cut?
    [(if ~! expr0 expr1 expr2)
     (define d (eval^ #'expr0 ρ))
     (cond
      [(eq? d #t) (eval^ #'expr1 ρ)]
      [(eq? d #f) (eval^ #'expr2 ρ)]
      [(eq? d  T) (lub (eval^ #'expr1 ρ) (eval^ #'expr2 ρ))]
      [(eq? d  ⊥) ⊥])]

    [(let-values ~! ([(x:id) expr] ...) expr0)
     (eval^ #'(#%plain-app (#%plain-lambda (x ...) expr0) expr ...) ρ)]

    ;; TODO: Make more similar to equation (single clause?)
    [(letrec-values ~! ([(x:id) expr] ...) expr0)
     (define ρ′
       (for/fold ([ρ ρ])
                 ([x (in-syntax #'(x ...))]
                  [expr (in-syntax #'(expr ...))])
         (define d (delay (eval^ expr ρ′)))
         (environment-set ρ x d)))
     (cond
      [(for/and ([x (in-syntax #'(x ...))])
         (not (eq? (force (environment-ref ρ′ x)) ⊥)))
       (eval^ #'expr0 ρ′)]
      [else ⊥])]))

;; TODO: Make more similiar to equations
;; TODO: Fix infinite loop
(define (abstract-apply proc args)
  (cond
   [(ormap ⊥? args) ⊥]
   [(closure? proc)
    (define/syntax-parse (_ (x ...) expr) (closure-source-syntax proc))
    (define args′ (continuation-mark-set-first #f proc))
    (define args″ (if args′ (map lub args args′) args))
    (define ρ′
      (for/fold ([ρ (closure-environment proc)])
                ([x (in-syntax #'(x ...))]
                 [d (in-list args″)])
        (environment-set ρ x d)))
    (with-continuation-mark proc args″
      (abstract-eval-kernel-syntax #'expr ρ′))]
   [(procedure? proc)
    (apply proc args)]
   [else ⊥]))