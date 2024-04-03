#lang racket

(provide abstract-eval
         abstract-eval-syntax)

(require syntax/parse
         "domain.rkt"
         "environment.rkt"
         "primitives.rkt")

(define (abstract-eval top-level-form [namespace primitives-namespace])
  (parameterize ([current-namespace namespace])
    (abstract-eval-syntax
     (namespace-syntax-introduce
      (datum->syntax #f top-level-form)))))

(define (abstract-eval-syntax top-level-form [namespace primitives-namespace])
  (parameterize ([current-namespace namespace])
    (abstract-eval-kernel-syntax (expand top-level-form))))

;; TODO: Convert kernel syntax to the language used in the equations?

(define-syntax-class var
  (pattern (~and id:id (~fail #:unless (eq? (identifier-binding #'id) 'lexical)))))

(define-syntax-class const
  #:literals (quote)
  (pattern (quote datum) #:attr value (syntax-e #'datum)))

(define-syntax-class primop
  (pattern id:id #:attr proc (namespace-variable-value (syntax-e #'id) #t)))

;; TODO: Rename to `abstract-eval` if possible
(define (abstract-eval-kernel-syntax expr [ρ (make-environment)])
  (let eval^ ([expr expr] [ρ ρ])
    (let/ec break
      (define (eval^/strict expr ρ)
        (define d (eval^ expr ρ))
        (if (⊥? d) (break d) d))
      (syntax-parse expr
        #:literal-sets (kernel-literals)

        [x:var ;; TODO: If possible, remove promise-related stuff
         (define d (environment-ref ρ #'x))
         (cond
          [(and (promise? d) (not (promise-forced? d))) ⊥]
          [else (force d)])]

        [k:const (attribute k.value)]

        [o:primop (attribute o.proc)]

        [(#%plain-app expr0 expr1 ...)
         (define proc (eval^/strict #'expr0 ρ))
         (define args
           (for/list ([expr1 (in-syntax #'(expr1 ...))])
             (eval^ expr1 ρ)))
         (cond
           [(or (procedure? proc) (closure? proc)) (abstract-apply proc args)]
           [else ⊥])]

        ;; TODO: Should produce a procedure
        [(#%plain-lambda (x:id ...) expr)
         (make-closure this-syntax ρ)]

        ;; TODO: Remove cut?
        [(if ~! e0 e1 e2)
         (define d (eval^ #'e0 ρ))
         (cond
          [(eq? d #t) (eval^ #'e1 ρ)]
          [(eq? d #f) (eval^ #'e2 ρ)]
          [(eq? d  T) (lub (eval^ #'e1 ρ) (eval^ #'e2 ρ))]
          [(eq? d  ⊥) ⊥])]

        [(let-values ~! ([(x:id) e0] ...) e1)
         (eval^ #'(#%plain-app (#%plain-lambda (x ...) e1) e0 ...) ρ)]

        ;; TODO: Make more similar to equation (single clause?)
        [(letrec-values ~! ([(x:id) expr0] ...) expr1)
         ;; d = (eval #'expr0 (ρ-set ρ x d))
         (define ρ′
           (for/fold ([ρ ρ])
                     ([x (in-syntax #'(x ...))]
                      [expr0 (in-syntax #'(expr0 ...))])
             (define d (delay (eval^/strict expr0 ρ′)))
             (environment-set ρ x d)))
         (for ([x (in-syntax #'(x ...))])
           (define d (environment-ref ρ′ x))
           (force d))
         (eval^ #'expr1 ρ′)]))))

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