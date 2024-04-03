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

;; TODO: Rename to `abstract-eval` if possible
(define (abstract-eval-kernel-syntax expr [ρ (make-ρ)])
  (let eval ([expr expr] [ρ ρ])
    (let/ec break
      (define (eval/strict expr ρ)
        (define d (eval expr ρ))
        (if (⊥? d) (break d) d))
      (syntax-parse expr
        #:literal-sets (kernel-literals)

        ;; TODO: Make more similar to equation
        [(~and x:id (~fail #:unless (eq? (identifier-binding #'x) 'lexical)))
         (define d (ρ-ref ρ #'x (⊥ (syntax-e #'x) "unbound identifier")))
         (if (and (promise? d) (not (promise-forced? d)))
             (⊥ (syntax-e #'x) "undefined;\n cannot use before initialization")
             (force d))]

        ;; TODO: Make more similar to equation
        [(quote k)
         #:fail-when (not (in-domain? (syntax->datum #'k))) "datum not in domain"
         (syntax-e #'k)]

        ;; TODO: Make more similar to equation
        [o:id
         (namespace-variable-value (syntax-e #'o)
                                   #t
                                   (lambda () (⊥ (syntax-e #'o) "unbound identifier")))]

        [(#%plain-lambda (x:id ...) expr)
         (make-closure this-syntax ρ)]

        [(if ~! expr0 expr1 expr2)
         (define d (eval/strict #'expr0 ρ))
         (cond
          [(T? d) (lub (eval #'expr1 ρ)
                       (eval #'expr2 ρ))]
          [(eq? d #f)  (eval #'expr2 ρ)]
          [else        (eval #'expr1 ρ)])]

        ;; TODO: Make more similar to equation (single clause?)
        [(let-values ~! ([(x:id) expr0] ...) expr1)
         (define ρ′
           (for/fold ([ρ ρ])
                     ([x (in-syntax #'(x ...))]
                      [expr0 (in-syntax #'(expr0 ...))])
             (ρ-set ρ x (eval/strict expr0 ρ))))
         (eval #'expr1 ρ′)]

        ;; TODO: Make more similar to equation (single clause?)
        [(letrec-values ~! ([(x:id) expr0] ...) expr1)
         (define ρ′
           (for/fold ([ρ ρ])
                     ([x (in-syntax #'(x ...))]
                      [expr0 (in-syntax #'(expr0 ...))])
             (define d (delay (eval/strict expr0 ρ′)))
             (ρ-set ρ x d)))
         (for ([x (in-syntax #'(x ...))])
           (define d (ρ-ref ρ′ x))
           (force d))
         (eval #'expr1 ρ′)]

        [(#%plain-app expr0 expr1 ...)
         (define fun (eval/strict #'expr0 ρ))
         (define arg-list
           (for/list ([expr1 (in-syntax #'(expr1 ...))])
             (eval/strict expr1 ρ)))
         (abstract-apply fun arg-list)]

        ;; [(#%expression expr) (eval #'expr ρ)]

        [_
         #:fail-when #t "not implemented"
         (assert-unreachable)]))))

;; TODO: Make more similiar to equations
;; TODO: Fix infinite loop
(define (abstract-apply proc args)
  (cond
   [(T? proc) T] ;; Remove if possible (define T as a proc)
   [(closure? proc)
    (define/syntax-parse (_ (x ...) expr) (closure-source-syntax proc))
    (define args′ (continuation-mark-set-first #f proc))
    (define args″ (if args′ (map lub args args′) args))
    (define ρ′
      (for/fold ([ρ (closure-environment proc)])
                ([x (in-syntax #'(x ...))]
                 [d (in-list args″)])
        (ρ-set ρ x d)))
    (with-continuation-mark proc args″
      (abstract-eval-kernel-syntax #'expr ρ′))]
   [(procedure? proc)
    (apply proc args)]
   [else (⊥ 'application "not a procedure;\n  given: ~a" proc)]))