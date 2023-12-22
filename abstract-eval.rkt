#lang racket

(provide abstract-eval
         abstract-eval-syntax)

(require syntax/parse
         "domain.rkt"
         "environment.rkt"
         "primitives.rkt"
         "store.rkt")

(define (abstract-eval top-level-form [namespace (make-primitive-namespace)])
  (parameterize ([current-namespace namespace])
    (abstract-eval-syntax
     (namespace-syntax-introduce
      (datum->syntax #f top-level-form)))))

(define (abstract-eval-syntax top-level-form [namespace (make-primitive-namespace)])
  (parameterize ([current-namespace namespace])
    (abstract-eval-kernel-syntax (expand top-level-form))))

(define-conventions id-suffix [#rx"(^|-)id$" id])
(define-conventions expr-suffix [#rx"(^|-)expr$" expr])

(define (raise-user-error message)
  (raise (exn:fail:user message (current-continuation-marks))))

(define (raise-undefined-error id)
  (raise-user-error
   (format "~a: undefined or used before initialization" (syntax-e id))))

(define (catch-user-error thunk)
  (with-handlers ([exn:fail:user? (λ (exn) ⊥)])
    (thunk)))

(define (re-raise-user-error d)
  (if (⊥? d) (raise-user-error "user error") d))

(define (abstract-eval-kernel-syntax stx [ρ (make-ρ)])
  (define aeval abstract-eval-kernel-syntax)
  (syntax-parse stx
    #:conventions (id-suffix expr-suffix)
    #:literal-sets (kernel-literals)

    [(~and id (~fail #:unless (eq? (identifier-binding #'id) 'lexical))) ; locally bound identifier
     (define v (ρ-ref ρ #'id (lambda () (raise-undefined-error #'id))))
     (if (and (promise? v) (promise-running? v))
         (raise-undefined-error #'id)
         (force v))]

    [id ; top-level, module-level, or unbound identifier
     (namespace-variable-value (syntax-e #'id)
                               #t
                               (lambda () (raise-undefined-error #'id)))]

    [(#%plain-lambda (_id ...) _body)
     (make-closure this-syntax ρ)]

    [(if ~! test-expr then-expr else-expr)
     (define v (aeval #'test-expr ρ))
     (cond
      [(T?  v)
       (re-raise-user-error
        (lub (catch-user-error (λ () (aeval #'then-expr ρ)))
             (catch-user-error (λ () (aeval #'else-expr ρ)))))]
      [(eq? v #f) (aeval #'else-expr ρ)]
      [else       (aeval #'then-expr ρ)])]
  
    [(let-values ~! ([(id) val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (define val-list
       (for/list ([expr (in-syntax #'(val-expr ...))])
         (aeval expr ρ)))
     (define ρ′
       (for/fold ([ρ ρ])
                 ([x (in-syntax #'(id ...))]
                  [v (in-list val-list)])
         (ρ-set ρ x v)))
     (aeval #'body ρ′)]
  
    [(letrec-values ~! ([(id) val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (define val-list
       (for/list ([expr (in-syntax #'(val-expr ...))])
         (delay (aeval expr ρ′))))
     (define ρ′
       (for/fold ([ρ ρ])
                 ([x (in-syntax #'(id ...))]
                  [v (in-list val-list)])
         (ρ-set ρ x v)))
     (aeval #'body ρ′)]
  
    [(quote datum)
     #:fail-when (not (in-domain? (syntax->datum #'datum)))
                 "unexpected datum"
     (syntax-e #'datum)]
  
    [(#%plain-app proc-expr arg-expr ...)
     (define proc (aeval #'proc-expr ρ))
     (define arg-list
       (for/list ([expr (in-syntax #'(arg-expr ...))])
         (aeval expr ρ)))
     (cond
      [(T? proc) T]
      [else (abstract-apply proc arg-list)])]
    
    [(#%expression expr) (aeval #'expr ρ)]
  
    ;; Not implemented: module, begin, begin-for-syntax, define-values,
    ;; define-syntaxes, #%require, case-lambda, begin, begin0, set!, quote-syntax,
    ;; with-continuation-mark, #%variable-reference, #%top

    [(kw . _)
     (displayln this-syntax)
     (error (format "~a: not implemented" (syntax-e #'kw)))]))

(define (abstract-apply proc args)
  (cond
   [(closure? proc)
    (define/syntax-parse (_ (id ...) body) (closure-source-syntax proc))
    (define ρ′
      (for/fold ([ρ′ (closure-environment proc)])
                ([x (in-syntax #'(id ...))]
                 [v (in-list args)])
        (ρ-set ρ′ x v)))
    (abstract-eval-kernel-syntax #'body ρ′)]
   [(procedure? proc)
    (apply proc args)]
   [else
    (raise-user-error (format "application: not a procedure;\n  given: ~a" proc))]))

#|
  [(closure? proc)
    (define lam (closure-lambda proc))
    (define/syntax-parse ((~literal #%plain-lambda) (id ...) body) lam)
    (match-define (list prev-args result backtrack)
      (hash-ref (current-trace) lam (list #f ⊥ 'no-backtrack)))
    (cond
     ; First call to procedure in the call stack
     [(not prev-args)
      (define backtrack #f)
      (define args-prime
        (let/cc k (set! backtrack k) args))
      (define env-prime
        (for/fold ([acc (closure-environment proc)])
                  ([id (in-syntax #'(id ...))]
                   [v (in-list args-prime)])
          (define loc (gensym))
          (store-set! store loc v)
          (environment-set acc id loc)))
      (iterate-fixpoint
       (lambda (result)
         (define trace-prime
           (hash-set (current-trace) lam
                     (list args-prime result backtrack)))
         (parameterize ([current-trace trace-prime])
           (abstract-eval-kernel-syntax #'body env-prime)))
      ⊥)]
     ; Subsequent call to procedure with compatible arguments
     [(andmap <=? args prev-args) result]
     ; Subsequent call to procedure, but arguments need to be combined
     [else (backtrack (map lub args prev-args))])]
   [else ⊥]))
|#