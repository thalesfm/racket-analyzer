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

(define (abstract-eval-kernel-syntax stx [ρ (make-ρ)])
  (let aeval ([stx stx] [ρ ρ])
    (let/ec break
      (define (aeval/strict stx ρ)
        (define d (aeval stx ρ))
        (if (⊥? d) (break d) d))
      (syntax-parse stx
        #:literal-sets (kernel-literals)

        [(~and id:id (~fail #:unless (eq? (identifier-binding #'id) 'lexical))) ; locally bound identifier
         (define v (ρ-ref ρ #'id (⊥ (syntax-e #'id) "unbound identifier")))
         (if (and (promise? v) (not (promise-forced? v)))
             (⊥ (syntax-e #'id) "undefined;\n cannot use before initialization")
             (force v))]

        [id:id ; top-level, module-level, or unbound identifier
         (namespace-variable-value (syntax-e #'id)
                                   #t
                                   (lambda () (⊥ (syntax-e #'id) "unbound identifier")))]

        [(#%plain-lambda (_id:id ...) _body)
         (make-closure this-syntax ρ)]

        [(if ~! test-expr then-expr else-expr)
         (define v (aeval/strict #'test-expr ρ))
         (cond
          [(T? v) (lub (aeval #'then-expr ρ)
                       (aeval #'else-expr ρ))]
          [(eq? v #f)  (aeval #'else-expr ρ)]
          [else        (aeval #'then-expr ρ)])]

        [(let-values ~! ([(id:id) val-expr] ...) body)
         (define vv
           (for/vector ([expr (in-syntax #'(val-expr ...))])
             (aeval/strict expr ρ)))
         (define ρ′
           (for/fold ([ρ ρ])
                     ([x (in-syntax #'(id ...))]
                      [v (in-vector vv)])
             (ρ-set ρ x v)))
         (aeval #'body ρ′)]

        [(letrec-values ~! ([(id:id) val-expr] ...) body)
         (define vv
           (for/vector ([expr (in-syntax #'(val-expr ...))])
             (delay (aeval/strict expr ρ′))))
         (define ρ′
           (for/fold ([ρ ρ])
                     ([x (in-syntax #'(id ...))]
                      [v (in-vector vv)])
             (ρ-set ρ x v)))
         (for ([v (in-vector vv)])
           (force v))
         (aeval #'body ρ′)]

        [(quote datum)
         #:fail-when (not (in-domain? (syntax->datum #'datum)))
                     "datum not in domain"
         (syntax-e #'datum)]

        [(#%plain-app proc-expr arg-expr ...)
         (define proc (aeval/strict #'proc-expr ρ))
         (define arg-list
           (for/list ([expr (in-syntax #'(arg-expr ...))])
             (aeval/strict expr ρ)))
         (abstract-apply proc arg-list)]

        [(#%expression expr) (aeval #'expr ρ)]

        ;; Not implemented: module, begin, begin-for-syntax, define-values,
        ;; define-syntaxes, #%require, case-lambda, begin, begin0, set!, quote-syntax,
        ;; with-continuation-mark, #%variable-reference, #%top

        [_
         #:fail-when #t "not implemented"
         (assert-unreachable)]))))

(define (abstract-apply proc args)
  (cond
   [(T? proc) T]
   [(closure? proc)
    (printf "(~a ~a)" proc args)
    (define/syntax-parse (_ (id ...) body) (closure-source-syntax proc))
    (define args′ (continuation-mark-set-first #f proc))
    (define args″ (if args′ (map lub args args′) args))
    (when args′
      (printf " <= (~a ~a)" proc args″))
    (printf "\n")
    (define ρ′
      (for/fold ([ρ (closure-environment proc)])
                ([x (in-syntax #'(id ...))]
                 [v (in-list args″)])
        (ρ-set ρ x v)))
    (with-continuation-mark proc args″
      (abstract-eval-kernel-syntax #'body ρ′))]
   [(procedure? proc)
    (apply proc args)]
   [else (⊥ 'application "not a procedure;\n  given: ~a" proc)]))

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