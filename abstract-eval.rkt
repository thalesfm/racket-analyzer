#lang racket

(provide abstract-eval
         abstract-eval-syntax)

(require racket/syntax
         syntax/free-vars
         syntax/parse
         "domain.rkt"
         "environment.rkt"
         "store.rkt")

;(define current-trace (make-parameter #f))

(define-conventions id-suffix [#rx"(^|-)id$" id])
(define-conventions expr-suffix [#rx"(^|-)expr$" expr])

(define (iterate-fixpoint proc init)
  (define next (proc init))
  (if (<=? next init)
      next
      (iterate-fixpoint proc next)))

(define (abstract-eval top-level-form [namespace (current-namespace)])
  (parameterize ([current-namespace namespace])
    (abstract-eval-syntax
     (namespace-syntax-introduce
      (datum->syntax #f top-level-form)))))

(define (abstract-eval-syntax top-level-form [namespace (current-namespace)])
  (parameterize ([current-namespace namespace])
    (abstract-eval-kernel-syntax (expand top-level-form))))

;; (define (abstract-eval-handler ...) ...)

(define (abstract-eval-kernel-syntax top-level-form)
  (let recur ([stx top-level-form]
              [ρ empty-environment]
              [σ (make-store)])
    (syntax-parse stx
      #:conventions (id-suffix expr-suffix)
      #:literal-sets (kernel-literals)

      [(#%expression expr) (recur #'expr ρ σ)]

      [(~and id (~fail #:unless (eq? (identifier-binding #'id) 'lexical)))
       (store-ref σ (environment-ref ρ #'id #f) ⊥)]

      ; Top-level, module-level, or unbound identifier
      [id (namespace-variable-value (syntax-e #'id) #t (lambda () ⊥))]

      [(#%plain-lambda (_id ...) _body)
       (letrec ([proc (make-introspectable-procedure
                       this-syntax
                       ρ
                       (lambda args
                         (apply-introspectable-procedure proc args σ)))])
         proc)]

      [(if ~! test-expr then-expr else-expr)
       (define test-val (recur #'test-expr ρ σ))
       ;; FIXME: Not general!
       (cond
        [(⊥? test-val) ⊥]
        [(T? test-val)
         (lub (recur #'then-expr ρ σ)
              (recur #'else-expr ρ σ))]
        [(equal? test-val #f) (recur #'else-expr ρ σ)]
        [else (recur #'then-expr ρ σ)])]
  
      [(let-values ~! ([(id) val-expr] ...) body)
       #:fail-when (check-duplicate-identifier (syntax->list #'(id ...))) "duplicate identifier"
       (define vals
         (for/stream ([val-expr (in-syntax #'(val-expr ...))])
           (recur val-expr ρ σ)))
       (cond
        [(stream-ormap ⊥? vals) ⊥]
        [else
         (define env-prime
           (for/fold ([acc ρ])
                     ([id (in-syntax #'(id ...))]
                      [v (in-stream vals)])
             (define loc (gensym))
             (store-set! σ loc v)
             (environment-set acc id loc)))
         (recur #'body env-prime σ)])]
  
      [(letrec-values ~! ([(id) val-expr] ...) body)
       #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                   "duplicate identifier"
       (define env-prime
         (for/fold ([acc ρ])
                   ([id (in-syntax #'(id ...))])
           (define loc (gensym))
           (environment-set acc id loc)))
       (define vals
         (for/stream ([val-expr (in-syntax #'(val-expr ...))])
           (recur val-expr env-prime σ)))
       (cond
        [(stream-ormap ⊥? vals) ⊥]
        [else
         (for ([id (in-syntax #'(id ...))]
               [v (in-stream vals)])
           (define loc (environment-ref env-prime id))
           (store-set! σ loc v))
         (recur #'body env-prime σ)])]
  
      [(quote datum)
       #:fail-when (not (in-domain? (syntax->datum #'datum))) "unexpected datum"
       (syntax-e #'datum)]
  
      [(#%plain-app proc-expr arg-expr ...)
       (define proc (recur #'proc-expr ρ σ))
       (define args
         (for/stream ([arg-expr (in-syntax #'(arg-expr ...))])
           (recur arg-expr ρ σ)))
       (cond
        [(or (⊥? proc) (stream-ormap ⊥? args)) ⊥]
        [(T? proc) T]
        ;; TODO: Catch errors?
        [else (apply proc (stream->list args))])]
  
      [(#%top . id) (recur #'id ρ σ)]
  
      ; Not implemented: module, begin, begin-for-syntax, define-values, define-syntaxes, #%require
      ; case-lambda, begin, begin0, set!, quote-syntax, with-continuation-mark, #%variable-reference

      [_ (error "not implemented")])))

;; FIXME: Can't take the lub without the store
(define (apply-introspectable-procedure proc args σ)
  (define lam-expr (introspectable-procedure-source proc))
  (define ρ (introspectable-procedure-closure proc))
  (with-syntax* ([(_ (arg-id ...) body) lam-expr]
                 [(arg-v ...) args]
                 [(free-id ...) (free-vars lam-expr)]
                 [(env-v ...) (for/list ([id (in-syntax #'(free-id ...))])
                                  (dict-ref σ (dict-ref ρ id #f) ⊥))])
    (eval #'(let ([free-id env-v] ... [arg-id arg-v] ...) body))))

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