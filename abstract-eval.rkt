#lang racket

(provide abstract-eval
         abstract-eval-syntax)

(require syntax/free-vars
         syntax/parse
         "common.rkt"
         "environment.rkt"
         "store.rkt")

(struct closure (lambda environment))

; TODO: Double check there isn't a risk that this goes into an
; infinite loop when both arguments are recursive procedures
(define (closure<=? clo1 clo2 [recur-proc <=?])
  (and (eq? (closure-lambda clo1)
            (closure-lambda clo2))
       (environment<=? (closure-environment clo1)
                       (closure-environment clo2)
                       recur-proc)))

; TODO: Double check there isn't a risk that this goes into an
; infinite loop when both arguments are recursive procedures
(define (closure-lub clo1 clo2 [recur-proc lub])
  (if (eq? (closure-lambda clo1)
           (closure-lambda clo2))
      (closure (closure-lambda clo1)
               (environment-lub (closure-environment clo1)
                                (closure-environment clo2)
                                recur-proc))
      T))

(define (<=? v1 v2)
  (cond
   [(eq? v1 v2) #t]
   [(T? v1) #f]
   [(T? v2) #t]
   [(⊥? v1) #t]
   [(⊥? v2) #f]
   [(or  (procedure? v1) (procedure? v2)) #f]
   [(and (closure? v1) (closure? v2)) (closure<=? v1 v2)]
   [(or  (closure? v1) (closure? v2)) #f]
   [else ((property-stronger?) v1 v2 <=?)]))

(define (lub v1 v2)
  (cond
   [(eq? v1 v2) v1]
   [(or (T? v1) (T? v2)) T]
   [(⊥? v1) v2]
   [(⊥? v2) v1]
   [(or  (procedure? v1) (procedure? v2)) T]
   [(and (closure? v1) (closure? v2)) (closure-lub v1 v2)]
   [(or  (closure? v1) (closure? v2)) T]
   [else ((property-combine) v1 v2 lub)]))

(define current-trace (make-parameter #f))
(define-conventions id-suffix [#rx"(^|-)id$" id])
(define-conventions expr-suffix [#rx"(^|-)expr$" expr])

(define (iterate-fixpoint proc init)
  (define result (proc init))
  (if (<=? result init)
      result
      (iterate-fixpoint proc result)))

(define (abstract-eval top-level-form [namespace (current-namespace)])
  (abstract-eval-syntax
   (namespace-syntax-introduce
    (datum->syntax #f top-level-form))
   namespace))

(define (abstract-eval-syntax top-level-form [namespace (current-namespace)])
  (parameterize ([current-namespace namespace]
                 [current-trace (hasheq)])
    (abstract-eval-kernel-syntax (expand top-level-form)
                                 (make-empty-environment))))

(define (abstract-eval-kernel-syntax kernel-form env)
  (syntax-parse kernel-form
    #:literal-sets (kernel-literals)
    [(#%expression expr) (abstract-eval-expr #'expr env)]
    [(module . _) (error "not implemented")]
    [(begin . _) (error "not implemented")]
    [(begin-for-syntax . _) (error "not implemented")]
    [(define-values . _) (error "not implemented")]
    [(define-syntaxes . _) (error "not implemented")]
    [(#%require . _) (error "not implemented")]
    [expr (abstract-eval-expr #'expr env)]))

(define (abstract-eval-expr stx env)
  (syntax-parse stx
    #:conventions (id-suffix expr-suffix)
    #:literal-sets (kernel-literals)

    [(~and id (~fail #:unless (eq? (identifier-binding #'id) 'lexical)))
     (store-ref (environment-ref env #'id #f) ⊥)]
    [id ; Top-level, module-level, or unbound identifier
     (namespace-variable-value (syntax-e #'id) #t (lambda () ⊥))]

    [(#%plain-lambda (id ...) body)
     (define captured-env
       (for/fold ([acc (make-empty-environment)])
                 ([id (free-vars stx #:module-bound? #t)])
         (environment-set acc id (environment-ref env id))))
     (closure stx captured-env)]

    [(case-lambda . _)
     (error "not implemented")]

    [(if ~! test-expr then-expr else-expr)
     (define test-val (abstract-eval-expr #'test-expr env))
     ;; FIXME: Not general!
     (cond
      [(⊥? test-val) ⊥]
      [(T? test-val)
       (lub (abstract-eval-expr #'then-expr env)
            (abstract-eval-expr #'else-expr env))]
      [(equal? test-val ((property-from-syntax) #'#f))
       (abstract-eval-expr #'else-expr env)]
      [else (abstract-eval-expr #'then-expr env)])]

    [(begin expr ...)
     (for/last ([expr (in-syntax #'(expr ...))])
       (abstract-eval-expr expr env))]

    [(begin0 expr0 expr ...)
     (for/first ([expr (in-syntax #'(expr0 expr ...))])
       (abstract-eval-expr expr env))]

    [(let-values ~! ([(id) val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-expr val-expr env)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else
       (define env-prime
         (for/fold ([acc env])
                   ([id (in-syntax #'(id ...))]
                    [v (in-stream vals)])
           (define loc (gensym))
           (store-set! loc v)
           (environment-set acc id loc)))
       (abstract-eval-expr #'body env-prime)])]

    [(letrec-values ~! ([(id) val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (define env-prime
       (for/fold ([acc env])
                 ([id (in-syntax #'(id ...))])
         (define loc (gensym))
         (environment-set acc id loc)))
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-expr val-expr env-prime)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else
       (for ([id (in-syntax #'(id ...))]
             [v (in-stream vals)])
         (define loc (environment-ref env-prime id))
         (store-set! loc v))
       (abstract-eval-expr #'body env-prime)])]

    [(set! id expr) (error "not implemented")]
    ;  (define loc (environment-ref env #'id))
    ;  (define old-v (store-ref loc))
    ;  (define new-v (abstract-eval-epxr #'expr env))
    ;  (dynamic-wind
    ;   (lambda () (store-set! loc new-v))
    ;   (lambda () ???)
    ;   (lambda () (store-set! loc old-v)))]

    [(quote datum) ((property-from-syntax) #'datum)]

    [(quote-syntax datum) (error "not implemented")]
    [(quote-syntax datum #:local) (error "not implemented")]
    [(with-continuation-mark . _) (error "not implemented")]

    [(#%plain-app proc-expr arg-expr ...)
     (define proc (abstract-eval-expr #'proc-expr env))
     (define args
       (for/stream ([arg-expr (in-syntax #'(arg-expr ...))])
         (abstract-eval-expr arg-expr env)))
     (cond
      [(or (⊥? proc) (stream-ormap ⊥? args)) ⊥]
      [else (abstract-apply proc (stream->list args))])]

    [(#%top . id) (abstract-eval-expr #'id env)]

    [(#%variable-reference id) (error "not implemented")]
    [(#%variable-reference (#%top . id)) (error "not implemented")]
    [(#%variable-reference) (error "not implemented")]))

(define (abstract-apply proc args)
  (cond
   [(T? proc) T]
   [(procedure? proc) (apply proc args)]
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
          (store-set! loc v)
          (environment-set acc id loc)))
      (iterate-fixpoint
       (lambda (result)
         (define trace-prime
           (hash-set (current-trace) lam
                     (list args-prime result backtrack)))
         (parameterize ([current-trace trace-prime])
           (abstract-eval-expr #'body env-prime)))
      ⊥)]
     ; Subsequent call to procedure with compatible arguments
     [(andmap <=? args prev-args) result]
     ; Subsequent call to procedure, but arguments need to be combined
     [else (backtrack (map lub args prev-args))])]
   [else ⊥]))
