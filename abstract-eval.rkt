#lang racket

(provide abstract-eval
         abstract-eval-syntax)

(require syntax/free-vars
         syntax/parse
         "common.rkt"
         "environment.rkt")

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

(define *store* (make-hasheq))
(define current-trace (make-parameter #f))

(define-conventions id-suffix [#rx"(^|-)id$" id])
(define-conventions expr-suffix [#rx"(^|-)expr$" expr])

(define (lookup env id)
  (define loc (environment-ref env id #f))
  (dict-ref *store* loc ⊥))

(define (bind env id v)
  (define loc (gensym))
  (dict-set! *store* loc v)
  (environment-set env id loc))

(define (update! env id v)
  (define loc (environment-ref env id))
  (dict-set! *store* loc v))

(define (iterate-fixpoint proc init)
  (define result (proc init))
  (if (<=? result init)
      result
      (iterate-fixpoint proc result)))

(define (abstract-eval expr [namespace (current-namespace)])
  (define stx (namespace-syntax-introduce (datum->syntax #f expr)))
  (abstract-eval-syntax stx namespace))

(define (abstract-eval-syntax stx [namespace (current-namespace)])
  (parameterize ([current-namespace namespace]
                 [current-trace (hasheq)])
    (abstract-eval-stx (expand stx) (make-empty-environment))))

(define (abstract-eval-stx stx env)
  (syntax-parse stx
    #:conventions (id-suffix expr-suffix)
    #:literal-sets (kernel-literals)
    [(#%expression expr) (abstract-eval-stx #'expr env)]
    [(~and id (~fail #:unless (eq? (identifier-binding #'id) 'lexical)))
     (lookup env #'id)]
    [id
     (namespace-variable-value (syntax-e #'id) #t (lambda () ⊥))]
    [(#%plain-lambda (arg-id ...) body)
     (define captured-env
       (for/fold ([acc (make-empty-environment)])
                 ([var (free-vars stx #:module-bound? #t)])
         (environment-set acc var (environment-ref env var))))
     (closure stx captured-env)]
    [(case-lambda . _) (error "not implemented")]
    [(if ~! test-expr then-expr else-expr)
     (define test-val (abstract-eval-stx #'test-expr env))
     ;; FIXME: Not general!
     (cond
      [(⊥? test-val) ⊥]
      [(T? test-val)
       (lub (abstract-eval-stx #'then-expr env)
            (abstract-eval-stx #'else-expr env))]
      [(equal? test-val ((property-from-syntax) #'#f))
       (abstract-eval-stx #'else-expr env)]
      [else
       (abstract-eval-stx #'then-expr env)])]
    [(begin expr ...) (error "not implemented")]
    [(begin0 expr0 expr ...) (error "not implemented")]
    [(let-values ~! ([(id) val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-stx val-expr env)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else
       (define env-prime
         (for/fold ([acc env])
                   ([var (in-syntax #'(id ...))]
                    [val (in-stream vals)])
           (bind acc var val)))
       (abstract-eval-stx #'body env-prime)])]
    [(letrec-values ~! ([(id) val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
                 "duplicate identifier"
     (define env-prime
       (for/fold ([acc env])
                 ([var (in-syntax #'(id ...))])
         (bind acc var ⊥)))
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-stx val-expr env-prime)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else
       (for ([var (in-syntax #'(id ...))]
             [val (in-stream vals)])
         (update! env-prime var val))
       (abstract-eval-stx #'body env-prime)])]
    [(set! id expr) (error "not implemented")]
    [(quote datum) ((property-from-syntax) #'datum)]
    [(quote-syntax datum) (error "not implemented")]
    [(with-continuation-mark . _) (error "not implemented")]
    [(#%plain-app proc-expr arg-expr ...)
     (define proc (abstract-eval-stx #'proc-expr env))
     (define args
       (for/stream ([arg-expr (in-syntax #'(arg-expr ...))])
         (abstract-eval-stx arg-expr env)))
     (cond
      [(or (⊥? proc) (stream-ormap ⊥? args)) ⊥]
      [else (abstract-apply proc (stream->list args))])]
    ; FIXME: Assuming `id` is unbound, which could not be true
    [(#%top . id) ⊥]
    [(#%variable-reference . _) (error "not implemented")]))

(define (abstract-apply proc args)
  (cond
   [(T? proc) T]
   [(procedure? proc) (apply proc args)]
   [(closure? proc)
    (define lam (closure-lambda proc))
    (define/syntax-parse ((~literal #%plain-lambda) (arg-id ...) body) lam)
    (match-define (cons prev-args result)
      (hash-ref (current-trace) lam (cons #f ⊥)))
    (cond
     [(and prev-args (andmap <=? args prev-args)) result]
     [else
      (define new-args
        (if prev-args (map lub args prev-args) args))
      (define env-prime
        (for/fold ([acc (closure-environment proc)])
                  ([arg-id (in-syntax #'(arg-id ...))]
                   [arg (in-list new-args)])
          (bind acc arg-id arg)))
      (iterate-fixpoint
        (lambda (result)
          (define old-trace (current-trace))
          (define new-trace (hash-set old-trace lam (cons new-args result)))
          (parameterize ([current-trace new-trace])
            (abstract-eval-stx #'body env-prime)))
        ⊥)])] ; Maybe use previous result instead of ⊥?
  [else ⊥]))
