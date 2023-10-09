#lang racket

(provide abstract-eval
         abstract-eval-syntax)

(require syntax/parse
         "common.rkt"
         "environment.rkt"
         "free-vars.rkt"
         "syntax.rkt"
         "types.rkt")

(struct closure (lambda environment) #:transparent)

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

; HACK
(define (constant? v)
  (if (Pairof? v)
      (and (Literal? (car v))
           (Literal? (cdr v)))
      (Literal? v)))

; HACK
(define (lift proc)
  (lambda args
    (if (andmap constant? args)
        (datum->type (apply proc (map Literal-value args)))
        T)))

; HACK
(define (make-base-environment)
  (let* ([env (make-empty-environment)]
         [env (environment-set env #'+ (lift +))]
         [env (environment-set env #'- (lift -))]
         [env (environment-set env #'* (lift *))]
         [env (environment-set env #'/ (lift /))]
         [env (environment-set env #'= (lift =))]
         [env (environment-set env #'map (lift map))]
         [env (environment-set env #'read (λ () T))]
         [env (environment-set env #'error (λ () ⊥))])
    env))

(define (abstract-eval expr [namespace (current-namespace)])
  (define stx (namespace-syntax-introduce (datum->syntax #f expr) namespace))
  (define env (make-base-environment))
  (abstract-eval-syntax stx env))

(define (abstract-eval-syntax stx env [trace (hasheq)])
  (syntax-parse stx
    #:conventions (conventions)
    #:literal-sets (literal-set)
    [var (environment-ref env #'var ⊥)]
    [lit:literal ((property-from-syntax) #'lit.datum)]
    [lam:lambda-expr
     (define captured-env
       (for/fold ([acc (make-empty-environment)])
                 ([var (free-vars #'lam)])
         (environment-set acc var (environment-ref env var ⊥))))
     (closure #'lam captured-env)]
    [(if ~! test-expr then-expr else-expr)
     (define test-val (abstract-eval-syntax #'test-expr env trace))
     ;; FIXME: Not general!
     (cond
      [(⊥? test-val) ⊥]
      [(T? test-val)
       (lub (abstract-eval-syntax #'then-expr env trace)
            (abstract-eval-syntax #'else-expr env trace))]
      [(equal? test-val ((property-from-syntax) #'#f))
       (abstract-eval-syntax #'else-expr env trace)]
      [else
       (abstract-eval-syntax #'then-expr env trace)])]
    [(let ~! ([var val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-syntax val-expr env trace)))
     (define (env-prime)
       (for/fold ([acc env])
                 ([var (in-syntax #'(var ...))]
                  [val (in-stream vals)])
         (environment-set acc var val)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else (abstract-eval-syntax #'body (env-prime) trace)])]
    [(letrec ~! ([var val-expr] ...) body)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "duplicate identifier"
     (define env-prime
       (for/fold ([acc env])
                 ([var (in-syntax #'(var ...))])
         (environment-set acc var (make-placeholder ⊥))))
     (define vals
       (for/stream ([val-expr (in-syntax #'(val-expr ...))])
         (abstract-eval-syntax val-expr env-prime trace)))
     (cond
      [(stream-ormap ⊥? vals) ⊥]
      [else
       (for ([var (in-syntax #'(var ...))]
             [val (in-stream vals)])
         (placeholder-set! (environment-ref env-prime var) val))
       (abstract-eval-syntax #'body (make-reader-graph env-prime) trace)])]
    [(proc-expr arg-expr ...)
     (define proc (abstract-eval-syntax #'proc-expr env trace))
     (define args
       (for/stream ([arg-expr (in-syntax #'(arg-expr ...))])
         (abstract-eval-syntax arg-expr env trace)))
     (cond
      [(or (⊥? proc) (stream-ormap ⊥? args)) ⊥]
      [else (abstract-apply proc (stream->list args) trace)])]))

(define (iterate-fixpoint proc init)
  (define result (proc init))
  (if (<=? result init)
      result
      (iterate-fixpoint proc result)))

(define (abstract-apply proc args trace)
  (cond
   [(T? proc) T]
   [(procedure? proc) (apply proc args)]
   [(closure? proc)
    (define/syntax-parse lam:lambda-expr
      (closure-lambda proc))
    (match-define (cons prev-args result)
      (hash-ref trace #'lam (cons #f ⊥)))
    (cond
     [(and prev-args (andmap <=? args prev-args)) result]
     [else
      (define new-args
        (if prev-args (map lub args prev-args) args))
      (define env-prime
        (for/fold ([acc (closure-environment proc)])
                  ([arg-id (in-syntax #'(lam.arg-id ...))]
                   [arg (in-list new-args)])
          (environment-set acc arg-id arg)))
      (iterate-fixpoint
        (lambda (result)
          (define new-trace (hash-set trace #'lam (cons new-args result)))
          (abstract-eval-syntax #'lam.body env-prime new-trace))
        ⊥)])] ; Maybe use previous result instead of ⊥?
  [else ⊥]))
