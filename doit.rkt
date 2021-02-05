#lang turnstile/quicklang

(require turnstile/no-unicode)
(require rackunit/turnstile)

(provide (all-defined-out) print-type)

(define-base-types Top None DynNumber)
(define-type-constructor StaNumber #:arity = 1)
(define-type-constructor -> #:arity >= 1)

(begin-for-syntax
  (define (static? τ)
    (syntax-parse τ
      [(~StaNumber _) #t]
      [_ #f]))

  (define (type->datum τ)
    (syntax->datum
      (syntax-parse τ
        [(~StaNumber 'x) #'x])))
  
  (define (type<=? t1 t2)
    (define τ1 ((current-type-eval) t1))
    (define τ2 ((current-type-eval) t2))
    (or (type=? τ1 τ2)
        (Top? τ2)
        (syntax-parse (list τ1 τ2)
          [((~StaNumber _) ~DynNumber) #t]
          [_ #f])))

  (current-typecheck-relation type<=?))

(define-typed-syntax (dyn-number) ≫
  --------
  [/- (#%datum- . 0) => DynNumber])

(define-typed-syntax #%datum
  [(_ . x:number) >>
   --------
   [/- (#%datum- . x) => (StaNumber #,(mk-type #''x))]]
  [(_ . v) >>
   --------
   [#:error (type-error #:src #'v #:msg "Unsupported literal: ~v" #'v)]])

(define-typed-syntax #%app
  [(_ e_fn e_arg ...) >>
   [/- e_fn >> e_fn- => (~-> τ_in ... τ_out)]
   #:fail-unless (stx-length=? #'(e_arg ...) #'(τ_in ...))
                 (error "Arity mismatch")
   [/- e_arg >> e_arg- (=> τ_arg) (<= τ_in)] ...
   #:when (stx-andmap static? #'(τ_arg ...))
   --------
   ;[/- (e_fn- e_arg- ...) => (StaNumber #,(mk-type #''x))]])
   [>>> 0]])

(define-typed-syntax +
  [(_ e1 e2) >>
   [/- e1 >> e1- (=> (~StaNumber _)) (⇒ τ1)]
   [/- e2 >> e2- (=> (~StaNumber _)) (⇒ τ2)]
   #:with x (+ (type->datum #'τ1) (type->datum #'τ2))
   --------
   [⊢ (+- e1- e2-) => (StaNumber #,(mk-type #''x))]]
  [(_ e1 e2) >>
   [/- e1 >> e1- <= DynNumber]
   [/- e2 >> e2- <= DynNumber]
   --------
   [/- (+- e1- e2-) => DynNumber]])
