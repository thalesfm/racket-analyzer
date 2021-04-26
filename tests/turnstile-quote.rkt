#lang turnstile/quicklang

(require turnstile/no-unicode)
(require rackunit/turnstile)

(provide (all-defined-out))

(define-base-types Any None AnyNumber AnyString)
(define-type-constructor Number #:arity = 1)
(define-type-constructor String #:arity = 1)

(begin-for-syntax
  (define (type->datum τ)
    (syntax->datum
      (syntax-parse τ #:literals (#%datum-)
        [(~Number 'x) #'x]
        [(~String 'str) #'str]))))

(define-typed-syntax (test) ≫
  --------
  [⊢ (#%datum- . 0) ⇒ (Number Any)])

(define-typed-syntax #%datum
  [(_ . x:number) ≫
   #:with τ_x (mk-type #''x)
   --------
   [⊢ (#%datum- . x) ⇒ (Number τ_x)]]
  [(_ . str:string) ≫
   #:with τ_str (mk-type #''str)
   --------
   [⊢ (#%datum- . str) ⇒ (String τ_str)]]
  [(_ . v) ≫
   --------
   [#:error (type-error #:src #'v #:msg "Unsupported literal: ~v" #'v)]])

(define-typed-syntax (+ e1 e2) ≫
  [⊢ e1 ≫ e1- (⇒ (~Number _)) (⇒ τ1)]
  [⊢ e2 ≫ e2- (⇒ (~Number _)) (⇒ τ2)]
  #:do [(displayln (get-orig #'τ1))]
  #:with t (+ (type->datum #'τ1) (type->datum #'τ2))
  #:with τ (mk-type #''t)
  --------
  [⊢ (+- e1- e2-) ⇒ (Number τ)])