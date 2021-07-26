#lang turnstile

(provide (type-out ⊤ ⊥ Number Real Integer Boolean String Cons)
         (for-syntax ⊤ ⊥ Number Real Integer Boolean String Cons))

(provide (for-syntax ⊑ ⊔ seq project static? datum->type type->datum))

;; Top, Empty, and Bottom

(define-base-type ⊤)
(define-base-type ∅)
(define-type-constructor ⊔-⊥)

(define-syntax (⊥ stx)
  (syntax-case stx ()
    [⊥ (identifier? #'⊥) #'(⊔-⊥ ∅)]))

(begin-for-syntax
  (define (⊥? τ)
    (syntax-parse τ
      [(~⊔-⊥ ~∅) #t]
      [_ #f]))

  (define-syntax ~⊥
    (pattern-expander
      (lambda (stx)
        (syntax-case stx ()
          [~⊥ (identifier? #'~⊥) #'(~⊔-⊥ ~∅)]))))

  (define ⊤ ((current-type-eval) #'⊤))
  (define ∅ ((current-type-eval) #'∅))
  (define ⊥ ((current-type-eval) #'⊥))

  (define (⊔-⊥ τ)
    (define ⊔-τ-⊥
      (syntax-parse τ
        [(~⊔-⊥ _) #'τ]
        [_ #`(⊔-⊥ #,τ)]))
    ((current-type-eval) ⊔-τ-⊥))

  (define (project τ)
    (syntax-parse τ
      [(~⊔-⊥ inner-τ) #'inner-τ]
      [_ τ])))

;; Numeric Types

(define-base-type Number)
(define-base-type Real)
(define-base-type Integer)

(begin-for-syntax
  (define Number ((current-type-eval) #'Number))
  (define Real ((current-type-eval) #'Real))
  (define Integer ((current-type-eval) #'Integer)))

;; Other Base Types

(define-base-type Boolean)
(define-base-type String)

(begin-for-syntax
  (define Boolean ((current-type-eval) #'Boolean))
  (define String ((current-type-eval) #'String)))

;; Pairs and Lists

(define-base-type Null)
(define-type-constructor Cons #:arity = 2)

(begin-for-syntax
  (define Null ((current-type-eval) #'Null))
  
  (define (Cons τ_a τ_d)
    (define τ
      (with-syntax ([τ_a (project τ_a)]
                    [τ_d (project τ_d)])
        (define static (and (static? #'τ_a) (static? #'τ_d)))
        (define τ (syntax-property #'(Cons τ_a τ_d) 'static? static))
        ((current-type-eval) τ)))
     (seq τ_a τ_d τ)))

;; Type Utilities

(begin-for-syntax
  (define (static? τ)
    (syntax-parse (project τ) #:literals (quote)
      [(quote v) #t]
      [(~Cons _ _) (syntax-property τ 'static?)]
      [_ #f]))
  
  (define (valid-literal? v)
    (or (exact-integer? v)
        (real? v)
        (boolean? v)
        (string? v)))
  
  (define (datum->type v)
    (cond
      [(pair? v) (Cons (datum->type (car v)) (datum->type (cdr v)))]
      [(null? v) Null]
      [(valid-literal? v) (mk-type #`(quote #,v))]
      [else (error (format "unsupported literal: ~a" v))]))
  
  (define (type->datum τ)
    (syntax-parse (project τ) #:literals (quote)
      [(quote v) (syntax-e #'v)]
      [(~Cons τ_a τ_d) (cons (type->datum #'τ_a) (type->datum #'τ_d))]
      [else (error "unreachable")]))

  (define (seq . vs)
    (if (ormap ⊔-⊥? vs)
        (⊔-⊥ (last vs))
        (last vs)))

  (define (supertype τ)
    (define super-τ
      (syntax-parse (project τ)
        [~⊤ #f]
        [~∅ #f]
        [~Number ⊤]
        [~Real Number]
        [~Integer Real]
        [~Boolean ⊤]
        [~String ⊤]
        [~Null ⊤]
        [(~Cons _ _) #f]
        [_ #:when (not (static? τ)) #f]
        [_ #:when (exact-integer? (type->datum τ)) Integer]
        [_ #:when (real? (type->datum τ)) Real]
        [_ #:when (boolean? (type->datum τ)) Boolean]
        [_ #:when (string? (type->datum τ)) String]
        [_ (error "unreachable")]))
    (if super-τ (seq τ super-τ) #f))

  (define (⊑ τ1 τ2)
    (syntax-parse #`(#,(project τ1) #,(project τ2))
      [_ #:when (and (⊔-⊥? τ1) (not (⊔-⊥? τ2))) #f]
      [(_ ~⊤) #t]
      [(~∅ _) #t]
      [((~Cons τ1_a τ1_d) (~Cons τ2_a τ2_d))
       (and (⊑ #'τ1_a #'τ2_a)
            (⊑ #'τ1_d #'τ2_d))]
      [(τ1 τ2) #:when (type=? #'τ1 #'τ2) #t]
      [(τ1 τ2)
       #:with super-τ1 (supertype #'τ1)
       #:when (syntax-e #'super-τ1)
       (⊑ #'super-τ1 #'τ2)]
      [else #f]))

  (define (⊔ τ1 τ2)
    (define τ
      (syntax-parse #`(#,(project τ1) #,(project τ2))
        [(~⊤ _) ⊤]
        [(_ ~⊤) ⊤]
        [(~∅ τ2) #'τ2]
        [(τ1 ~∅) #'τ1]
        [((~Cons τ1_a τ1_d) (~Cons τ2_a τ2_d))
         (Cons (⊔ #'τ1_a #'τ2_a)
               (⊔ #'τ1_d #'τ2_d))]
        [(τ1 τ2) #:when (type=? #'τ1 #'τ2) #'τ1]
        [(τ1 τ2)
         #:when (⊑ #'τ1 #'τ2)
         #:with super-τ1 (supertype #'τ1)
         #:when (syntax-e #'super-τ1)
         (⊔ #'super-τ1 #'τ2)]
        [(τ1 τ2)
         #:when (⊑ #'τ2 #'τ1)
         #:with super-τ2 (supertype #'τ2)
         #:when (syntax-e #'super-τ2)
         (⊔ #'τ1 #'super-τ2)]
        [else (error "unreachable")]))
    (seq τ1 τ2 τ)))
