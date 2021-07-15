#lang racket

(provide
  (type-out Top Nothing Number Real Integer Boolean String Pairof)
  (for-syntax datum->type
              type->datum
              static?
              maybe-bot?
              type<=?
              type-comparable?
              lub
              seq))

(require turnstile)

(define-base-types Top Nothing)
(define-base-types Number Real Integer)
(define-base-types Boolean String)
(define-type-constructor Pairof #:arity = 2)

(define-for-syntax (make-type stx
                              #:static? [static #f]
                              #:maybe-bot? [maybe-bot #f])
  (define τ (mk-type stx))
  (define τ/static (syntax-property τ 'static? static))
  (syntax-property τ/static 'maybe-bot? maybe-bot))


(define-for-syntax (static? τ)
  (syntax-property τ 'static?))
  
(define-for-syntax (maybe-bot? τ)
  (syntax-property τ 'maybe-bot?))

(define-for-syntax (valid-datum? v)
  (or (integer? v)
      (real? v)
      (boolean? v)
      (string? v)))
  
(define-for-syntax (datum->type v)
  (unless (valid-datum? v)
    (error (format "unsupported literal: ~a" v)))
  (make-type (datum->syntax #f (list #'quote v)) #:static? #t))
  
(define-for-syntax (type->datum τ)
  (unless (static? τ)
    (error "unable to convert dynamic type to datum"))
  (syntax-parse τ
    [((~literal quote) v) (syntax->datum #'v)]))
  
(define-for-syntax (supertype τ)
  (define super-τ
    (syntax-parse τ
      [~Top #f]
      [~Nothing #f]
      [~Number #'Top]
      [~Real #'Number]
      [~Integer #'Real]
      [~Boolean #'Top]
      [~String #'Top]
      [_ #:when (not (static? τ)) #f]
      [_ #:when (exact-integer? (type->datum τ)) #'Integer]
      [_ #:when (real? (type->datum τ)) #'Real]
      [_ #:when (boolean? (type->datum τ)) #'Boolean]
      [_ #:when (string? (type->datum τ)) #'String]
      [_ #f]))
  (if super-τ
      (seq τ ((current-type-eval) super-τ))
      #f))
  
(define-for-syntax (type<=? τ1 τ2)
  (syntax-parse #`(#,τ1 #,τ2)
    [_ #:when (and (maybe-bot? τ1) (not (maybe-bot? τ2))) #f]
    [_ #:when (type=? τ1 τ2) #t]
    [(_ ~Top) #t]
    [(~Nothing _) #t]
    [((~Pairof τ1-a τ1-d) (~Pairof τ2-a τ2-d))
     (and (type<=? #'τ1-a #'τ2-a)
          (type<=? #'τ1-d #'τ2-d))]
    [_ #:when (supertype τ1) (type<=? (supertype τ1) τ2)]
    [_ #f]))
  
(define-for-syntax (type-comparable? τ1 τ2)
  (or (type<=? τ1 τ2) (type<=? τ2 τ1)))

(define-for-syntax (seq . τs)
  (make-type (last τs) #:maybe-bot? (ormap maybe-bot? τs)))

(define-for-syntax (τ-cons τ1 τ2)
  (define τ (make-type #`(Pairof #,τ1 #,τ2)
                       #:static? (and (static? τ1) (static? τ2))))
  (seq τ1 τ2 τ))

(define-for-syntax (lub τ1 τ2)
  (define τ
    (syntax-parse #`(#,τ1 #,τ2)
      [_ #:when (type=? τ1 τ2) τ1]
      [(~Top _) #'Top]
      [(_ ~Top) #'Top]
      [(~Nothing _) τ2]
      [(_ ~Nothing) τ1]
      [((~Pairof τ1-a τ1-d) (~Pairof τ2-a τ2-d))
       (τ-cons (lub #'τ1-a #'τ2-a)
               (lub #'τ1-d #'τ2-d))]
      [_ #:when (type<=? τ1 τ2) (lub (supertype τ1) τ2)]
      [_ #:when (type<=? τ2 τ1) (lub τ1 (supertype τ2))]))
  (seq τ1 τ2 τ))
