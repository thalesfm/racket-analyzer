#lang racket

(provide (type-out Top)
         (type-out Bot)
         (type-out Truthy)
         (type-out Number)
         (type-out Real)
         (type-out Rational)
         (type-out Integer)
         (type-out Exact-Nonnegative-Integer)
         (type-out Pairof))

(provide
 (contract-out
  [literal?    (-> type? boolean?)]
  [type?       (-> any/c boolean?)]
  [datum->type (-> any/c type?)]
  [type->datum (-> type? any/c)]
  [type<=?     (-> type? type? boolean?)]
  [lub         (-> type? type? type?)]
  [rename datum->type α (-> any/c type?)]
  [rename type->datum γ (-> type? any/c)]))

(require "types-base.rkt")

(define-base-type Top)
(define-base-type Bot)

(define-base-type Truthy)

(define-base-type Number)
(define-base-type Real)
(define-base-type Rational)
(define-base-type Integer)
(define-base-type Exact-Nonnegative-Integer)

;; TODO: Check that arguments are type? when invoked
(define-type-constructor Pairof #:arity 2)

(define (literal? v)
  (or (boolean? v) (number? v)))

(define (type? v)
  (or (base-type? v) (type-ctor? v) (literal? v)))

(define/match (datum->type v)
  [(v) #:when (literal? v) v]
  [((cons a d))
   (Pairof (datum->type a)
           (datum->type d))]
  [(v)
   (raise-arguments-error 'datum->type "unsuported datum" "v" v)])

(define/match (type->datum t)
  [(t) #:when (literal? t) t]
  [((Pairof a d))
   (cons (type->datum a)
         (type->datum d))]
  [(t)
   (raise-arguments-error 'type->datum "type can't be concretized" "τ" t)])

;; Returns the supertype of `t` when one exists,
;; returns false when none (or multiple) exist
(define/match (super t)
  [(#f) Top]
  [(#t) Truthy]

  [((? exact-nonnegative-integer?)) Exact-Nonnegative-Integer]
  [((? integer?) ) Integer]
  [((? rational?)) Rational]
  [((? real?)    ) Real]
  [((? number?)  ) Number]

  [((== Truthy)  ) Top]
  [((== Number)  ) Truthy]
  [((== Real)    ) Number]
  [((== Rational)) Real]
  [((== Integer) ) Rational]
  [((== Exact-Nonnegative-Integer)) Integer]

  [(_) #f])

(define/match (type<=? t1 t2)
  [(_        (== Top)) #t]
  [((== Bot) _       ) #t]

  [((Pairof a1 d1) (Pairof a2 d2))
    (and (type<=? a1 a2)
         (type<=? d1 d2))]

  [(t1 t2) #:when (equal? t1 t2) #t]
  [(t1 t2) #:when (or (literal? t1) (literal? t2))
   (type<=? (if (literal? t1) (super t1) t1)
            (if (literal? t2) (super t2) t2))]
  [(t1 t2)
   (let ([sup (super t1)])
     (if  sup (type<=? sup t2) #f))])

(define/match (lub t1 t2)
  [((== Top) _       ) Top]
  [(_        (== Top)) Top]
  [((== Bot) t       ) t]
  [(t        (== Bot)) t]

  [((Pairof a1 d1) (Pairof a2 d2)) (Pairof (lub a1 a2) (lub d1 d2))]
  [((Pairof _  _ ) t             ) (if t Truthy Top)]
  [(t              (Pairof _  _ )) (if t Truthy Top)]

  [(t1 t2) #:when (equal? t1 t2)  t1]
  [(t1 t2) #:when (or (literal? t1) (literal? t2))
   (lub (if (literal? t1) (super t1) t1)
        (if (literal? t2) (super t2) t2))]
  [(t1 t2) #:when (type<=? t1 t2) t2]
  [(t1 t2) #:when (type<=? t2 t1) t1]
  [(t1 t2) (if (and t1 t2) Truthy Top)])
