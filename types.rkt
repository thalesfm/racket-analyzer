#lang racket

(provide (type-out Top) (rename-out [Top T] [Top? T?])
         (type-out Bot) (rename-out [Bot ⊥] [Bot? ⊥?])
         (type-out Truthy)
         (type-out Char)
         (type-out String)
         (type-out Symbol)
         (type-out Void)
         (type-out Number)
         (type-out Real)
         (type-out Rational)
         (type-out Integer)
         (type-out Exact-Nonnegative-Integer)
         (type-out Null)
         (type-out Pairof)
         (type-out Listof))

(provide
 (contract-out
  [literal-type? (-> type? boolean?)]
  [type?         (-> any/c boolean?)]
  [datum->type   (-> any/c type?)]
  [type->datum   (-> type? any/c)]
  [type<=?       (-> type? type? boolean?)]
  [lub           (-> type? type? type?)]
  [rename datum->type α (-> any/c type?)]
  [rename type->datum γ (-> type? any/c)]))

(require "types-base.rkt")

(define-base-type Top)
(define-base-type Bot)

(define-base-type Truthy)

;; No boolean datatype because we have `Truthy`
(define-base-type Char)
(define-base-type String)
(define-base-type Symbol)
(define-base-type Void)
;; Ommited built-in datatypes: bytes, byte strings, keywords, vectors,
;; hash tables, boxes, and undefined (as well as user defined structs)

(define-base-type Number)
(define-base-type Real)
(define-base-type Rational)
(define-base-type Integer)
(define-base-type Exact-Nonnegative-Integer)

;; TODO: Check that arguments are `type?` when invoked
(define-base-type Null)
(define-type-constructor Pairof #:arity 2)
(define-type-constructor Listof #:arity 1)
;; (define-type List (Listof Top))

(define literal-type?
  (disjoin boolean? number? char? string? symbol?))

(define (type? v)
  (or (base-type? v) (type-ctor? v) (literal-type? v)))

(define/match (datum->type v)
  [(v) #:when (literal-type? v) v]
  [((? void?)) Void]
  [((? null?)) Null]
  [((cons a d))
   (Pairof (datum->type a)
           (datum->type d))]
  [(v)
   (raise-arguments-error 'datum->type "unsuported datum" "v" v)])

(define/match (type->datum t)
  [(t) #:when (literal-type? t) t]
  [((== Void)) (void)]
  [((== Null)) null]
  [((Pairof a d))
   (cons (type->datum a)
         (type->datum d))]
  [(t)
   (raise-arguments-error 'type->datum "type can't be concretized" "τ" t)])

;; Returns the supertype of `t` when one exists,
;; returns false when none (or multiple) exist
(define (super t)
  (match t
    [#f Top]
    [#t Truthy]

    [(? char?)     Char]
    [(? string?)   String]
    [(? symbol?)   Symbol]
    [(? void?)     Void]

    [(? exact-nonnegative-integer?) Exact-Nonnegative-Integer]
    [(? integer?)  Integer]
    [(? rational?) Rational]
    [(? real?)     Real]
    [(? number?)   Number]

    [(== Char)     Truthy]
    [(== String)   Truthy]
    [(== Symbol)   Truthy]
    [(== Void)     Truthy]

    [(== Truthy)   Top]
    [(== Number)   Truthy]
    [(== Real)     Number]
    [(== Rational) Real]
    [(== Integer)  Rational]
    [(== Exact-Nonnegative-Integer) Integer]

    [(== Null) (Listof Bot)]
    [(Listof t) (=> next)
     (let ([sup (super t)])
       (if sup (Listof sup) (next)))]

    [_ #f]))

(define (type=? t1 t2)
  (equal? t1 t2))

(define/match (type<=? t1 t2)
  [(_        (== Top)) #t]
  [((== Bot) _       ) #t]

  [((== Null)      (Listof _ )   ) #t]
  [((Listof t1)    (Listof t2)   ) (type<=? t1 t2)]
  [((Pairof a  d ) (Listof t )   ) (and (type<=? a t)
                                        (type<=? d (Listof t)))]
  [((Pairof a1 d1) (Pairof a2 d2)) (and (type<=? a1 a2)
                                        (type<=? d1 d2))]

  [(t1 t2) #:when (type=? t1 t2) #t]
  ;; TODO: Check if this still makes sense
  [(t1 t2) #:when (or (literal-type? t1) (literal-type? t2))
   (type<=? (if (literal-type? t1) (super t1) t1)
            (if (literal-type? t2) (super t2) t2))]
  [(t1 t2) (=> next)
   (let ([sup (super t1)])
     (if sup (type<=? sup t2) (next)))]

  [(_  _ ) #f])

(define/match (lub t1 t2)
  [((== Top) _       ) Top]
  [(_        (== Top)) Top]
  [((== Bot) t       ) t]
  [(t        (== Bot)) t]

  [((Listof t1)    (Listof t2)   ) (Listof (lub t1 t2))]
  [((Pairof a1 d1) (Pairof a2 d2)) (Pairof (lub a1 a2) (lub d1 d2))]
  [((Pairof a  d ) (== Null)     ) (lub (Pairof a d) (Listof Bot))]
  [((== Null)      (Pairof a  d )) (lub (Pairof a d) (Listof Bot))]
  [((Pairof a  d ) (Listof t )   )
   (match (lub d (Listof t))
     [(Listof t*) (Listof (lub a t*))]
     [_           Truthy])]
  [((Listof t )    (Pairof a  d ))
   (match (lub d (Listof t))
     [(Listof t*) (Listof (lub a t*))]
     [_           Truthy])]

  ;; These cases are reduntant but avoid expensive `type<=?` comparisons
  [((Pairof _  _ ) t             ) (if t Truthy Top)]
  [(t              (Pairof _  _ )) (if t Truthy Top)]

  [(t1 t2) #:when (equal?  t1 t2) t1]
  [(t1 t2) #:when (or (literal-type? t1) (literal-type? t2))
   (lub (if (literal-type? t1) (super t1) t1)
        (if (literal-type? t2) (super t2) t2))]
  [(t1 t2) #:when (type<=? t1 t2) t2]
  [(t1 t2) #:when (type<=? t2 t1) t1]
  [(t1 t2) (if (and t1 t2) Truthy Top)])
