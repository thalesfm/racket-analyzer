#lang racket

(provide (type-out Top)
         (type-out Bot)
         (type-out Truthy)
         (type-out Number)
         (type-out Real)
         (type-out Rational)
         (type-out Integer)
         (type-out Exact-Nonnegative-Integer)
         (contract-out [type? (-> any/c boolean?)]
                       [literal? (-> type? boolean?)]
                       [type<=? (-> type? type? boolean?)]
                       [lub (-> type? type? type?)]))

(require racket/provide-syntax
         syntax/parse/define
         (for-syntax racket/syntax))

(struct base-type (name) #:transparent)
(struct type-ctor (name args) #:transparent)

(define-for-syntax (format-pred-id id)
  (format-id id "~a?" (syntax-e id)))

(define-for-syntax (stx-length=? stx n)
  (eq? (length (syntax->list stx)) n))

(define-syntax-parser define-base-type
  [(_ type:id)
   #:with pred (format-pred-id #'type)
   #'(begin
       (define type (base-type 'type))
       (define (pred v) (eq? v type)))])

(define-syntax-parser define-type-constructor
  [(_ ctor:id #:arity arity:exact-positive-integer)
   #:with pred (format-pred-id #'ctor)
   #'(begin
       (define-match-expander ctor
         (syntax-parser
           [(_ pat:expr (... ...))
            #:fail-when (not (stx-length=? #'(pat (... ...)) arity))
                        "arity mismatch"
            #'(type-ctor 'ctor (list pat (... ...)))])
         (syntax-parser
           [(_ arg:expr (... ...))
            #:fail-when (not (stx-length=? #'(arg (... ...)) arity))
                        "arity mismatch"
            #'(type-ctor 'ctor (list arg (... ...)))]))
       (define (pred v)
         (and (type-ctor? v)
              (eq? (type-ctor-name v) 'id))))])

(define-provide-syntax type-out
  (syntax-parser
    [(_ type)
     #:with pred (format-pred-id #'type)
     #'(combine-out type pred)]))

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

;; TODO: Define abstraction function (datum->type)
#;(define (α v) ...)

;; TODO: Define "concretization" function (type->datum)
#;(define (γ τ) ...)

;; WARN: Does not consider pairs to be literals!!!
(define (literal? v)
  (or (boolean? v) (number? v)))

(define (type? v)
  (or (base-type? v) (type-ctor? v) (literal? v)))

;; Returns the supertype of `t` when one exists,
;; returns false when there are none/multiple
(define/match (super _t)
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

(define/match (type<=? _t1 _t2)
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

(define/match (lub _t1 _t2)
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
