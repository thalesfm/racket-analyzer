#lang racket

(provide (struct-out Top)
         (struct-out Bot)
         (struct-out Truthy)
         (struct-out Literal)
         (struct-out Char)
         (struct-out Null)
         (struct-out String)
         (struct-out Symbol)
         (struct-out Void)
         (struct-out Number)
         (struct-out Real)
         (struct-out Rational)
         (struct-out Integer)
         (struct-out Exact-Nonnegative-Integer)
         (struct-out Pairof)
         (struct-out Listof)
         type-domain)

(provide
 (contract-out
  [type?         (-> any/c boolean?)]
  [datum->type   (-> any/c type?)]
  [type->datum   (-> type? any/c)]
  [type<=?       (-> type? type? boolean?)]
  [type-lub      (-> type? type? type?)]))

(require "domain.rkt")

(struct type () #:transparent)

(struct Top type () #:transparent)
(struct Bot type () #:transparent)

(struct Truthy type () #:transparent)

(struct Literal type (value) #:transparent)

(struct Char type () #:transparent)
(struct Null type () #:transparent)
(struct String type () #:transparent)
(struct Symbol type () #:transparent)
(struct Void type () #:transparent)

;; No boolean datatype because we have `Truthy`
;; Ommited built-in datatypes: bytes, byte strings, keywords, vectors,
;; hash tables, boxes, and undefined (as well as user defined structs)

(struct Number type () #:transparent)
(struct Real type () #:transparent)
(struct Rational type () #:transparent)
(struct Integer type () #:transparent)
(struct Exact-Nonnegative-Integer type () #:transparent)

;; TODO: Check that arguments are `type?` when invoked
(struct Pairof type (car cdr) #:transparent)
(struct Listof type (element) #:transparent)
;; (define-type List (Listof Top))

(define check-literal
  (disjoin boolean? number? char? string? symbol?))

(define/match (datum->type v)
  [(v) #:when (check-literal v) (Literal v)]
  [((? void?)) (Void)]
  [((? null?)) (Null)]
  [((cons a d))
   (Pairof (datum->type a)
           (datum->type d))]
  [(v)
   (raise-arguments-error 'datum->type "unsuported datum" "v" v)])

(define/match (type->datum t)
  [((Literal v)) v]
  [((Void)) (void)]
  [((Null)) null]
  [((Pairof car cdr))
   (cons (type->datum car)
         (type->datum cdr))]
  [(t)
   (raise-arguments-error 'type->datum "type can't be concretized" "τ" t)])

;; Returns the supertype of `t` when one exists,
;; returns false when none (or multiple) exist
(define (super t)
  (match t
    [(Literal #f) (Top)]
    [(Literal #t) (Truthy)]

    [(Literal (? char?))     (Char)]
    [(Literal (? string?))   (String)]
    [(Literal (? symbol?))   (Symbol)]
    [(Literal (? void?))     (Void)]

    [(Literal (? exact-nonnegative-integer?))
     (Exact-Nonnegative-Integer)]
    [(Literal (? integer?))  (Integer)]
    [(Literal (? rational?)) (Rational)]
    [(Literal (? real?))     (Real)]
    [(Literal (? number?))   (Number)]

    [(Char)     (Truthy)]
    [(String)   (Truthy)]
    [(Symbol)   (Truthy)]
    [(Void)     (Truthy)]

    [(Truthy)   (Top)]
    [(Number)   (Truthy)]
    [(Real)     (Number)]
    [(Rational) (Real)]
    [(Integer)  (Rational)]
    [(Exact-Nonnegative-Integer) (Integer)]

    [(Null) (Listof (Bot))]
    [(Listof t) (=> next)
     (let ([sup (super t)])
       (if sup (Listof sup) (next)))]

    [_ #f]))

(define/match (type<=? t1 t2)
  [(_ (Top)) #t]
  [((Bot) _) #t]

  [((== Null)      (Listof _ )   ) #t]
  [((Listof t1)    (Listof t2)   ) (type<=? t1 t2)]
  [((Pairof a  d ) (Listof t )   ) (and (type<=? a t)
                                        (type<=? d (Listof t)))]
  [((Pairof a1 d1) (Pairof a2 d2)) (and (type<=? a1 a2)
                                        (type<=? d1 d2))]

  [(t1 t2) #:when (equal? t1 t2) #t]
  [((Literal _) (Literal _)) #f]
  [((Literal _) _) (type<=? (super t1) t2)]
  [(_ (Literal _)) (type<=? t1 (super t2))]
  [(t1 t2) (=> next)
   (let ([sup (super t1)])
     (if sup (type<=? sup t2) (next)))]

  [(_  _ ) #f])

(define/match (type-lub t1 t2)
  [((Top) _       ) (Top)]
  [(_        (Top)) (Top)]
  [((Bot)    _    ) t2]
  [(_        (Bot)) t1]

  [((Listof t1)    (Listof t2)   ) (Listof (type-lub t1 t2))]
  [((Pairof a1 d1) (Pairof a2 d2)) (Pairof (type-lub a1 a2) (type-lub d1 d2))]
  [((Pairof a  d ) (Null)        ) (type-lub (Pairof a d) (Listof (Bot)))]
  [((Null)         (Pairof a  d )) (type-lub (Pairof a d) (Listof (Bot)))]
  [((Pairof a  d ) (Listof t )   )
   (match (type-lub d (Listof t))
     [(Listof t*) (Listof (type-lub a t*))]
     [_           (Truthy)])]
  [((Listof t )    (Pairof a  d ))
   (match (type-lub d (Listof t))
     [(Listof t*) (Listof (type-lub a t*))]
     [_           (Truthy)])]

  ;; These cases are reduntant but avoid expensive `type<=?` comparisons
  [((Pairof _  _ ) t             ) (if t (Truthy) (Top))]
  [(t              (Pairof _  _ )) (if t (Truthy) (Top))]

  [(t1 t2) #:when (equal?  t1 t2) t1]
  [((Literal _) (Literal _)) (type-lub (super t1) (super t2))]
  [((Literal _) _          ) (type-lub (super t1) t2)]
  [(_           (Literal _)) (type-lub t1         (super t2))]
  [(t1 t2) #:when (type<=? t1 t2) t2]
  [(t1 t2) #:when (type<=? t2 t1) t1]
  [(t1 t2) (Truthy)])

(define type-domain
  (make-domain type? type<=? type-lub (Top)))
