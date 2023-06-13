#lang racket

(provide (type-out Any)
         (type-out True)
         (type-out Nothing)
         (type-out Number)
         (type-out Real)
         (type-out Rational)
         (type-out Integer)
         (type-out Exact-Nonnegative-Integer)
         (contract-out [abstract? (-> any/c boolean?)]
                       [constant? (-> abstract? boolean?)]
                       [<=? (-> abstract? abstract? boolean?)]
                       [lub (-> abstract? abstract? abstract?)]))

(require racket/provide-syntax
         racket/struct
         syntax/parse/define
         (for-syntax racket/syntax))

(struct base-type (name) #:transparent)
(struct type-ctor (name args) #:transparent)

(define-for-syntax (format-pred-id id)
  (format-id id "~a?" (syntax-e id)))

(define-for-syntax (stx-list-length=? stx n)
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
            #:fail-when (not (stx-list-length=? #'(pat (... ...)) arity))
                        "arity mismatch"
            #'(type-ctor 'ctor (list pat (... ...)))])
         (syntax-parser
           [(_ arg:expr (... ...))
            #:fail-when (not (stx-list-length=? #'(arg (... ...)) arity))
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

(define-base-type Any)
(define-base-type True)
(define-base-type Nothing)

(define-base-type Number)
(define-base-type Real)
(define-base-type Rational)
(define-base-type Integer)
(define-base-type Exact-Nonnegative-Integer)

;; Returns the supertype of `t` when one exists,
;; returns false when there are none/multiple
(define (supertype t)
  (cond
    [(eq? t #f) Any]
    [(eq? t #t) True]
    [(exact-nonnegative-integer? t) Exact-Nonnegative-Integer]
    [(integer? t) Integer]
    [(rational? t) Rational]
    [(real? t) Real]
    [(number? t) Number]
    [(True? t) Any]
    [(Number? t) True]
    [(Real? t) Number]
    [(Rational? t) Real]
    [(Integer? t) Rational]
    [(Exact-Nonnegative-Integer? t) Integer]
    [else #f]))

(define (abstract? v)
  (cond [(boolean? v) #t]
        [(number? v) #t]
        [(base-type? v) #t]
        [(type-ctor? v) #t]
        [(cons? v) (and (abstract? (car v))
                        (abstract? (cdr v)))]
        [else #f]))

(define (constant? v)
  (cond
    [(boolean? v) #t]
    [(number? v) #t]
    [(base-type? v) #f]
    [(type-ctor? v) #f]
    [(pair? v) (and (constant? (car v))
                    (constant? (cdr v)))]
    [else #f]))

(define (<=? v1 v2)
  (cond
    [(equal? v2 Any) #t]
    [(equal? v1 Nothing) #t]
    [(and (pair? v1) (pair? v2))
     (and (<=? (car v1) (car v2))
          (<=? (cdr v1) (cdr v2)))]
    [(equal? v1 v2) #t]
    [(or (not (base-type? v1)) (not (base-type? v2)))
     (<=? (if (base-type? v1) v1 (supertype v1))
          (if (base-type? v2) v2 (supertype v2)))]
    [(supertype v1) => (λ (super-v1) (<=? super-v1 v2))]
    [else #f]))

(define (lub v1 v2)
  (cond
    [(or (equal? v1 Any) (equal? v2 Any)) Any]
    [(equal? v1 Nothing) v2]
    [(equal? v2 Nothing) v1]
    [(and (pair? v1) (pair? v2))
     (cons (lub (car v1) (car v2))
           (lub (cdr v1) (cdr v2)))]
    [(or (pair? v1) (pair? v2))
     (if (and v1 v2) True Any)]
    [(equal? v1 v2) v1]
    [(or (not (base-type? v1)) (not (base-type? v2)))
     (lub (if (base-type? v1) v1 (supertype v1))
          (if (base-type? v2) v2 (supertype v2)))]
    [(<=? v1 v2) v2]
    [(<=? v2 v1) v1]
    [else
     (if (and v1 v2) True Any)]))
