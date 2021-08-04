#lang racket

;; TODO: Define other base types (i.e. Char, String, Symbol, Vector, etc.)

(provide (all-defined-out))

(require racket/provide-syntax
         (for-syntax syntax/parse))

;; Abstract value representing types
(struct type (ctor-id args) #:transparent)

(define (type=? t1 t2)
  (or (eqv? t1 t2)
      (and (eq? (type-ctor-id t1) (type-ctor-id t2))
           (andmap type=? (type-args t1) (type-args t2)))))

;; Syntax for defining both base types and type constructors
(define-syntax define-type
  (syntax-parser
    [(define-type id:id)
      #'(define id (type 'id '()))]
    [(define-type (id:id param-id:id ...))
      #'(define (id param-id ...) (type 'id (list param-id ...)))]))

;; Provide syntax for exporting types and type constructors
(define-provide-syntax type-out
  (syntax-parser
    [(type-out type-id) #'type-id]))

(define-type Any)
(define-type Truthy)
(define-type Nothing)

(define-type Number)
(define-type Real)
(define-type Rational)
(define-type Integer)
(define-type Exact-Nonnegative-Integer)

; TODO: Define Char, String, Symbol, Vector, Lambda, etc.

;; Returns the supertype of `t` when one exists, returns `#f` when there are none/multiple
(define (supertype t)
  (case (type-ctor-id t)
    [(Truthy) Any]
    [(Number) Truthy]
    [(Real) Number]
    [(Rational) Real]
    [(Integer) Rational]
    [(Exact-Nonnegative-Integer) Integer]
    [else #f]))

;; Returns the abstract value representing the type of the datum `v`
(define (typeof v)
  (cond
    [(eq? v #f) Any]
    [(eq? v #t) Truthy]
    [(exact-nonnegative-integer? v) Exact-Nonnegative-Integer]
    [(integer? v) Integer]
    [(rational? v) Rational]
    [(real? v) Real]
    [(number? v) Number]
    [else (error "error: unsupported datum" v)]))