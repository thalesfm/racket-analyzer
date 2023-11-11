#lang racket

(provide (all-defined-out))

#;
(provide
 (contract-out
  [type?        (-> any/c boolean?)]
  [type-of      (-> any/c (or/c type? #f))]
  [type=?       (-> (or/c type? T? ⊥?) (or/c type? T? ⊥?) boolean?)]
  [type<=?      (-> (or/c type? T? ⊥?) (or/c type? T? ⊥?) boolean?)]
  [type>=?      (-> (or/c type? T? ⊥?) (or/c type? T? ⊥?) boolean?)]
  [type-lub     (-> (or/c type? T? ⊥?) (or/c type? T? ⊥?) type?)]
  [compute-type (-> any/c any)]))

(require (for-syntax racket/syntax)
         "abstract-eval.rkt"
         "common.rkt")

(struct base-type (name) #:transparent)

(define-syntax-rule (define-base-type name)
  (define name (base-type 'name)))

(struct type-union (set) #:transparent)

(define (U . types) (error "not implemented"))

(define Any T)
(define Any? T?)
(define Nothing ⊥)
(define Nothing? ⊥?)

;; Booleans

(define False #f)
(define True #t)
(define Boolean (U False True))

;; Numbers

;; Corresponds to the `number?`/`complex?` predicates
;; Simplified w.r.t. full type: does not include complex/imaginary numbers
(define Number Real)

;; Corresponds to the `real?` predicate
;; Simplified w.r.t. full type: does not include single-flonums
(define Real (U Exact-Rational Float))

;; Corresponds to the `rational?` predicate (I think?)
(define Exact-Rational
  (U Integer
     Negative-Rational-Not-Integer
     Positive-Rational-Not-Integer))

;; Corresponds to the `exact-integer?` predicate
(define Integer (U Natural Negative-Integer))

;; Corresponds to the `exact-nonnegative-integer?` predicate
(define Zero 0)
(define One 1)
(define Natural (U Zero One Positive-Integer))

;; Corresponds to the `flonum?` predicate
;; Abbreviated w.r.t. full type: does not distinguish +0.0, -0.0, +nan.0, etc.
(define-base-type Float)

(define-base-type Negative-Rational-Not-Integer)
(define-base-type Positive-Rational-Not-Integer)
(define-base-type Negative-Integer)
(define-base-type Positive-Integer)

;; Pairs and lists

(define-base-type Null)
(struct Pairof (s t) #:transparent)
(struct Listof (t) #:transparent)

;; Other datatypes

(define-base-type String)
(define-base-type Char)
(define-base-type Symbol)
(define-base-type Void)

;; Omitted build-in datatypes: byte strings, regular expressions,
;; keywords, mutable pairs and lists, vectors, stencil vectors, boxes,
;; hash tables, sets, and undefined. Also ommited are user-defined structures.

(define (type-of v)
  (cond
   ;; Booleans
   [(eq? v #t) #t]
   [(eq? v #f) #f]

   ;; Numbers
   [(exact-nonnegative-integer? v) Natural]
   [(integer? v) Integer]
   [(rational? v) Exact-Rational]
   [(real? v) Real]
   [(number? v) Number]

   ;; Pairs and lists
   [(null? v) Null]
   [(pair? v) (Pairof (type-of (car v)) (type-of (cdr v)))]

   ;; Misc.
   [(string? v) String]
   [(char? v) Char]
   [(symbol? v) Symbol]
   [(void? v) Void]

   [else (raise-arguments-error 'type-of "unsuported datum" "v" v)]))

(define (type=? t1 t2)
  (cond
   [(eq? t1 t2) #t]
   [(and (base-type? t1) (base-type? t2))
    (eq? (base-type-name t1) (base-type-name t2))]
   [(and (type-union? t1) (type-union? t2))
    (set=? (type-union-set t1) (type-union-set t2))]
   [(and (Pairof? t1) (Pairof? t2))
    (match-define (Pairof a1 d1) t1)
    (match-define (Pairof a2 d2) t2)
    (and (type=? a1 a2) (type=? d1 d2))]
   [(and (Listof? t1) (Listof? t2))
    (match-define (Listof e1) t1)
    (match-define (Listof e2) t2)
    (type=? e1 e2)]
   [else #f]))

(define (type<=? t1 t2)
  (cond
   [(eq? t1 t2) #t]

   ;; Any and Nothing
   [(Any? t2) #t]
   [(Nothing? t1) #t]

   ;; Base types and type unions
   [(and (base-type? t1) (base-type? t2))
    (eq? (base-type-name t1) (base-type-name t2))]
   [(and (type-union? t1) (type-union? t2))
    (subset? (type-union-set t1) (type-union-set t2))]

   ;; Pairs and lists
   [(and (type=? t1 Null) (Listof? t2)) #t]
   [(and (Pairof? t1) (Pairof? t2))
    (and (type<=? (Pairof-s t1) (Pairof-s t2))
         (type<=? (Pairof-t t1) (Pairof-t t2)))]
   [(and (Pairof? t1) (Listof? t2))
    (and (type<=? (Pairof-s t1) (Listof-t t2))
         (type<=? (Pairof-t t1) t2))]
   [(and (Listof? t1) (Listof? t2))
    (type<=? (Listof-t t1) (Listof-t t2))]

   [else #f]))

(define (type-comparable? t1 t2)
  (or (type<=? t1 t2) (type<=? t2 t1)))

(define (type>=? t1 t2)
  (type<=? t2 t1))

(define (type-lub t1 t2)
  (cond
   [(eq? t1 t2) t1]

   ;; Any and Nothing
   [(or (Any? t1) (Any? t2)) Any]
   [(Nothing? t1) t2]
   [(Nothing? t2) t1]

   ;; TODO: Base types and type unions
   [(and (or (base-type? t1) (type-union? t1))
         (or (base-type? t2) (type-union? t2)))
    (U t1 t2)]

   ;; Pairs and lists
   [(and (type=? t1 Null) (Pairof? t2)) (type-lub t2 (Listof Nothing))]
   [(and (Pairof? t1) (type=? t2 Null)) (type-lub t1 (Listof Nothing))]
   [(and (Pairof? t1) (Pairof? t2))
    (Pairof (type-lub (Pairof-s t1) (Pairof-s t2))
            (type-lub (Pairof-t t1) (Pairof-t t2)))]
   [(and (Pairof? t1) (Listof? t2))
    (match (type-lub (Pairof-t t1) t2)
      [(Listof t*) (Listof (type-lub (Pairof-s t1) t*))]
      [_ Any])] ; TODO: This could be more precise
   [(and (Listof? t1) (Pairof? t2))
    (match (type-lub (Pairof-t t2) t1)
      [(Listof t*) (Listof (type-lub (Pairof-s t2) t*))]
      [_ Any])] ; TODO: This could be more precise
   [(or (Pairof? t1) (Pairof? t2)) Any]
   [(and (Listof? t1) (Listof? t2)) (type<=? (Listof-t t1) (Listof-t t2))]

   [(type<=? t1 t2) t2]
   [(type<=? t2 t1) t1]

   [else Any]))

;; TODO: Refactor this using macros
(define (make-namespace)
  (define namespace (make-base-namespace))
  (define (set-constant-value! sym v)
    (namespace-set-variable-value! sym v #t namespace #t))
  (set-constant-value! '+
    (lambda ts
      (if (andmap (lambda (t) (type-comparable? t Number)) ts)
          (foldl type-lub (type-of 0) ts)
          ⊥)))
  (set-constant-value! '-
    (lambda (t . ts)
      (if (andmap (lambda (t) (type-comparable? t Number)) (cons t ts))
          (foldl type-lub Integer (cons t ts))
          ⊥)))
  (set-constant-value! '*
    (lambda ts
      (if (andmap (lambda (t) (type-comparable? t Number)) ts)
          (foldl type-lub (type-of 1) ts)
          ⊥)))
  (set-constant-value! '/
    (lambda (t . ts)
      (if (andmap (lambda (t) (type-comparable? t Number)) (cons t ts))
          (foldl type-lub Exact-Rational (cons t ts))
          ⊥)))
  (set-constant-value! '=
    (lambda (t . ts)
      (if (andmap (lambda (t) (type-comparable? t Number)) (cons t ts))
          (if (null? ts) True Boolean)
          ⊥)))
  (set-constant-value! 'map
    ;; HACK: Should check the type of `proc-t`
    (lambda (_proc-t . ts)
      (if (andmap (lambda (t) (type-comparable? (Listof Any))) ts)
          (Listof Any)
          ⊥)))
  (set-constant-value! 'read (lambda () Any))
  (set-constant-value! 'error (lambda () ⊥))
  namespace)

(define (compute-type expr)
  (parameterize
    ([property-from-syntax
        (lambda (stx) (type-of (syntax->datum stx)))]
     [property-stronger?
        (lambda (t1 t2 _recur-proc) (type<=? t1 t2))]
     [property-combine
        (lambda (t1 t2 _recur-proc) (type-lub t1 t2))])
    (abstract-eval expr (make-namespace))))

;; Numeric types as defined by `typed/racket`:
;; - Number -> Complex ->
;;     (U Inexact-Real
;;        Exact-Complex
;;        Exact-Imaginary
;;        Float-Complex
;;        Float-Imaginary
;;        Single-Flonum-Complex
;;        Single-Flonum-Imaginary)
;; - Integer -> Exact-Integer ->
;;     (U Exact-Nonnegative-Integer
;;        Negative-Fixnum
;;        Negative-Integer-Not-Fixnum)
;; - Float -> Flonum ->
;;     (U Float-Nan
;;        Float-Negative-Zero
;;        Float-Positive-Zero
;;        Negative-Float-No-NaN
;;        Positive-Float-No-NaN)
;; - Single-Flonum ->
;;     (U Negative-Single-Flonum-No-Nan
;;        Positive-Single-Flonum-No-Nan
;;        Single-Flonum-Nan
;;        Single-Flonum-Negative-Zero
;;        Single-Flonum-Positive-Zero)
;; - Inexact-Real -> (U Float Single-Flonum)
;; - Exact-Rational ->
;;     (U Exact-Integer
;;        Negative-Rational-Not-Integer
;;        Positive-Rational-Not-Integer)
;; - Real -> (U Exact-Rational Inexact-Real)
;; - Natural -> Exact-Nonnegative-Integer ->
;;     (U 0
;;        1
;;        Byte-Larger-Than-One
;;        Positive-Fixnum-Not-Index
;;        Positive-Index-Not-Byte
;;        Positive-Integer-Not-Fixnum)
