#lang racket

(provide Any Any?
         Nothing Nothing?

         Boolean Boolean?
         True True?
         False False?

         Number Number?
         Real Real?
         Rational Rational?
         Integer Integer?
         Exact-Nonnegative-Integer Exact-Nonnegative-Integer?

         Null Null?
         Pairof Pairof?
         Listof Listof?

         Char Char?
         String String?
         Symbol Symbol?
         Void Void?)


(provide
 (contract-out
  [type?        (-> any/c boolean?)]
  [type-of      (-> any/c (or/c type? #f))]
  [type=?       (-> (or/c type? T?) (or/c type? T?) boolean?)]
  [type<=?      (-> (or/c type? T?) (or/c type? T?) boolean?)]
  [type>=?      (-> (or/c type? T?) (or/c type? T?) boolean?)]
  [type-lub     (-> (or/c type? T?) (or/c type? T?) type?)]
  [compute-type (-> any/c any)]))

(require (for-syntax racket/syntax)
         "abstract-eval.rkt"
         "common.rkt")

(struct type () #:transparent)

(define-syntax (define-type stx)
  (syntax-case stx ()
    ; Base type
    [(_ type-id)
     (identifier? #'type-id)
     (with-syntax ([pred-id (format-id #'type-id "~a?" (syntax-e #'type-id))])
       #'(begin
           (struct ctor-id type ()
                   #:transparent
                   #:reflection-name 'type-id
                   #:omit-define-syntaxes)
           (define type-id (ctor-id))
           (define (pred-id v) (eq? v type-id))))]
    ; Type constructor
    [(_ (ctor-id field ...))
     #'(struct ctor-id type (field ...) #:transparent)]))

; HACK
(define Any T)
(define Any? T?)
(define-type Nothing)

(define-type Boolean)
(define-type True)
(define-type False)

(define-type Number)
(define-type Real)
(define-type Rational)
(define-type Integer)
(define-type Exact-Nonnegative-Integer)

(define-type Null)
(define-type (Pairof car-type cdr-type))
(define-type (Listof elem-type))

(define-type String)
(define-type Char)
(define-type Symbol)
(define-type Void)

; Ommited build-in datatypes: byte strings, regular expressions,
; keywords, mutable pairs and lists, vectors, stencil vectors, boxes,
; hash tables, sets, and undefined. Also ommited are user-defined structures.

(define (type-of v)
  (cond
   ; Booleans
   [(eq? v #t) True]
   [(eq? v #f) False]
   [(boolean? v) Boolean]

   ; Numbers
   [(exact-nonnegative-integer? v) Exact-Nonnegative-Integer]
   [(integer? v) Integer]
   [(rational? v) Rational]
   [(real? v) Real]
   [(number? v) Number]

   ; Pairs and lists
   [(null? v) Null]
   [(pair? v) (Pairof (type-of (car v)) (type-of (cdr v)))]

   ; Misc.
   [(string? v) String]
   [(char? v) Char]
   [(symbol? v) Symbol]
   [(void? v) Void]

   [else (raise-arguments-error 'type-of "unsuported datum" "v" v)]))

;; Returns the supertype of `t` when one exists,
;; returns `#f` when none (or multiple) exist
(define (supertype t)
  (cond
   ; Booleans
   [(True? t) Boolean]
   [(False? t) Boolean]
   [(Boolean? t) Any]

   ; Numbers
   [(Number? t) Any]
   [(Real? t) Number]
   [(Rational? t) Real]
   [(Integer? t) Rational]
   [(Exact-Nonnegative-Integer? t) Integer]

   ; Pairs and lists
   [(Null? t) (Listof Nothing)]
   [(and (Listof? t) (supertype (Listof-elem-type t))) =>
    (lambda (super-t) (Listof super-t))]

   ; Misc.
   [(String? t) Any]
   [(Char? t) Any]
   [(Symbol? t) Any]
   [(Void? t) Any]

   [else #f]))

(define (type=? t1 t2)
  (cond
   [(eq? t1 t2) #t]

   ; Pairs and lists
   [(and (Listof? t1) (Listof? t2))
    (match-define (Listof e1) t1)
    (match-define (Listof e2) t2)
    (type=? e1 e2)]
   [(and (Pairof? t1) (Pairof? t2))
    (match-define (Pairof a1 d1) t1)
    (match-define (Pairof a2 d2) t2)
    (and (type=? a1 a2)
         (type=? d1 d2))]

   [else #f]))

(define (type<=? t1 t2)
  (cond
   [(eq? t1 t2) #t]

   ; Top and bottom
   [(Any? t2) #t]
   [(Any? t1) #f]
   [(Nothing? t1) #t]
   [(Nothing? t2) #f]

   ; Pairs and lists
   [(and (Null? t1) (Listof? t2)) #t]
   [(and (Listof? t1) (Listof? t2))
    (type<=? (Listof-elem-type t1) (Listof-elem-type t2))]
   [(and (Pairof? t1) (Listof? t2))
    (and (type<=? (Pairof-car-type t1) (Listof-elem-type t2))
         (type<=? (Pairof-cdr-type t1) t2))]
   [(and (Pairof? t1) (Pairof? t2))
    (and (type<=? (Pairof-car-type t1) (Pairof-car-type t2))
         (type<=? (Pairof-cdr-type t1) (Pairof-cdr-type t2)))]

   ; Misc. base types
   [(supertype t1) =>
    (lambda (super-t1) (type<=? super-t1 t2))]
   [else #f]))

(define (type>=? t1 t2)
  (type<=? t2 t1))

(define (type-lub t1 t2)
  (cond
   [(eq? t1 t2) t1]

   ; Top and bottom
   [(or (Any? t1) (Any? t2)) Any]
   [(Nothing? t1) t2]
   [(Nothing? t2) t1]

   ; Pairs and lists
   [(and (Pairof? t1) (Pairof? t2))
    (match-define (Pairof a1 d1) t1)
    (match-define (Pairof a2 d2) t2)
    (Pairof (type-lub a1 a2) (type-lub d1 d2))]
   [(and (Pairof? t1) (Null? t2)) (type-lub t1 (Listof Nothing))]
   [(and (Null? t1) (Pairof? t2)) (type-lub (Listof Nothing) t2)]
   [(and (Pairof? t1) (Listof? t2))
    (match-define (Pairof a1 d1) t1)
    (match (type-lub d1 t2)
      [(Listof t*) (Listof (type-lub a1 t*))]
      [_ Any])] ; TODO: This could be more precise
   [(and (Listof? t1) (Pairof? t2))
    (match-define (Pairof a2 d2) t2)
    (match (type-lub t1 d2)
      [(Listof t*) (Listof (type-lub t* a2))]
      [_ Any])] ; TODO: This could be more precise
  [(or (Pairof? t1) (Pairof? t2)) Any]

  ; Misc. base types
  [(type<=? t1 t2) t2]
  [(type<=? t2 t1) t1]
  [else Any]))

; TODO: Refactor this using macros
(define (make-namespace)
  (define namespace (make-base-namespace))
  (define (set-constant-value! sym v)
    (namespace-set-variable-value! sym v #t namespace #t))
  (set-constant-value! '+
    (lambda ts
      (if (andmap (curryr type<=? Number) ts)
          (foldl type-lub (type-of 0) ts)
          ⊥)))
  (set-constant-value! '-
    (lambda (t . ts)
      (if (andmap (curryr type<=? Number) (cons t ts))
          (foldl type-lub Integer (cons t ts))
          ⊥)))
  (set-constant-value! '*
    (lambda ts
      (if (andmap (curryr type<=? Number) ts)
          (foldl type-lub (type-of 1) ts)
          ⊥)))
  (set-constant-value! '/
    (lambda (t . ts)
      (if (andmap (curryr type<=? Number) (cons t ts))
          (foldl type-lub Rational (cons t ts))
          ⊥)))
  (set-constant-value! '=
    (lambda (t . ts)
      (if (andmap (curryr type<=? Number) (cons t ts))
          (if (null? ts) True Boolean)
          ⊥)))
  (set-constant-value! 'map
    ; HACK: Should check the type of `proc-t`
    (lambda (_proc-t . ts)
      (if (andmap (curryr type<=? (Listof Any)) ts) (Listof Any) ⊥)))
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
