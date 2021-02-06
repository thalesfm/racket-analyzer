#lang turnstile/quicklang

(require rackunit/turnstile)
(require turnstile/no-unicode)

(provide (all-defined-out) print-type)

(define-base-types Top None DynNumber DynString)
(define-type-constructor StaNumber #:arity = 1)
(define-type-constructor StaString #:arity = 1)

#|
(define-internal-type-constructor StaNumber)
(define-syntax StaNumber
  (syntax-parser
    [(_ n) #`(StaNumber- #,(mk-type #''n))]))
|#

(begin-for-syntax
  (define (static? τ)
    (syntax-parse τ
      [(~StaNumber _) #t]
      [(~StaString _) #t]
      [_ #f]))

  (define (type->value τ)
    (syntax->datum
      (syntax-parse τ
        [(~StaNumber 'n) #'n]
        [(~StaString 's) #'s])))
  
  (define (type<=? t1 t2)
    (define τ1 ((current-type-eval) t1))
    (define τ2 ((current-type-eval) t2))
    (or (type=? τ1 τ2)
        (Top? τ2)
        (syntax-parse (list τ1 τ2)
          [((~StaNumber _) ~DynNumber) #t]
          [((~StaString _) ~DynString) #t]
          [_ #f])))

  (define (type-comparable? τ1 τ2)
    (or (type<=? τ1 τ2) (type<=? τ2 τ1)))

  (current-typecheck-relation type<=?))

(define-syntax ?
  (syntax-parser
    [_:id (assign-type #'(error "Not available at runtime") #'Top)]))

(define-syntax ?num
  (syntax-parser
    [_:id (assign-type #'(error "Not available at runtime") #'DynNumber)]))

(define-syntax ?str
  (syntax-parser
    [_:id (assign-type #'(error "Not available at runtime") #'DynString)]))

(define-typed-syntax #%datum
  [(_ . n:number) >>
   --------
   [/- (#%datum- . n) => (StaNumber #,(mk-type #''n))]]
  [(_ . s:string) >>
   --------
   [/- (#%datum- . s) => (StaString #,(mk-type #''s))]]
  [(_ . v) >>
   --------
   [#:error (type-error #:src #'v #:msg "Unsupported literal: ~v" #'v)]])

(begin-for-syntax
  (define (type+ τ1 τ2)
    (cond
      [(and (type<=? τ1 #'DynNumber)
            (type<=? τ2 #'DynNumber)
            (static? τ1)
            (static? τ2))
       (define n (+ (type->value τ1) (type->value τ2)))
       #`(StaNumber #,(mk-type #`(quote #,n)))]
      [(and (type<=? τ1 #'DynNumber)
            (type<=? τ2 #'DynNumber))
       #'DynNumber]
      [(and (type-comparable? τ1 #'DynNumber)
            (type-comparable? τ2 #'DynNumber))
       #'Top]
      [else
       #'None])))

(define-typed-syntax (+ e1 e2) >>
   [/- e1 >> e1- ⇒ τ1]
   [/- e2 >> e2- ⇒ τ2]
   #:with τ_res (type+ #'τ1 #'τ2)
   --------
   [/- (+- e1- e2-) => τ_res])
