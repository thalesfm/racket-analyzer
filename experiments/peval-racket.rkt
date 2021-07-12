#lang racket

(require (for-syntax racket
                     racket/match
                     syntax/parse
                     rebellion/type/singleton))

(provide print-type
         top
         dyn-number
         dyn-string
         (rename-out [typed#%datum #%datum]
                     [typed+ +]))

(define-syntax-rule (print-type e)
  (begin-for-syntax
    (printf "~v\n" (compute-type #'e))))

(begin-for-syntax
  (define (add-type e t)
    (syntax-property e 'type t))

  (define (get-type e)
    (define t (syntax-property e 'type))
    (unless t (error "Excpected a typed syntax"))
    t)

  (define (compute-type e)
    (let-values ([(e- t) (comp+erase-type e)]) t))

  (define (erase-type e)
    (let-values ([(e- t) (comp+erase-type e)]) e-))

  (define (comp+erase-type e)
    (define e-/t (local-expand e 'expression '()))
    (define t (syntax-property e-/t 'type))
    (define e- (syntax-property-remove e-/t 'type))
    (values e- t))

  (define-singleton-type Top)
  (define-singleton-type None)
  (define-singleton-type DynNumber)
  (define-singleton-type DynString)
  (struct StaNumber (v) #:transparent)
  (struct StaString (v) #:transparent)

  (define (type=? t1 t2)
    (equal? t1 t2))
  
  (define (type<=? t1 t2)
    (or (Top? t2)
        (type=? t1 t2)
        (match* (t1 t2)
          [((StaNumber _) DynNumber) #t]
          [((StaString _) DynString) #t]
          [(_ _) #f])))

  (define (type-comparable? t1 t2)
    (or (type<=? t1 t2) (type<=? t2 t1)))

  (define (static? t)
    (match t
      [(StaNumber _) #t]
      [(StaString _) #t]
      [_ #f]))

  (define (type->value t)
    (match t
      [(StaNumber v) v]
      [(StaString v) v])))

(define-syntax (typed#%datum stx)
  (syntax-parse stx
    [(_ . x:number)
     (add-type #'(#%datum . x) (StaNumber (syntax-e #'x)))]))

(define-syntax (top stx)
  (syntax-parse stx
    [(_) (add-type #''top Top)]))

(define-syntax (dyn-number stx)
  (syntax-parse stx
    [(_) (add-type #'0 DynNumber)]))

(define-syntax (dyn-string stx)
  (syntax-parse stx
    [(_) (add-type #'"" DynString)]))

(define-syntax (typed+ stx)
  (syntax-parse stx
    [(_ e1 e2)
     (define-values (e1- t1) (comp+erase-type #'e1))
     (define-values (e2- t2) (comp+erase-type #'e2))
     (define t
       (cond
         [(and (type<=? t1 DynNumber)
               (type<=? t2 DynNumber)
               (static? t1)
               (static? t2))
          (StaNumber (+ (type->value t1) (type->value t2)))]
         [(and (type<=? t1 DynNumber)
               (type<=? t2 DynNumber))
          DynNumber]
         [(and (type-comparable? t1 DynNumber)
               (type-comparable? t2 DynNumber))
          Top]
         [else None]))
     (add-type #'(+ e1 e2) t)]))
