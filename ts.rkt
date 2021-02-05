#lang racket

(require (for-syntax racket racket/match syntax/parse))

(provide print-type
         dyn-number
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
    (define-values (_e- t) (comp+erase-type e))
    t)

  (define (erase-type e)
    (define-values (e- _t) (comp+erase-type e))
    e-)

  (define (comp+erase-type e)
    (define e-/t (local-expand e 'expression '()))
    (define t (syntax-property e-/t 'type))
    (define e- (syntax-property-remove e-/t 'type))
    (values e- t))

  (struct Top () #:transparent)
  (struct DynNumber () #:transparent)
  (struct StaNumber (x) #:transparent)

  (define (type=? t1 t2)
    (equal? t1 t2))
  
  (define (type<=? t1 t2)
    (or (Top? t2)
        (type=? t1 t2)
        (match* (t1 t2)
          [((StaNumber _) DynNumber) #t])))

  (define (static? t)
    (match t
      [(StaNumber _) #t]
      [_ #f]))

  (define (type->datum t)
    (match t
      [(StaNumber x) x])))

(define-syntax (typed#%datum stx)
  (syntax-parse stx
    [(_ . x:number)
     (add-type #'(#%datum . x) (StaNumber (syntax->datum #'x)))]))

(define-syntax (dyn-number stx)
  (syntax-parse stx
    [(_) (add-type #'0 (DynNumber))]))

(define-syntax (typed+ stx)
  (syntax-parse stx
    [(_ e1 e2)
     (define-values (e1- t1) (comp+erase-type #'e1))
     (define-values (e2- t2) (comp+erase-type #'e2))
     (cond
       [(and (static? t1) (static? t2))
        (add-type #`(+ #,e1- #,e2-) (StaNumber (+ (type->datum t1) (type->datum t2))))]
       [(and (type<=? t1 (DynNumber)) (type<=? t2 (DynNumber)))
        (add-type #`(+ #,e1- #,e2-) (DynNumber))])]))
