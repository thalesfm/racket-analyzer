#lang racket

(require syntax/parse)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(begin-for-syntax
  (define (add-t e t)
    (syntax-property e 'type t))

  (define (get-t e)
    (define t (syntax-property e 'type))
    (if t t (error "Excpected a typed syntax")))

  (define (compute-t e)
    (syntax-case (comp+erase-t e) ()
      [(_ t) #'t]))

  (define (erase-t e)
    (syntax-case (comp+erase-t e) ()
      [(e- _) #'e]))

  (define (comp+erase-t e)
    (define e-/t (local-expand e 'expression '()))
    (define t (syntax-property e-/t 'type))
    (define e- (syntax-property-remove e-/t 'type))
    #`(#,e- #,t)))
