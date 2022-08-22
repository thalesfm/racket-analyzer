#lang racket

(provide (all-defined-out))

(require racket/generic)

(define-generics type
  (type<=? type t)
  (type-static? type))

(struct Top ()
  #:methods gen:type
  [(define (type<=? t1 t2)
     (Top? t2))
   (define (type-static? t)
     #f)])

(struct Bot ()
  #:methods gen:type
  [(define (type<=? t1 t2)
     #t)
   (define (type-static? t)
     #t)])

(struct DNumber ()
  #:methods gen:type
  [(define/generic super-type<=? type<=?)
   (define (type<=? t1 t2)
     (or (DNumber? t2) (super-type<=? (Top) t2)))
   (define (type-static? t)
     #f)])

(struct SNumber (v)
  #:methods gen:type
  [(define/generic super-type<=? type<=?)
   (define (type<=? t1 t2)
     (or (and (SNumber? t2) (= (SNumber-v t1) (SNumber-v t2)))
	 (super-type<=? (DNumber) t2)))
   (define (type-static? t)
     #t)])

(type<=? (Top) (Top))
(type<=? (DNumber) (DNumber))
(type<=? (SNumber 1) (SNumber 1))
(type<=? (SNumber 1) (SNumber 2))
(type<=? (SNumber 1) (DNumber))
(type<=? (SNumber 1) (Top))
(type<=? (DNumber) (Top))
(type<=? (Bot) (Top))
