#lang racket

(provide abstract-value? T ⊥ <=? lub)

(define (abstract-value? v)
  (or (eq? v T) (eq? v ⊥) (boolean? v) (natural? v)))

(define T (unquoted-printing-string "T"))
(define ⊥ (unquoted-printing-string "⊥"))

(define (<=? d d′)
  (cond
   [(eqv? d  d′) #t]
   [(eq?  d  ⊥ ) #t]
   [(eq?  d′ T ) #t]
   [else #f]))

(define (lub d d′)
  (cond
   [(eqv? d  d′) d ]
   [(eq?  d  ⊥ ) d′]
   [(eq?  d′ ⊥ ) d ]
   [else T]))