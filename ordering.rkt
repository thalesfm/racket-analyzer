#lang racket

(provide T T?
         ⊥ ⊥?
         <=? lub
         gen:dcpo)

(require racket/generic)

(define T (string->uninterned-symbol "T"))
(define (T? v) (eq? v T))

(define ⊥ (string->uninterned-symbol "⊥"))
(define (⊥? v) (eq? v ⊥))

(define (<=? dcpo1 dcpo2)
  (cond
   [(eq? dcpo1 dcpo2) #t]
   [(⊥? dcpo1) #t]
   [(T? dcpo2) #t]
   [else (gen-<=? dcpo1 dcpo2)]))

(define (lub dcpo1 dcpo2)
  (cond
   [(eq? dcpo1 dcpo2) dcpo1]
   [(or (T? dcpo1) (T? dcpo2)) T]
   [(⊥? dcpo1) dcpo2]
   [(⊥? dcpo2) dcpo1]
   [else (gen-lub dcpo1 dcpo2)]))

; TODO: Provide default implementation for dictionaries
(define-generics dcpo
  (gen-<=? dcpo v)
  (gen-lub dcpo v))
