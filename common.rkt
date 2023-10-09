#lang racket

(provide T
         T?
         ⊥
         ⊥?
         property-from-syntax
         property-stronger?
         property-combine)

(define T (string->uninterned-symbol "T"))
(define ⊥ (string->uninterned-symbol "⊥"))

(define (T? v) (eq? v T))
(define (⊥? v) (eq? v ⊥))

(define property-from-syntax (make-parameter syntax->datum))
(define property-stronger? (make-parameter equal?/recur))
(define property-combine
  (make-parameter
    (lambda (v1 v2 recur-proc)
      (if (equal? v1 v2) v1 T))))
