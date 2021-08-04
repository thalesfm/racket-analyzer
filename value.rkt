#lang racket

(require "types.rkt")

(provide (all-defined-out))

;; Equality predicate between abstract values
(define (value=? v1 v2)
  (cond
    [(eqv? v1 v2) #t]
    [(and (pair? v1) (pair? v2))
     (and (value=? (car v1) (car v2))
          (value=? (cdr v1) (cdr v2)))]
    [(and (type? v1) (type? v2)) (type=? v1 v2)]
    [else #f]))

;; Partial order predicate between abstract values
(define (value<=? v1 v2)
  (cond
    [(value=? v2 Any) #t]
    [(value=? v1 Nothing) #t]
    [(and (pair? v1) (pair? v2))
     (and (value<=? (car v1) (car v2))
          (value<=? (cdr v1) (cdr v2)))]
    [(value=? v1 v2) #t]
    [(or (not (type? v1)) (not (type? v2)))
     (value<=? (if (type? v1) v1 (typeof v1))
               (if (type? v2) v2 (typeof v2)))]
    [(supertype v1) => (λ (super-v1) (value<=? super-v1 v2))]
    [else #f]))

;; Computes the least-upper-bound between two abstract values
(define (lub v1 v2)
  (cond
    [(or (value=? v1 Any) (value=? v2 Any)) Any]
    [(value=? v1 Nothing) v2]
    [(value=? v2 Nothing) v1]
    [(and (pair? v1) (pair? v2))
     (cons (lub (car v1) (car v2))
           (lub (cdr v1) (cdr v2)))]
    [(or (pair? v1) (pair? v2)) Truthy]
    [(value=? v1 v2) v1]
    [(or (not (type? v1)) (not (type? v2)))
     (lub (if (type? v1) v1 (typeof v1))
          (if (type? v2) v2 (typeof v2)))]
    [(value<=? v1 v2) v2]
    [(value<=? v2 v1) v1]
    [(and v1 v2) Truthy]
    [else Any]))