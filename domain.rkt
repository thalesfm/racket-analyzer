#lang racket

(require "types.rkt")

(provide
 (all-from-out "types.rkt")
 (contract-out [abstract? (-> any/c boolean?)]
               [constant? (-> abstract? boolean?)]
               [<=? (-> abstract? abstract? boolean?)]
               [lub (-> abstract? abstract? abstract?)]
))

(define (abstract? v)
  (cond [(boolean? v) #t]
        [(number? v) #t]
        [(type? v) #t]
        [(cons? v) (and (abstract? (car v))
                        (abstract? (cdr v)))]
        [else #f]))

(define (constant? v)
  (cond
    [(boolean? v) #t]
    [(number? v) #t]
    [(type? v) #f]
    [(pair? v) (and (constant? (car v))
                    (constant? (cdr v)))]
    [else #f]))

(define (<=? v1 v2)
  (cond
    [(equal? v2 Any) #t]
    [(equal? v1 Nothing) #t]
    [(and (pair? v1) (pair? v2))
     (and (<=? (car v1) (car v2))
          (<=? (cdr v1) (cdr v2)))]
    [(equal? v1 v2) #t]
    [(or (not (type? v1)) (not (type? v2)))
     (<=? (if (type? v1) v1 (typeof v1))
          (if (type? v2) v2 (typeof v2)))]
    [(supertype v1) => (λ (super-v1) (<=? super-v1 v2))]
    [else #f]))

(define (lub v1 v2)
  (cond
    [(or (equal? v1 Any) (equal? v2 Any)) Any]
    [(equal? v1 Nothing) v2]
    [(equal? v2 Nothing) v1]
    [(and (pair? v1) (pair? v2))
     (cons (lub (car v1) (car v2))
           (lub (cdr v1) (cdr v2)))]
    [(or (pair? v1) (pair? v2))
     (if (and v1 v2) True Any)]
    [(equal? v1 v2) v1]
    [(or (not (type? v1)) (not (type? v2)))
     (lub (if (type? v1) v1 (typeof v1))
          (if (type? v2) v2 (typeof v2)))]
    [(<=? v1 v2) v2]
    [(<=? v2 v1) v1]
    [else
     (if (and v1 v2) True Any)]))
