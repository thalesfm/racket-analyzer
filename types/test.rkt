#lang racket

(require rackunit
         "base.rkt"
         "defs.rkt")

(test-case
  "Partial order tests"

  (define/match (check-descending vs)
    [('()) (void)]
    [((cons v vs))
     (for ([elem (in-list vs)])
       (check-eq? (<=? elem v) #t)
       (check-eq? (<=? v elem) #f))
     (check-descending vs)])

  ;; Checks for `Any`
  (check-eq? (<=? Any Any) #t)
  (check-eq? (<=? 10 Any) #t)
  (check-eq? (<=? Number Any) #t)
  (check-eq? (<=? Any 10) #f)
  (check-eq? (<=? Any Number) #f)

  ;; Checks for `Nothing`
  (check-eq? (<=? Nothing Any) #t)
  (check-eq? (<=? Nothing Nothing) #t)
  (check-eq? (<=? Nothing 10) #t)
  (check-eq? (<=? Nothing Number) #t)

  ;; Checks for `True`
  (check-eq? (<=? True True) #t)
  (check-eq? (<=? Number True) #t)
  (check-eq? (<=? #f True) #f)

  ;; Checks for `Number`, `Real`, `Integer`, etc.
  (check-descending (list Number Real Rational Integer Exact-Nonnegative-Integer))

  ;; Checks for numeric literals
  (check-eq? (<=? 10 Exact-Nonnegative-Integer) #t)
  (check-eq? (<=? 10.0 Integer) #t)
  (check-eq? (<=? -999 Integer) #t)
  (check-eq? (<=? 10/3 Rational) #t)
  (check-eq? (<=? 10.3 Rational) #t)
  (check-eq? (<=? +inf.0 Rational) #f)
  (check-eq? (<=? +nan.0 Rational) #f)
  (check-eq? (<=? +inf.0 Real) #t)
  (check-eq? (<=? +nan.0 Real) #t)
  (check-eq? (<=? 3+2i Number) #t)
  (check-eq? (<=? 3.0+2.0i Number) #t)

  ;; TODO: Checks for pairs
)

(test-case
  "Tests for the least-upper-bound"

  ;; Checks for `Any`
  (check-equal? (lub 10 Any) Any)
  (check-equal? (lub Any 10) Any)

  ;; Checks for `Nothing`
  (check-equal? (lub 10 Nothing) 10)
  (check-equal? (lub Nothing 10) 10)
  (check-equal? (lub Nothing Nothing) Nothing)

  ;; Checks for pairs
  (check-equal? (lub #t (cons 1 2)) True)
  (check-equal? (lub #f (cons 1 2)) Any)

  ;; (check-eq? (<=? Nothing (lub Nothing (random-value)) #t)
  ;; (check-eq? (<=? (lub Nothing (random-value)) Nothing) #f)
)
