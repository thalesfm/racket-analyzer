#lang racket

(require rackunit "types.rkt" "value.rkt")

(test-case
  "Partial order tests"

  (define/match (check-descending vs)
    [('()) (void)]
    [((cons v vs))
    (for ([elem (in-list vs)])
      (check-true (value<=? elem v))
      (check-false (value<=? v elem)))
    (check-descending vs)])

  ;; Checks for `Any`
  (check-true (value<=? Any Any))
  (check-true (value<=? 10 Any))
  (check-true (value<=? Number Any))
  (check-false (value<=? Any 10))
  (check-false (value<=? Any Number))
  
  ;; Checks for `Nothing`
  (check-true (value<=? Nothing Any))
  (check-true (value<=? Nothing Nothing))
  (check-true (value<=? Nothing 10))
  (check-true (value<=? Nothing Number))

  ;; Checks for `True`
  (check-true (value<=? True True))
  (check-true (value<=? Number True))
  (check-false (value<=? #f True))
  
  ;; Checks for `Number`, `Real`, `Integer`, etc.
  (check-descending (list Number Real Rational Integer Exact-Nonnegative-Integer))

  ;; Checks for numeric literals
  (check-true (value<=? 10 Exact-Nonnegative-Integer))
  (check-true (value<=? 10.0 Integer))
  (check-true (value<=? -999 Integer))
  (check-true (value<=? 10/3 Rational))
  (check-true (value<=? 10.3 Rational))
  (check-false (value<=? +inf.0 Rational))
  (check-false (value<=? +nan.0 Rational))
  (check-true (value<=? +inf.0 Real))
  (check-true (value<=? +nan.0 Real))
  (check-true (value<=? 3+2i Number))
  (check-true (value<=? 3.0+2.0i Number))
  
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

  ;; (check-true (value<=? Nothing (lub Nothing (random-value)))
  ;; (check-false (value<=? (lub Nothing (random-value)) Nothing))
)