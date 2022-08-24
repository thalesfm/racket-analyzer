#lang racket

(require rackunit "types.rkt" "order.rkt")

(test-case
  "Partial order tests"

  ;; Checks for `Any`
  (check-true (<=? Any Any))
  (check-true (<=? 10 Any))
  (check-true (<=? Number Any))
  (check-false (<=? Any 10))
  (check-false (<=? Any Number))
  
  ;; Checks for `Nothing`
  (check-true (<=? Nothing Any))
  (check-true (<=? Nothing Nothing))
  (check-true (<=? Nothing 10))
  (check-true (<=? Nothing Number))

  ;; Checks for `Truthy`
  (check-true (<=? Truthy Truthy))
  (check-true (<=? Number Truthy))
  (check-false (<=? #f Truthy))
  
  ;; Checks for `Number`, `Real`, `Integer`, etc.
  ;; Exact-Nonnegative-Integer <= Integer <= Rational <= Real <= Number
  (check-true (<=? Number Number))
  (check-true (<=? Real Real))
  (check-true (<=? Real Number))
  (check-true (<=? Rational Rational))
  (check-true (<=? Rational Real))
  (check-true (<=? Rational Number))
  (check-true (<=? Integer Integer))
  (check-true (<=? Integer Rational))
  (check-true (<=? Integer Real))
  (check-true (<=? Integer Number))
  (check-true (<=? Exact-Nonnegative-Integer Exact-Nonnegative-Integer))
  (check-true (<=? Exact-Nonnegative-Integer Integer))
  (check-true (<=? Exact-Nonnegative-Integer Rational))
  (check-true (<=? Exact-Nonnegative-Integer Real))
  (check-true (<=? Exact-Nonnegative-Integer Number))

  (check-false (<=? Number Real))
  (check-false (<=? Real Rational))
  (check-false (<=? Number Rational))
  (check-false (<=? Rational Integer))
  (check-false (<=? Real Integer))
  (check-false (<=? Number Integer))
  (check-false (<=? Integer Exact-Nonnegative-Integer))
  (check-false (<=? Rational Exact-Nonnegative-Integer))
  (check-false (<=? Real Exact-Nonnegative-Integer))
  (check-false (<=? Number Exact-Nonnegative-Integer))

  ;; Checks for numeric literals
  (check-true (<=? 10 Exact-Nonnegative-Integer))
  (check-true (<=? 10.0 Integer))
  (check-true (<=? -999 Integer))
  (check-true (<=? 10/3 Rational))
  (check-true (<=? 10.3 Rational))
  (check-false (<=? +inf.0 Rational))
  (check-false (<=? +nan.0 Rational))
  (check-true (<=? +inf.0 Real))
  (check-true (<=? +nan.0 Real))
  (check-true (<=? 3+2i Number))
  (check-true (<=? 3.0+2.0i Number))
  
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
  (check-equal? (lub #t (cons 1 2)) Truthy)
  (check-equal? (lub #f (cons 1 2)) Any)

  ;; (check-true (<=? Nothing (lub Nothing (random-value)))
  ;; (check-false (<=? (lub Nothing (random-value)) Nothing))
)