#lang racket

(require rackunit "types.rkt" "order.rkt")

(test-case
  "Tests for <=?"

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
  (check-true (<=? Nothing (lub Any Nothing)))
  (check-true (<=? Nothing (lub 10 Nothing)))
  (check-true (<=? Nothing (lub Number Nothing)))

  (check-false (<=? (lub Any Nothing) Nothing))
  (check-false (<=? (lub 10 Nothing) Nothing))
  (check-false (<=? (lub Number Nothing) Nothing))
  
  ;; Checks for `Number`, `Real`, `Integer`, etc.
  (check-true (<=? Number Number))
  (check-true (<=? Real Real))
  (check-true (<=? Integer Integer))
  (check-true (<=? Real Number))
  (check-true (<=? Integer Number))
  (check-true (<=? Integer Real))
  
  (check-false (<=? Number Real))
  (check-false (<=? Number Integer))
  (check-false (<=? Real Integer))
  
  ;; Checks for numeric literals
  (check-true (<=? 10 Integer))
  (check-true (<=? 10 Real))
  (check-true (<=? 10 Number))
  (check-true (<=? 1.0 Integer))
  (check-true (<=? 1.0 Number))
  
  (check-false (<=? Integer 10))
  (check-false (<=? Real 1.0))
  
  ;; TODO: Checks for pairs
)

;; TODO: Tests for `lub`