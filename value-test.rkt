#lang racket

(require rackunit "types.rkt" "value.rkt")

(test-case
  "Tests for `value<=?`"

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
  (check-true (value<=? Nothing (lub Any Nothing)))
  (check-true (value<=? Nothing (lub 10 Nothing)))
  (check-true (value<=? Nothing (lub Number Nothing)))

  (check-false (value<=? (lub Any Nothing) Nothing))
  (check-false (value<=? (lub 10 Nothing) Nothing))
  (check-false (value<=? (lub Number Nothing) Nothing))
  
  ;; Checks for `Number`, `Real`, `Integer`, etc.
  (check-true (value<=? Number Number))
  (check-true (value<=? Real Real))
  (check-true (value<=? Integer Integer))
  (check-true (value<=? Real Number))
  (check-true (value<=? Integer Number))
  (check-true (value<=? Integer Real))
  
  (check-false (value<=? Number Real))
  (check-false (value<=? Number Integer))
  (check-false (value<=? Real Integer))
  
  ;; Checks for numeric literals
  (check-true (value<=? 10 Integer))
  (check-true (value<=? 10 Real))
  (check-true (value<=? 10 Number))
  (check-true (value<=? 1.0 Integer))
  (check-true (value<=? 1.0 Number))
  
  (check-false (value<=? Integer 10))
  (check-false (value<=? Real 1.0))
  
  ;; TODO: Checks for pairs
)

;; TODO: Tests for `lub`