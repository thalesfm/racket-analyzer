#lang racket

(require turnstile "types.rkt")
(require (for-syntax rackunit))

;; Tests for datum->type and type->datum
;; TODO
 
;; Tests for ⊑

;; Test cases for ⊤
(begin-for-syntax
  (check-true (⊑ ⊤ ⊤))
  (check-true (⊑ (datum->type 10) ⊤))
  (check-true (⊑ Number ⊤))
  
  (check-false (⊑ ⊤ (datum->type 10)))
  (check-false (⊑ ⊤ Number)))

;; Test cases for ⊥
(begin-for-syntax
  (check-true (⊑ ⊥ ⊥))
  (check-true (⊑ ⊥ (⊔ ⊤ ⊥)))
  (check-true (⊑ ⊥ (⊔ (datum->type 10) ⊥)))
  (check-true (⊑ ⊥ (⊔ Number ⊥)))

  (check-false (⊑ ⊥ ⊤))
  (check-false (⊑ ⊥ (datum->type 10)))
  (check-false (⊑ ⊥ Number))
  (check-false (⊑ (⊔ ⊤ ⊥) ⊥))
  (check-false (⊑ (⊔ (datum->type 10) ⊥) ⊥))
  (check-false (⊑ (⊔ Number ⊥) ⊥)))

;; Test cases for Number, Real, and Integer
(begin-for-syntax
  (check-true (⊑ Number Number))
  (check-true (⊑ Real Real))
  (check-true (⊑ Integer Integer))
  (check-true (⊑ Real Number))
  (check-true (⊑ Integer Number))
  (check-true (⊑ Integer Real))

  (check-false (⊑ Number Real))
  (check-false (⊑ Number Integer))
  (check-false (⊑ Real Integer)))

;; Cases for numeric literals
(begin-for-syntax
  (check-true (⊑ (datum->type 10) Integer))
  (check-true (⊑ (datum->type 10) Real))
  (check-true (⊑ (datum->type 10) Number))
  (check-true (⊑ (datum->type 1.0) Real))
  (check-true (⊑ (datum->type 1.0) Number))

  (check-false (⊑ (datum->type 1.0) Integer))
  (check-false (⊑ Integer (datum->type 10)))
  (check-false (⊑ Real (datum->type 1.0))))

;; TODO: Test cases for Boolean and String

;; TODO: Test cases for Pair

;; Tests for ⊔
;; TODO
