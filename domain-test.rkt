#lang racket

(require rackunit
         "domain.rkt")

;; Tests for α and γ
(check
 
;; Tests for ⊑

;; Test cases for ⊤
(check-true (⊑ ⊤ ⊤))
(check-true (⊑ (α 10) ⊤))
(check-true (⊑ Number ⊤))
(check-false (⊑ ⊤ (α 10)))
(check-false (⊑ ⊤ Number))

;; Test cases for ⊥
(check-true (⊑ ⊥ ⊥))
(check-true (⊑ ⊥ (⊔ ⊤ ⊥)))
(check-true (⊑ ⊥ (⊔ (α 10) ⊥)))
(check-true (⊑ ⊥ (⊔ Number ⊥)))

(check-false (⊑ ⊥ ⊤))
(check-false (⊑ ⊥ (α 10)))
(check-false (⊑ ⊥ Number))
(check-false (⊑ (⊔ ⊤ ⊥) ⊥))
(check-false (⊑ (⊔ (α 10) ⊥) ⊥))
(check-false (⊑ (⊔ Number ⊥) ⊥))

;; Test cases for Number, Real, and Integer
(check-true (⊑ Number Number))
(check-true (⊑ Real Real))
(check-true (⊑ Integer Integer))
(check-true (⊑ Real Number))
(check-true (⊑ Integer Number))
(check-true (⊑ Integer Real))

(check-false (⊑ Number Real))
(check-false (⊑ Number Integer))
(check-false (⊑ Real Integer))

;; Cases for numeric literals
(check-true (⊑ (α 10) Integer))
(check-true (⊑ (α 10) Real))
(check-true (⊑ (α 10) Number))
(check-true (⊑ (α 1.0) Real))
(check-true (⊑ (α 1.0) Number))

(check-false (⊑ (α 1.0) Integer))
(check-false (⊑ Integer (α 10)))
(check-false (⊑ Real (α 1.0)))

;; TODO: Test cases for Boolean and String

;; TODO: Test cases for Pair

;; Tests for ⊔
;; TODO
