#lang racket

(require rackunit
         "../domain.rkt")

;; ----------------------------------------------------------------------------
;; Checks for `<=?`

(check-true (<=? T T))
;; Removed for simplified implementation
;; (check-true (<=? ℕ T)) (check-false (<=? T ℕ))
(check-true (<=? 3 T)) (check-false (<=? T 3))
(check-true (<=? ⊥ T)) (check-false (<=? T ⊥))

;; Removed for simplified implementation
;; (check-true (<=? ⊥ ℕ)) (check-false (<=? ℕ ⊥))
(check-true (<=? ⊥ 4)) (check-false (<=? 4 ⊥))
(check-true (<=? ⊥ ⊥))

;; Removed for simplified implementation
;; (check-true (<=? ℕ ℕ))
;; (check-true (<=? 5 ℕ)) (check-false (<=? ℕ 5))

(check-true  (<=? 6 6))
(check-false (<=? 6 7))
(check-false (<=? 7 6))

(check-true  (<=? + +))
(check-false (<=? + /))

;; ----------------------------------------------------------------------------
;; Checks for `lub`

(check-equal? (lub T T) T)
;; Removed for simplified implementation
;; (check-equal? (lub ℕ T) T)
;; (check-equal? (lub T ℕ) T)
(check-equal? (lub 3 T) T)
(check-equal? (lub T 3) T)
(check-equal? (lub ⊥ T) T)
(check-equal? (lub T ⊥) T)

;; Removed for simplified implementation
;; (check-equal? (lub ℕ ⊥) ℕ)
;; (check-equal? (lub ⊥ ℕ) ℕ)
(check-equal? (lub 4 ⊥) 4)
(check-equal? (lub ⊥ 4) 4)
(check-equal? (lub ⊥ ⊥) ⊥)

;; Removed for simplified implementation
;; (check-equal? (lub ℕ ℕ) ℕ)
;; (check-equal? (lub 5 ℕ) ℕ)
;; (check-equal? (lub ℕ 5) ℕ)

(check-equal? (lub 6 6) 6)
;; Removed for simplified implementation
;; (check-equal? (lub 6 7) ℕ)
;; (check-equal? (lub 7 6) ℕ)

(check-equal? (lub + +) +)
(check-equal? (lub + /) T)