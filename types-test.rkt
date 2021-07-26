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

;; Test cases for Boolean and String
(begin-for-syntax
  (check-true (⊑ Boolean Boolean))
  (check-true (⊑ Boolean ⊤))
  (check-true (⊑ ⊥ (⊔ Boolean ⊥)))
  (check-true (⊑ String String))
  (check-true (⊑ String ⊤))
  (check-true (⊑ ⊥ (⊔ String ⊥)))

  (check-false (⊑ ⊤ Boolean))
  (check-false (⊑ ⊥ Boolean))
  (check-false (⊑ Boolean String))
  (check-false (⊑ String Boolean)))

;; TODO: Test cases for pairs
(begin-for-syntax
  (check-true (⊑ (datum->type (cons 1 2)) (datum->type (cons 1 2))))
  (check-true (⊑ (datum->type (cons 1 2))
                 (Cons (datum->type 1) (datum->type 2))))
  (check-true (⊑ (datum->type (cons 1 2)) ⊤))
  (check-true (⊑ ⊥ (⊔ (datum->type (cons 1 2)) ⊥)))
  (check-true (⊑ (datum->type (cons 1 2)) (Cons ⊤ (datum->type 2))))
  (check-true (⊑ (datum->type (cons 1 2)) (Cons (datum->type 1) ⊤)))
  (check-true (⊑ (datum->type '(1 2 . 3))
                 (Cons (datum->type 1) (Cons (datum->type 2) ⊤))))
  (check-true (⊑ (datum->type '(1 2 3))
                 (Cons ⊤ (datum->type '(2 3)))))

  (check-false (⊑ (Cons ⊤ (datum->type 2)) (datum->type (cons 1 2))))
  (check-false (⊑ (Cons (datum->type 1) ⊤) (datum->type (cons 1 2))))
  (check-false (⊑ (Cons (datum->type 1) (Cons (datum->type 2) ⊤))
                  (datum->type '(1 2 . 3))))
  (check-false (⊑ (Cons ⊤ (datum->type '(2 3)))
                  (datum->type '(1 2 3)))))

;; Tests for ⊔
;; TODO
