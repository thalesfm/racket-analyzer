#lang racket

(require rackunit
         "types.rkt")

;; TODO: Randomized tests?

(define/match (check-descending vs)
  [('()) (void)]
  [((cons v vs))
    (for ([elem (in-list vs)])
      (check-eq? (type<=? elem v) #t)
      (check-eq? (type<=? v elem) #f))
    (check-descending vs)])

;; Test cases for `type<=?`

;; Checks for `Top`
(check-eq? (type<=? Top Top) #t)
(check-eq? (type<=? 10 Top) #t)
(check-eq? (type<=? Number Top) #t)
(check-eq? (type<=? Top 10) #f)
(check-eq? (type<=? Top Number) #f)

;; Checks for `Bot`
(check-eq? (type<=? Bot Top) #t)
(check-eq? (type<=? Bot Bot) #t)
(check-eq? (type<=? Bot 10) #t)
(check-eq? (type<=? Bot Number) #t)

;; Checks for `Truthy`
(check-eq? (type<=? Truthy Truthy) #t)
(check-eq? (type<=? Number Truthy) #t)
(check-eq? (type<=? #f Truthy) #f)

;; Checks for `Char`, `String` and `Symbol`
(check-eq? (type<=? #\h Char) #t)
(check-eq? (type<=? "hello" String) #t)
(check-eq? (type<=? 'h Symbol) #t)

;; Checks for `Number`, `Real`, `Integer`, etc.
(check-descending (list Number Real Rational Integer Exact-Nonnegative-Integer))

;; Checks for numeric literals
(check-eq? (type<=? 10 Exact-Nonnegative-Integer) #t)
(check-eq? (type<=? 10.0 Integer) #t)
(check-eq? (type<=? -999 Integer) #t)
(check-eq? (type<=? 10/3 Rational) #t)
(check-eq? (type<=? 10.3 Rational) #t)
(check-eq? (type<=? +inf.0 Rational) #f)
(check-eq? (type<=? +nan.0 Rational) #f)
(check-eq? (type<=? +inf.0 Real) #t)
(check-eq? (type<=? +nan.0 Real) #t)
(check-eq? (type<=? 3+2i Number) #t)
(check-eq? (type<=? 3.0+2.0i Number) #t)

;; Checks for pairs and lists
(check-eq? (type<=? (Pairof 1 2) (Pairof 1 2)) #t)
(check-eq? (type<=? (Pairof 1 2) (Pairof 1 Top)) #t)
(check-eq? (type<=? (Pairof 1 2) (Pairof Top 2)) #t)
(check-eq? (type<=? (Pairof 1 2) (Pairof 1 Bot)) #f)
(check-eq? (type<=? (Pairof 1 2) (Pairof Bot 2)) #f)
(check-eq? (type<=? Null Null) #t)
(check-eq? (type<=? (Listof Number) (Listof Number)) #t)
(check-eq? (type<=? (Listof Integer) (Listof Number)) #t)
(check-eq? (type<=? Null (Listof Number)) #t)
(check-eq? (type<=? (Listof Number) Null) #f)
(check-eq? (type<=? (Pairof Number Null) (Listof Number)) #t)
(check-eq? (type<=? (Pairof 1 Null) (Listof Number)) #t)
;;(check-eq? (type<=? (Pairof 1 Null) (Listof String)) #f)
(check-eq? (type<=? (Pairof Number (Pairof Number Null)) (Listof Number)) #t)
(check-eq? (type<=? (Pairof 1 (Pairof 2 Null)) (Listof Number)) #t)
;;(check-eq? (type<=? (Pairof 1 (Pairof 2 Null)) (Listof String)) #f)

;; Test cases for `lub`

;; Checks for `Top`
(check-equal? (lub 10 Top) Top)
(check-equal? (lub Top 10) Top)

;; Checks for `Bot`
(check-equal? (lub 10 Bot) 10)
(check-equal? (lub Bot 10) 10)
(check-equal? (lub Bot Bot) Bot)

(check-equal? (lub 10 #t) Truthy)
(check-equal? (lub 10 #f) Top)

;; Checks for `Char`, `String` and `Symbol`
(check-equal? (lub #\a #\a) #\a)
(check-equal? (lub #\a #\b) Char)
(check-equal? (lub "hello" "hello") "hello")
(check-equal? (lub "hello" "hi") String)
(check-equal? (lub 'a 'a) 'a)
(check-equal? (lub 'a 'b) Symbol)

;; Checks for pairs and lists
(check-equal? (lub (Pairof 1 2) (Pairof 1 #f)) (Pairof 1 Top))
(check-equal? (lub (Pairof 1 2) (Pairof #f 2)) (Pairof Top 2))
(check-equal? (lub #t (Pairof 1 2)) Truthy)
(check-equal? (lub #f (Pairof 1 2)) Top)

(check-equal? (lub Null Null) Null)
(check-equal? (lub (Listof Number) (Listof Number)) (Listof Number))
(check-equal? (lub (Listof 10) (Listof #f)) (Listof Top))
(check-equal? (lub (Listof Number) Null) (Listof Number))
(check-equal? (lub Null (Listof Number)) (Listof Number))

(check-equal? (lub (Pairof Number Null) Null) (Listof Number))
(check-equal? (lub Null (Pairof Number Null)) (Listof Number))
(check-equal? (lub (Pairof Number Null) (Listof Number)) (Listof Number))
(check-equal? (lub (Listof Number) (Pairof Number Null)) (Listof Number))
(check-equal? (lub (Pairof 1 (Pairof 2 Null)) (Listof Number)) (Listof Number))
(check-equal? (lub (Listof Number) (Pairof 1 (Pairof 2 Null))) (Listof Number))
(check-equal? (lub (Pairof 1 (Pairof 2 Null)) (Listof Top)) (Listof Top))
(check-equal? (lub (Listof Top) (Pairof 1 (Pairof 2 Null))) (Listof Top))
(check-equal? (lub (Pairof 1 2) (Listof Number)) Truthy)
(check-equal? (lub (Listof Number) (Pairof 1 2)) Truthy)
