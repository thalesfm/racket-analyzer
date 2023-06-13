#lang racket

(require rackunit
         "types.rkt")

(test-case
  "Partial order tests"

  (define/match (check-descending vs)
    [('()) (void)]
    [((cons v vs))
     (for ([elem (in-list vs)])
       (check-eq? (type<=? elem v) #t)
       (check-eq? (type<=? v elem) #f))
     (check-descending vs)])

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

  ;; TODO: Checks for pairs
)

(test-case
  "Tests for the least-upper-bound"

  ;; Checks for `Top`
  (check-equal? (lub 10 Top) Top)
  (check-equal? (lub Top 10) Top)

  ;; Checks for `Bot`
  (check-equal? (lub 10 Bot) 10)
  (check-equal? (lub Bot 10) 10)
  (check-equal? (lub Bot Bot) Bot)

  ;; Checks for pairs
  (check-equal? (lub (Pairof 1 2) (Pairof 1 #f)) (Pairof 1 Top))
  (check-equal? (lub (Pairof 1 2) (Pairof #f 2)) (Pairof Top 2))
  (check-equal? (lub #t (Pairof 1 2)) Truthy)
  (check-equal? (lub #f (Pairof 1 2)) Top)

  ;; (check-eq? (type<=? Bot (lub Bot (random-value)) #t)
  ;; (check-eq? (type<=? (lub Bot (random-value)) Bot) #f)
)
