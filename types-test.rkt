#lang racket

(require rackunit
         "types.rkt")

;; Tests for type<=?

;; TODO: Cases for Bot

;; Cases for Top and Empty
(check-true (type<=? Top Top))
(check-true (type<=? Empty Empty))
(check-true (type<=? Empty Top))
(check-true (type<=? Number Top))
(check-true (type<=? Empty Number))

(check-false (type<=? Top Empty))
(check-false (type<=? Top Number))
(check-false (type<=? Number Empty))

;; Cases for Number, Real, and Integer
(check-true (type<=? Number Number))
(check-true (type<=? Real Real))
(check-true (type<=? Integer Integer))
(check-true (type<=? Real Number))
(check-true (type<=? Integer Number))
(check-true (type<=? Integer Real))

(check-false (type<=? Number Real))
(check-false (type<=? Number Integer))
(check-false (type<=? Real Integer))

;; Cases for numeric literals
(check-true (type<=? (datum->type 10) Integer))
(check-true (type<=? (datum->type 10) Real))
(check-true (type<=? (datum->type 10) Number))
(check-true (type<=? (datum->type 1.0) Real))
(check-true (type<=? (datum->type 1.0) Number))

(check-false (type<=? (datum->type 1.0) Integer))
(check-false (type<=? Integer (datum->type 10)))
(check-false (type<=? Real (datum->type 1.0)))

;; TODO: Cases for Boolean and String

;; TODO: Cases for Pairof

;; Tests for lub
;; TODO
