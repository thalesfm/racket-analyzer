#lang racket

(require (for-syntax rackunit))
(require turnstile)
(require "type.rkt")

;; Tests for type<=?
(begin-for-syntax
  (match-define
    (list top nothing number real integer boolean string)
    (map (current-type-eval)
         (list #'Top #'Nothing #'Number #'Real #'Integer #'Boolean #'String)))

  (define a-real-literal ((current-type-eval) (datum->type 1.0)))
  (define an-integer-literal ((current-type-eval) (datum->type 10)))

  ;; TODO: Cases for Bot

  ;; Cases for Top and Nothing
  (check-true
    (type<=? ((current-type-eval) #'Top)
             ((current-type-eval) #'Top)))
  (check-true
    (type<=? ((current-type-eval) #'Nothing)
             ((current-type-eval) #'Nothing)))
  (check-true
    (type<=? ((current-type-eval) #'Nothing)
             ((current-type-eval) #'Top)))
  (check-true
    (type<=? ((current-type-eval) (datum->type 10))
             ((current-type-eval) #'Top)))
  (check-false
    (type<=? ((current-type-eval) #'Top)
             ((current-type-eval) #'Nothing)))
  (check-false
    (type<=? ((current-type-eval) #'Top)
             ((current-type-eval) (datum->type 10))))
  (check-true
    (type<=? ((current-type-eval) #'Nothing)
             ((current-type-eval) #'Number)))
  (check-true
    (type<=? ((current-type-eval) #'Nothing)
             ((current-type-eval) (datum->type 10))))
  (check-false
    (type<=? ((current-type-eval) #'Number)
             ((current-type-eval) #'Nothing)))
  (check-false
    (type<=? ((current-type-eval) (datum->type 10))
             ((current-type-eval) #'Nothing)))

  ;; Cases for Number, Real, and Integer
  (check-true
    (type<=? ((current-type-eval) #'Number)
             ((current-type-eval) #'Number)))
  (check-true
    (type<=? ((current-type-eval) #'Real)
             ((current-type-eval) #'Real)))
  (check-true
    (type<=? ((current-type-eval) #'Integer)
             ((current-type-eval) #'Integer)))
  (check-true
    (type<=? ((current-type-eval) #'Real)
             ((current-type-eval) #'Number)))
  (check-true
    (type<=? ((current-type-eval) #'Integer)
             ((current-type-eval) #'Number)))
  (check-true
    (type<=? ((current-type-eval) #'Integer)
             ((current-type-eval) #'Real)))
  (check-false
    (type<=? ((current-type-eval) #'Number)
             ((current-type-eval) #'Real)))
  (check-false
    (type<=? ((current-type-eval) #'Number)
             ((current-type-eval) #'Integer)))
  (check-false
    (type<=? ((current-type-eval) #'Real)
             ((current-type-eval) #'Integer)))

  ;; Cases for numeric literals
  (check-true
    (type<=? ((current-type-eval) (datum->type 10))
             ((current-type-eval) #'Integer)))
  (check-true
    (type<=? ((current-type-eval) (datum->type 10))
             ((current-type-eval) #'Real)))
  (check-true
    (type<=? ((current-type-eval) (datum->type 10))
             ((current-type-eval) #'Number)))
  (check-false
    (type<=? ((current-type-eval) (datum->type 1.0))
             ((current-type-eval) #'Integer)))
  (check-true
    (type<=? ((current-type-eval) (datum->type 1.0))
             ((current-type-eval) #'Real)))
  (check-true
    (type<=? ((current-type-eval) (datum->type 1.0))
             ((current-type-eval) #'Number)))
  (check-false
    (type<=?  ((current-type-eval) #'Integer)
              ((current-type-eval) (datum->type 10))))
  (check-false
    (type<=?  ((current-type-eval) #'Real)
              ((current-type-eval) (datum->type 1.0)))))

  ;; TODO: Cases for Boolean and String

  ;; TODO: Cases for Pairof

;; Tests for lub
;; TODO
