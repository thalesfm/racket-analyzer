#lang racket

(require "bind-time.rkt")
(require "bind-time-lift.rkt")

(provide (rename-out [bt+ +]
		     [bt- -]
		     [bt* *]
		     [bt/ /]))

(define bt+
  (lift/contract (-> number? number? number?) +))

(define bt-
  (lift/contract (-> number? number? number?) -))

(define bt*
  (lift/contract (-> number? number? number?) *))

(define bt/
  (lift/contract (-> number? number? number?) /))
