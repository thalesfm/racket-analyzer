#lang racket

(require "bind-time.rkt")

(provide (all-defined-out))

;; TODO: Add contract to lifted procedures
;; TODO: Handle bind-times which are marked as potentially non-terminating

(define (lift proc)
  (lambda args
	(cond
	  [(andmap singleton? args) (apply proc args)]
	  [else top])))

(define (lift-with-contract proc args-bt out-bt)
  (lambda args
	(cond
	  [(not (= (length args) (length args-bt))) bot]
	  [(and (andmap bind-time<=? args args-bt)
	        (andmap singleton? args))
	   (apply proc args)]
	  [(andmap bind-time-comparable?  args args-bt) out-bt]
	  [else bot])))

(define bind-time+
  (lift-with-contract + (list number? number?) number?))

(define bind-time-
  (lift-with-contract - (list number? number?) number?))

(define bind-time*
  (lift-with-contract * (list number? number?) number?))

(define bind-time/
  (lift-with-contract / (list number? number?) number?))
