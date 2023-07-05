#lang racket

(require "types.rkt")

(provide lift)

(define (constant? v)
  (if (pair? v)
      (and (literal-type? (car v))
           (literal-type? (cdr v)))
      (literal-type? v)))

(define (lift proc)
  (lambda args
    (if (andmap constant? args) (apply proc args) Top)))

#|
(define-syntax lift/contract
  (syntax-rules (->)
    [(lift/contract (-> dom ... range) proc)
     (lift/contract-helper (list dom ...) range proc)]))

(define (lift/contract-helper args-bt out-bt proc)
  (lambda args
    (cond
      [(not (= (length args) (length args-bt))) bot]
      [(and (andmap bind-time<=? args args-bt)
            (andmap singleton? args))
       (apply proc args)]
      [(andmap bind-time-comparable? args args-bt) out-bt]
      [else bot])))
|#

#|
(define bind-time+
  (lift/contract + (list number? number?) number?))

(define bind-time-
  (lift/contract - (list number? number?) number?))

(define bind-time*
  (lift/contract * (list number? number?) number?))

(define bind-time/
  (lift/contract / (list number? number?) number?))
|#
