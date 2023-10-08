#lang racket

(provide make-base-environment)

(require "environment.rkt"
         "domain.rkt"
         "types.rkt")

(define (constant? v)
  (if (Pairof? v)
      (and (Literal? (car v))
           (Literal? (cdr v)))
      (Literal? v)))

(define (lift proc)
  (lambda args
    (if (andmap constant? args)
        (datum->type (apply proc (map Literal-value args)))
        (Top))))

(define (make-base-environment)
  (let* ([env (make-empty-environment)]
         [env (environment-set env #'+ (lift +))]
         [env (environment-set env #'- (lift -))]
         [env (environment-set env #'* (lift *))]
         [env (environment-set env #'/ (lift /))]
         [env (environment-set env #'= (lift =))]
         [env (environment-set env #'map (lift map))]
         [env (environment-set env #'read (λ () (Top)))]
         [env (environment-set env #'error (λ () (⊥ type-domain)))])
    env))
