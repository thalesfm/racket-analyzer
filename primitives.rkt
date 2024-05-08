#lang racket

(provide primitives-namespace)

(require syntax/parse/define
         "domain.rkt")

(define primitives-namespace (make-base-namespace))

(define-syntax-parse-rule (define-primitive id:id expr)
  (namespace-set-variable-value! 'id expr #t primitives-namespace #t))

(define-syntax-parse-rule (define-primitive/lift id:id expr)
  (define-primitive id (lift expr)))

(define ((lift proc) . args)
  (cond
   [(not (procedure-arity-includes? proc (length args))) (⊥ "arity mismatch")]
   [(andmap natural? args) (apply proc args)]
   [else T]))

(define-primitive/lift read
  (lambda () T))
(define-primitive/lift error
  (lambda () (⊥ "error")))
(define-primitive/lift + (procedure-reduce-arity + 2))
(define-primitive/lift - (procedure-reduce-arity - 2))
(define-primitive/lift * (procedure-reduce-arity * 2))
(define-primitive/lift / (procedure-reduce-arity / 2))
(define-primitive/lift = (procedure-reduce-arity = 2))