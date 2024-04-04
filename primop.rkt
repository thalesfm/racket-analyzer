#lang racket

(provide get-primop)

(require syntax/parse/define
         "abstract-value.rkt")

(define prim-tab (make-hasheq))

(define (get-primop sym)
  (hash-ref prim-tab sym))

(define-syntax-parse-rule (define-primitive id:id expr)
  (hash-set! prim-tab 'id expr))

(define-syntax-parse-rule (define-primitive/lift id:id expr)
  (define-primitive id (lift expr)))

(define ((lift proc) . args)
  (cond
   [(not (procedure-arity-includes? proc (length args))) ⊥]
   [(andmap natural? args) (apply proc args)]
   [(ormap ⊥? args) ⊥]
   [else T]))

(define-primitive/lift read
  (lambda () T))
(define-primitive/lift error
  (lambda () ⊥))
(define-primitive/lift + (procedure-reduce-arity + 2))
(define-primitive/lift - (procedure-reduce-arity - 2))
(define-primitive/lift * (procedure-reduce-arity * 2))
(define-primitive/lift / (procedure-reduce-arity / 2))
(define-primitive/lift = (procedure-reduce-arity = 2))