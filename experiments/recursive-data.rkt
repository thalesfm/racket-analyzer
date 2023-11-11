#lang racket

(struct closure (lambda environment)
        #:prefab)

(define (empty-environment)
  (hash))

(define (environment-set env id v)
  (hash-set env id v))

;; NOTE: This works but probably does a shallow copy of the entire hash!
(define (make-recursive-closure id lam env)
  (let* ([ph (make-placeholder #f)]
         [env/ph (environment-set env id ph)]
         [clo (closure lam env/ph)])
    (placeholder-set! ph clo)
    (make-reader-graph clo)))

(make-recursive-closure 'f 'lambda (empty-environment))
