#lang racket

(define saved-k #f)

(define (setjmp! . args)
  (call/cc
    (lambda (k)
      (set! saved-k k)
      (apply k args))))

(define (longjmp . args)
  (apply saved-k args))

(define (foo)
  (define bar 10)
  (setjmp!)
  (displayln bar)
  (set! bar 11))

(foo)
(longjmp)
