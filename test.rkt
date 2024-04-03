#lang racket

(define (fixpoint proc)
  (define (f x)
    (lambda (y)
      ((proc (x x)) y)))
  (f f))

(define fact
  (let ()
    (define (fact/recur recur-proc)
      (lambda (n)
        (if (= n 0)
            1
            (* n (recur-proc (sub1 n))))))
    (fixpoint fact/recur)))

(define (fixpoint* proc)
  (define (f x)
    (lambda (y)
