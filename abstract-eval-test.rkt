#lang racket

(require rackunit
         "abstract-eval.rkt"
         "domain.rkt")

(check-equal? (abstract-eval '3) 3)
(check-equal? (abstract-eval '+) +)
(check-equal? (abstract-eval '(+ 3 4)) 7)
(check-equal? (abstract-eval '(let ([x 5]) x)) 5)
(check-equal? (abstract-eval '(let ([x 5]) x)) 5)
(check-equal? (abstract-eval '(if  2 3 4)) 3)
(check-equal? (abstract-eval '(if  0 3 4)) 3)
(check-equal? (abstract-eval '(if #f 3 4)) 4)
(check-equal? (abstract-eval '(if (read) 3 4)) ℕ)
(check-equal? (abstract-eval '((lambda (x) x) 6)) 6)
(check-equal? (abstract-eval '((lambda (x y) (+ x y)) 3 4)) 7)

(check-equal?
 (abstract-eval
  '(let ()
     (define (fact n)
       (if (= n 0)
           1
           (* n (fact (- n 1)))))
     (fact 5)))
 120)

(check-equal?
 (abstract-eval
  '(let* ([c (lambda (k) (lambda (_) k))]
          [f (if (read) (c 3) (c 3))])
     (f 7)))
 3)

(check-equal?
 (abstract-eval
  '(let* ([c (lambda (k) (lambda (_) k))]
          [f (if (read) (c 3) (c 4))])
     (f 7)))
 ℕ)

(check-equal?
 (abstract-eval
  '(let ()
     (define (Y f)
       (let ([x (λ (x) (λ (y) (f (x x) y)))])
         (x x)))
     (define (f r n)
       (if (= n 0)
           1
           (* n (r (- n 1)))))
     ((Y f) 5)))
 120)

;; FIXME
(check-equal?
 (abstract-eval
  '(let* ([Y (lambda (f)
               (let ([z (lambda (z)
                          (lambda (x) (f (z z) x)))])
                 (z z)))]
          [f (lambda (f n)
               (if (= n 0)
                   1
                   (* n (f (- n 1)))))])
     ((Y f) (read))))
 120)