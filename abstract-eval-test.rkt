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

;; FIXME: Primitives don't handle abstract values properly
(check-equal?  (abstract-eval '(if (= (read) 2) 3 4)) ℕ)

(check-equal?
  (abstract-eval
   '(let ()
      (define (times m)
        (letrec ([recur (λ (n)
                           (if (= n 0)
                               0
                               (+ m (recur (- n 1)))))])
          recur))
      ((times 3) 4)))
 12)

(check-equal?
  (abstract-eval
   '(let ()
      (define (times m)
        (letrec ([recur (λ (n)
                           (if (= n 0)
                               0
                               (+ m (recur (- n 1)))))])
          recur))
      (let ([f (if (read) (times 3) (times 3))])
        (f 4))))
 12)