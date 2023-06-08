#lang racket

(require rackunit
         "partial-eval.rkt"
         "types.rkt")

(check-equal? (partial-eval 10) 10)
(check-equal? (partial-eval '(let ([x 10]) x)) 10)
(check-equal? (partial-eval '(let ([x 'not-ok]) (let ([x 'ok]) x))) 'ok)
(check-equal? (partial-eval '(+ 10 11)) 21)
(check-not-exn
 (lambda ()
   (partial-eval '(lambda (x) x))))
(check-equal? (partial-eval '((lambda (x) x) 10)) 10)
(check-equal? (partial-eval '((lambda (x) (+ x 11)) 10)) 21)
(check-equal? (partial-eval '(if #t 'ok 'not-ok)) 'ok)
(check-equal? (partial-eval '(if #f 'not-ok 'ok)) 'ok)
(check-equal? (partial-eval '(+ 1 (* 3 (- 10 7)))) 10)
(check-equal? (partial-eval '(let ([f +]) (f 10 11))) 21)

(check-equal? (partial-eval '(let ([x 10] [y 11]) (+ x y))) 21)

(check-exn
 exn:fail?
 (lambda ()
   (partial-eval '(let ([x 10] [x 11]) x))))
(check-equal?
 (partial-eval
  '(letrec ([fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))])
     (fac 5)))
 120)

(check-equal?
  (partial-eval
   '(letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))]
             [odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))])
      (even? 101)))
  #f)

(check-eq? (partial-eval '(read)) Any)
(check-eq? (partial-eval '(+ (read) 10)) Any)

(check-equal? (partial-eval '(if #t 'ok (read))) 'ok)
(check-equal? (partial-eval '(if #f (read) 'ok)) 'ok)
(check-equal? (partial-eval '(if (read) 10 10)) 10)
(check-true (<=? (partial-eval '(if (read) 10 11)) Number))

(check-eq? (partial-eval '(error)) Nothing)
(check-eq? (partial-eval '(let ([x (error)]) 'unused)) Nothing)
(check-eq? (partial-eval '(letrec ([x (error)]) 'unused)) Nothing)
(check-eq? (partial-eval '((error) 'unused)) Nothing)
(check-eq? (partial-eval '(+ (error) 10)) Nothing)
(check-eq? (partial-eval '(+ 10 (error))) Nothing)

(check-eq? (partial-eval 'x) Nothing)
(check-eq? (partial-eval '(let ([x 10]) y)) Nothing)
(check-eq? (partial-eval '(if (read) 10 x)) 10)
