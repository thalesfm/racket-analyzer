#lang racket

(require rackunit "partial-eval.rkt")

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
