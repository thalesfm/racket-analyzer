#lang racket

(require rackunit
         "abstract-eval.rkt"
         "environment.rkt"
         "types.rkt")

(check-equal? (abstract-eval 10) 10)
(check-equal? (abstract-eval '(let ([x 10]) x)) 10)
(check-equal? (abstract-eval '(let ([x 'not-ok]) (let ([x 'ok]) x))) 'ok)
(check-equal? (abstract-eval '(+ 10 11)) 21)
(check-not-exn
 (lambda ()
   (abstract-eval '(lambda (x) x))))
(check-equal? (abstract-eval '((lambda (x) x) 10)) 10)
(check-equal? (abstract-eval '((lambda (x) (+ x 11)) 10)) 21)
(check-equal? (abstract-eval '(if #t 'ok 'not-ok)) 'ok)
(check-equal? (abstract-eval '(if #f 'not-ok 'ok)) 'ok)
(check-equal? (abstract-eval '(+ 1 (* 3 (- 10 7)))) 10)
(check-equal? (abstract-eval '(let ([f +]) (f 10 11))) 21)

(check-equal? (abstract-eval '(let ([x 10] [y 11]) (+ x y))) 21)

(check-exn
 exn:fail?
 (lambda ()
   (abstract-eval '(let ([x 10] [x 11]) x))))

; FIXME: Goes into an infinite loop because of `trace` (regression)
#;(check-equal?
 (abstract-eval
  '(letrec ([fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))])
     (fac 5)))
 120)

; FIXME: Goes into an infinite loop because of `trace` (regression)
#;(check-equal?
 (abstract-eval
  '(letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))]
            [odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))])
     (even? 101)))
 #f)

(check-eq? (abstract-eval '(read)) Top)
(check-eq? (abstract-eval '(+ (read) 10)) Top)

(check-equal? (abstract-eval '(if #t 'ok (read))) 'ok)
(check-equal? (abstract-eval '(if #f (read) 'ok)) 'ok)
(check-equal? (abstract-eval '(if (read) 10 10)) 10)
(check-true (type<=? (abstract-eval '(if (read) 10 11)) Number))

(check-eq? (abstract-eval '(error)) Bot)
(check-eq? (abstract-eval '(let ([x (error)]) 'unused)) Bot)
(check-eq? (abstract-eval '(letrec ([x (error)]) 'unused)) Bot)
(check-eq? (abstract-eval '((error) 'unused)) Bot)
(check-eq? (abstract-eval '(+ (error) 10)) Bot)
(check-eq? (abstract-eval '(+ 10 (error))) Bot)

(check-eq? (abstract-eval 'x) Bot)
(check-eq? (abstract-eval '(let ([x 10]) y)) Bot)
(check-eq? (abstract-eval '(if (read) 10 x)) 10)

(check-not-eq? (abstract-eval '(if (read) + +)) T)

(check-true
 (closure?
   (abstract-eval
     '(let ([f (lambda (x) x)])
        (if (read) f f)))))

(check-eq?
 (environment-ref
  (closure-environment
   (abstract-eval
     '(let ([c (lambda (x) (lambda () x))])
        (if (read)
            (c 10)
            (c 10)))))
  'x)
 10)

(check-true
 (type<=?
  (environment-ref
   (closure-environment
    (abstract-eval
      '(let ([c (lambda (x) (lambda () x))])
         (if (read)
             (c 10)
             (c 11)))))
   'x)
  Number))

(check-eq?
 (abstract-eval
   '(if (read)
        (lambda (x) x)
        (lambda (y) y)))
 Top)
