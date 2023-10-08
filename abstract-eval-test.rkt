#lang racket

(require rackunit
         "abstract-eval.rkt"
         "environment.rkt"
         "ordering.rkt"
         "types.rkt")

(current-namespace (make-base-namespace))

(check-equal? (abstract-eval 10) (datum->type 10))
(check-equal? (abstract-eval '(let ([x 10]) x)) (datum->type 10))
(check-equal?
 (abstract-eval '(let ([x 'not-ok]) (let ([x 'ok]) x)))
                (datum->type 'ok))
(check-equal? (abstract-eval '(+ 10 11)) (datum->type 21))
(check-not-exn
 (lambda ()
   (abstract-eval '(lambda (x) x))))
(check-equal? (abstract-eval '((lambda (x) x) 10)) (datum->type 10))
(check-equal? (abstract-eval '((lambda (x) (+ x 11)) 10)) (datum->type 21))
(check-equal? (abstract-eval '(if #t 'ok 'not-ok)) (datum->type 'ok))
(check-equal? (abstract-eval '(if #f 'not-ok 'ok)) (datum->type 'ok))
(check-equal? (abstract-eval '(+ 1 (* 3 (- 10 7)))) (datum->type 10))
(check-equal? (abstract-eval '(let ([f +]) (f 10 11))) (datum->type 21))

(check-equal? (abstract-eval '(let ([x 10] [y 11]) (+ x y))) (datum->type 21))

(check-exn
 exn:fail?
 (lambda ()
   (abstract-eval '(let ([x 10] [x 11]) x))))

; FIXME: Evaluates to T because we don't check primitive contracts
(check-equal?
 (abstract-eval
  '(letrec ([fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))])
     (fac 5)))
 (Exact-Nonnegative-Integer)
 "this is expected to fail for now")

; FIXME: Fails because closures are no longer `#:transparent` (regression)
(check-equal?
 (abstract-eval
  '(letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))]
            [odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))])
     (even? 101)))
 T)

(check-equal? (abstract-eval '(read)) T)
(check-equal? (abstract-eval '(+ (read) 10)) T)

(check-equal? (abstract-eval '(if #t 'ok (read))) (datum->type 'ok))
(check-equal? (abstract-eval '(if #f (read) 'ok)) (datum->type 'ok))
(check-equal? (abstract-eval '(if (read) 10 10)) (datum->type 10))
(check-true (<=? (abstract-eval '(if (read) 10 11)) (Number)))

(check-equal? (abstract-eval '(error)) ⊥)
(check-equal? (abstract-eval '(let ([x (error)]) 'unused)) ⊥)
(check-equal? (abstract-eval '(letrec ([x (error)]) 'unused)) ⊥)
(check-equal? (abstract-eval '((error) 'unused)) ⊥)
(check-equal? (abstract-eval '(+ (error) 10)) ⊥)
(check-equal? (abstract-eval '(+ 10 (error))) ⊥)

(check-equal? (abstract-eval 'x) ⊥)
(check-equal? (abstract-eval '(let ([x 10]) y)) ⊥)
(check-equal? (abstract-eval '(if (read) 10 x)) (datum->type 10))

(check-not-equal? (abstract-eval '(if (read) + +)) T)

(check-true
 (closure?
   (abstract-eval
     '(let ([f (lambda (x) x)])
        (if (read) f f)))))

(check-equal?
 (environment-ref
  (closure-environment
   (abstract-eval
     '(let ([c (lambda (x) (lambda () x))])
        (if (read)
            (c 10)
            (c 10)))))
  #'x)
 (datum->type 10))

(check-true
 (<=?
  (environment-ref
   (closure-environment
    (abstract-eval
      '(let ([c (lambda (x) (lambda () x))])
         (if (read)
             (c 10)
             (c 11)))))
   #'x)
  (Number)))

(check-equal?
 (abstract-eval
   '(if (read)
        (lambda (x) x)
        (lambda (y) y)))
 T)

(check-equal?
  (abstract-eval
    '(let ([f (lambda (x) (* 2 x))])
       (map f '(1 2 3))))
 (datum->type '(2 4 6))
 "this is expected to fail for now")
