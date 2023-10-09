#lang racket

(require rackunit
         "common.rkt"
         "abstract-eval.rkt"
         "environment.rkt"
         "types.rkt")

(define (infer-type expr)
  (parameterize
    ([property-from-syntax
        (lambda (stx) (datum->type (syntax->datum stx)))]
     [property-stronger?
        (lambda (v1 v2 recur-proc) (type<=? v1 v2))]
     [property-combine
        (lambda (v1 v2 recur-proc) (type-lub v1 v2))])
    (abstract-eval expr (make-base-namespace))))


(check-equal? (infer-type 10) (datum->type 10))
(check-equal? (infer-type '(let ([x 10]) x)) (datum->type 10))
(check-equal?
  (infer-type '(let ([x 'not-ok]) (let ([x 'ok]) x)))
  (datum->type 'ok))
(check-equal? (infer-type '(+ 10 11)) (datum->type 21))
(check-not-exn
 (lambda ()
   (infer-type '(lambda (x) x))))
(check-equal? (infer-type '((lambda (x) x) 10)) (datum->type 10))
(check-equal? (infer-type '((lambda (x) (+ x 11)) 10)) (datum->type 21))
(check-equal? (infer-type '(if #t 'ok 'not-ok)) (datum->type 'ok))
(check-equal? (infer-type '(if #f 'not-ok 'ok)) (datum->type 'ok))
(check-equal? (infer-type '(+ 1 (* 3 (- 10 7)))) (datum->type 10))
(check-equal? (infer-type '(let ([f +]) (f 10 11))) (datum->type 21))

(check-equal? (infer-type '(let ([x 10] [y 11]) (+ x y))) (datum->type 21))

(check-exn
 exn:fail?
 (lambda ()
   (infer-type '(let ([x 10] [x 11]) x))))

; FIXME: Evaluates to T because we don't check primitive contracts
(check-equal?
 (infer-type
  '(letrec ([fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))])
     (fac 5)))
 (Exact-Nonnegative-Integer)
 "this is expected to fail for now")

; FIXME: Fails because closures are no longer `#:transparent` (regression)
(check-equal?
 (infer-type
  '(letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))]
            [odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))])
     (even? 101)))
 T)

(check-equal? (infer-type '(read)) T)
(check-equal? (infer-type '(+ (read) 10)) T)

(check-equal? (infer-type '(if #t 'ok (read))) (datum->type 'ok))
(check-equal? (infer-type '(if #f (read) 'ok)) (datum->type 'ok))
(check-equal? (infer-type '(if (read) 10 10)) (datum->type 10))
(check-true (type<=? (infer-type '(if (read) 10 11)) (Number)))

(check-equal? (infer-type '(error)) ⊥)
(check-equal? (infer-type '(let ([x (error)]) 'unused)) ⊥)
(check-equal? (infer-type '(letrec ([x (error)]) 'unused)) ⊥)
(check-equal? (infer-type '((error) 'unused)) ⊥)
(check-equal? (infer-type '(+ (error) 10)) ⊥)
(check-equal? (infer-type '(+ 10 (error))) ⊥)

(check-equal? (infer-type 'x) ⊥)
(check-equal? (infer-type '(let ([x 10]) y)) ⊥)
(check-equal? (infer-type '(if (read) 10 x)) (datum->type 10))

(check-not-equal? (infer-type '(if (read) + +)) T)

#;(check-true
 (closure?
   (infer-type
     '(let ([f (lambda (x) x)])
        (if (read) f f)))))

#;(check-equal?
 (environment-ref
  (closure-environment
   (infer-type
     '(let ([c (lambda (x) (lambda () x))])
        (if (read)
            (c 10)
            (c 10)))))
  #'x)
 (datum->type 10))

#;(check-true
 (type<=?
  (environment-ref
   (closure-environment
    (infer-type
      '(let ([c (lambda (x) (lambda () x))])
         (if (read)
             (c 10)
             (c 11)))))
   #'x)
  (Number)))

(check-equal?
 (infer-type
   '(if (read)
        (lambda (x) x)
        (lambda (y) y)))
 T)

(check-equal?
  (infer-type
    '(let ([f (lambda (x) (* 2 x))])
       (map f '(1 2 3))))
 (datum->type '(2 4 6))
 "this is expected to fail for now")
