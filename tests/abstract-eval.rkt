#lang racket

(require rackunit
         "../abstract-eval.rkt"
         "../abstract-value.rkt")

(current-namespace (make-base-namespace))

;; -----------------------------------------------------------------------------
;; Checks for datum literals

(check-equal? (abstract-eval '3) 3)

;; -----------------------------------------------------------------------------
;; Checks for primitive procedures

(check-pred procedure? (abstract-eval '+))
(check-equal? (abstract-eval '(+ 3 4)) 7)
(check-equal? (abstract-eval '(+ (read) 4)) T)
(check-pred ⊥? (abstract-eval '(+ 1 2 3)))

;; -----------------------------------------------------------------------------
;; Checks for `let` expressions

(check-equal? (abstract-eval '(let ([x 5]) x)) 5)
(check-equal? (abstract-eval '(let ([x 3] [y 4]) (+ x y))) 7)
(check-equal? (abstract-eval '(let ([x 5]) (let ([x 6]) x))) 6)

;; -----------------------------------------------------------------------------
;; Checks for `letrec` expressions

(check-equal? (abstract-eval '(letrec ([x 3]) x)) 3)
(check-equal? (abstract-eval '(letrec ([x 3] [y 4]) (+ x y))) 7)
(check-equal? (abstract-eval '(letrec ([x 5]) (letrec ([x 6]) x))) 6)
;; Regression
;; (check-pred   ⊥? (abstract-eval '(letrec ([x x]) x)))
(check-equal? 5  (abstract-eval '(letrec ([x 5] [y x]) y)))
;; Regression
;; (check-pred   ⊥? (abstract-eval '(letrec ([x y] [y 5]) x)))
(check-equal? 5  (abstract-eval '(letrec ([x (lambda () y)] [y 5]) (x))))

;; -----------------------------------------------------------------------------
;; Checks for conditional expressions

(check-equal? 3 (abstract-eval '(if #t 3 4)))
(check-equal? 4 (abstract-eval '(if #f 3 4)))
(check-equal? T (abstract-eval '(if (read) 3 4)))

(check-pred   ⊥? (abstract-eval '(if (error) 3 4)))
(check-equal? 3  (abstract-eval '(if #t 3 (error))))
(check-equal? 4  (abstract-eval '(if #f (error) 4)))
(check-equal? 3  (abstract-eval '(if (read) 3 (error))))
(check-equal? 4  (abstract-eval '(if (read) (error) 4)))
(check-pred   ⊥? (abstract-eval '(if (read) (error) (error))))

;; -----------------------------------------------------------------------------
;; Checks for lambda expressions and procedure application

(check-equal? 6  (abstract-eval '((lambda (x) x) 6)))
(check-equal? 7  (abstract-eval '((lambda (x y) (+ x y)) 3 4)))
(check-pred   ⊥? (abstract-eval '(2 3 4)))
(check-pred   ⊥? (abstract-eval '((error) 3 4)))
(check-pred   ⊥? (abstract-eval '(+ 3 (error))))

(check-equal?
 (abstract-eval
  '(let* ([c (lambda (k) (lambda (x) k))]
          [f (if (read) (c 3) (c 3))])
     (f 7)))
 3)

(check-equal? T
 (abstract-eval
  '(let* ([c (lambda (k) (lambda (x) k))]
          [f (if (read) (c 3) (c 4))])
     (f 7))))