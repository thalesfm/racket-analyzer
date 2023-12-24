#lang racket

(require rackunit
         "../abstract-eval.rkt"
         "../domain.rkt")

;; -----------------------------------------------------------------------------
;; Checks for datum literals

(check-equal? (abstract-eval '3) 3)

;; -----------------------------------------------------------------------------
;; Checks for primitive procedures

(test-begin
  (check-equal? + (abstract-eval '+))
  (check-equal? 7 (abstract-eval '(+ 3 4)))
  (check-equal? ℕ (abstract-eval '(+ (read) 4)))) ; FIXME: failing

;; -----------------------------------------------------------------------------
;; Checks for `let` expressions

(check-equal? 5 (abstract-eval '(let ([x 5]) x)))
(check-equal? 7 (abstract-eval '(let ([x 3] [y 4]) (+ x y))))
(check-equal? 6 (abstract-eval '(let ([x 5]) (let ([x 6]) x))))

;; -----------------------------------------------------------------------------
;; Checks for `letrec` expressions

(check-equal? 3  (abstract-eval '(letrec ([x 3]) x)))
(check-equal? 7  (abstract-eval '(letrec ([x 3] [y 4]) (+ x y))))
(check-equal? 6  (abstract-eval '(letrec ([x 5]) (letrec ([x 6]) x))))
(check-pred   ⊥? (abstract-eval '(letrec ([x x]) x)))
(check-equal? 5  (abstract-eval '(letrec ([x 5] [y x]) y)))
(check-pred   ⊥? (abstract-eval '(letrec ([x y] [y 5]) x)))
(check-equal? 5  (abstract-eval '(letrec ([x (lambda () y)] [y 5]) (x))))

;; -----------------------------------------------------------------------------
;; Checks for conditional expressions

(check-equal? 3 (abstract-eval '(if  2 3 4)))
(check-equal? 3 (abstract-eval '(if  0 3 4)))
(check-equal? 4 (abstract-eval '(if #f 3 4)))
(check-equal? ℕ (abstract-eval '(if (read) 3 4)))

(check-pred   ⊥? (abstract-eval '(if (error) 3 4)))
(check-equal? 3  (abstract-eval '(if 10 3 (error))))
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

(check-equal? 3
 (abstract-eval
  '(let* ([c (lambda (k) (lambda (x) k))]
          [f (if (read) (c 3) (c 3))])
     (f 7))))

(check-equal? ℕ
 (abstract-eval
  '(let* ([c (lambda (k) (lambda (x) k))]
          [f (if (read) (c 3) (c 4))])
     (f 7))))

;; -----------------------------------------------------------------------------
;; Checks for recursive procedures

(check-equal? 120
 (abstract-eval
  '(let ()
     (define (f n)
       (if (= n 0)
           1
           (* n (f (- n 1)))))
     (f 5))))

(check-equal? 120
 (abstract-eval
  '(let ()
     (define (Y f)
       (let ([x (λ (x) (λ (y) (f (x x) y)))])
         (x x)))
     (define (f r n)
       (if (= n 0)
           1
           (* n (r (- n 1)))))
     ((Y f) 5))))

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