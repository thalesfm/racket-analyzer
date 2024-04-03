#lang racket

(require rackunit
         "../abstract-eval.rkt"
         "../domain.rkt")

;; -----------------------------------------------------------------------------
;; Checks for recursive procedures

(displayln "test1")
(check-equal? 120
 (abstract-eval
  '(let ()
     (define (f n)
       (if (= n 0)
           1
           (* n (f (- n 1)))))
     (f 5))))

(displayln "test2")
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

(displayln "test3")
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

(displayln "test4")
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