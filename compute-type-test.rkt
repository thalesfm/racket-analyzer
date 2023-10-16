#lang racket

(require rackunit
         "abstract-eval.rkt"
         "common.rkt"
         "types.rkt")

(define-simple-check (typecheck op expr t)
  (op (compute-type expr) t))

(define-simple-check (typecheck-pred pred expr)
  (pred (compute-type expr)))

(define-syntax-rule (typecheck-<=? expr t . maybe-message)
  (typecheck type<=? expr t . maybe-message))

(define-syntax-rule (typecheck-=? expr t . maybe-message)
  (typecheck type=? expr t . maybe-message))

(typecheck-<=? 10 Integer)
(typecheck-<=? '(let ([x 10]) x) Integer)
(typecheck-<=? '(let ([x 'a]) (let ([x 10]) x)) Integer)
(typecheck-<=? '(let ([+ 10]) +) Integer)
(typecheck-<=? '(+ 10 11) Integer)
(typecheck-<=? '((lambda (x) x) 10) Integer)
(typecheck-<=? '((lambda (x) (+ x 11)) 10) Integer)
(typecheck-<=? '(if #t 'a 'b) Symbol)
(typecheck-<=? '(if #f 'a 'b) Symbol)
(typecheck-<=? '(+ 1 (* 3 (- 10 7))) Integer)
(typecheck-<=? '(let ([f +]) (f 10 11)) Integer)
(typecheck-<=? '(let ([x 10] [y 11]) (+ x y)) Integer)

(typecheck-<=?
 '(letrec ([fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))])
    (fac 5))
 Exact-Nonnegative-Integer)

(typecheck-<=?
 '(letrec ([even? (lambda (n) (if (= n 0) #t (odd?  (- n 1))))]
           [odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))])
    (even? 101))
 Boolean)

(typecheck-=? '(read) Any)
(typecheck-pred ⊥? '(+ (read) 10))

(typecheck-=? '(if #t 'ok (read)) Symbol)
(typecheck-=? '(if #f (read) 'ok) Symbol)
(typecheck-=? '(if (read) 10 10) Exact-Nonnegative-Integer)
(typecheck-=? '(if (read) 10 3+4i) Number)

(typecheck-pred ⊥? '(error))
(typecheck-pred ⊥? '(let ([x (error)]) 'unused))
(typecheck-pred ⊥? '(letrec ([x (error)]) 'unused))
(typecheck-pred ⊥? '((error) 'unused))
(typecheck-pred ⊥? '(+ (error) 10))
(typecheck-pred ⊥? '(+ 10 (error)))

(typecheck-pred ⊥? 'x)
(typecheck-pred ⊥? '(let ([x 10]) y))
(typecheck-<=? '(if (read) 10 x) (type-of 10))

(typecheck-pred (negate T?) '(if (read) + +))
(typecheck-=? '(let ([f (lambda () x)]) 10) (type-of 10))

(typecheck-pred
 closure?
 '(let ([f (lambda (x) x)])
    (if (read) f f)))

; FIXME: Failing check
(typecheck-=?
 '(let* ([f (lambda (x) (lambda () x))]
         [g (if (read) (f 10) (f 10))])
    (g))
 (type-of 10))

(typecheck-=?
 '(if (read)
      (lambda (x) x)
      (lambda (y) y))
 T)

; FIXME: Failing check
(typecheck
 type=?
 '(let ([f (lambda (x) (* 2 x))])
    (map f '(1 2 3)))
 (type-of '(2 4 6))
 "this check is expected to fail for now")

(typecheck-=?
 '(letrec ([f (lambda (x) (if x f x))])
    (let ([g (f 10)])
      (g #f)))
  (type-of #f))

; FIXME: Failing check (#9)
(typecheck-=?
 '(letrec ([f (lambda (x) (if x f x))]
           [g (f 10)])
    (g #f))
  (type-of #f))
