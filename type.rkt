#lang racket

(provide top bot top? bot? sup)

(define-values (top bot)
  (let ()
    (struct type ())
    (values (type) (type))))

(define (top? t)
  (eq? t top))

(define (bot? t)
  (eq? t bot))

(define atom?
  (disjoin boolean?
           number?
           char?
           string?
           symbol?
           vector?
           procedure?))

(define (singleton? v)
  (or (atom? v)
	  (and (pair? v)
		   (atom? (car v))
		   (atom? (cdr v)))))

(define (total t)
  (error "undefined"))

(define (total? t)
  (error "undefined"))

(define (seq t1 t2)
  (error "undefined"))

(define (sup t1 t2)
  (cond [(bot? t1) t2]
        [(bot? t2) t1]
        [(eqv? t1 t2) t1]
        [(and (pair? t1) (pair? t2))
         (cons (sup (car t1) (car t2))
               (sup (cdr t1) (cdr t2)))]
        [else top]))
