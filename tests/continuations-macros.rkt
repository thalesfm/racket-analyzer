#lang racket

(define-syntax (begin/break stx)
  (define (go lst k)
    (cond
	  [(eq? lst '()) '()]
	  [(eq? (car lst) 'break) (k (go (cdr lst) k))]
	  [else (cons (car lst) (go (cdr lst) k))]))
  (datum->syntax stx (cons 'begin (call/cc (lambda (k) (go (syntax->datum stx) k))))))

(begin/break
  (displayln 1)
  break
  (displayln 2)
  break
  (displayln 3))
