#lang racket

(provide bind-time-analysis
         syntax/bind-time
	 syntax-bind-time
	 syntax-erase-bind-time)

(define (bind-time-analysis stx)
  (local-expand stx 'expression '()))

(define (syntax/bind-time stx bt)
  (syntax-property stx 'bind-time bt))

(define (syntax-bind-time stx)
  (define bt (syntax-property stx 'bind-time))
  (unless bt (error "expected a \"bind-time\" syntax property"))
  bt)

(define (syntax-erase-bind-time stx)
  (syntax-property-remove stx 'bind-time))
