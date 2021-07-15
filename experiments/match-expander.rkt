#lang racket

(provide (all-defined-out))

(define foo-internal (gensym 'foo))

(define-match-expander foo
  (syntax-rules ()
    [(foo) (? foo?)])
  (make-rename-transformer #'foo-internal))

(define (foo? v)
  (eq? v foo))

(define (is-foo? v)
  (match v
    [(foo) #t]
    [_ #f]))
