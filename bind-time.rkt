#lang racket

(provide (all-defined-out))

(struct type (name) #:transparent)

(define top (type 'top))

(define bot (type 'bot))

(define (top? v)
  (eq? v top))

(define (bot? v)
  (eq? v bot))

(define (singleton? v)
  (or (boolean? v)
      (number? v)
      (string? v)
      (char? v)
      (symbol? v)
      (and (pair? v)
           (singleton? (car v))
           (singleton? (cdr v)))))

(define (bind-time? v)
  (or (top? v)
      (bot? v)
      (boolean? v)
      (number? v)
      (string? v)
      (char? v)
      (symbol? v)
      (eq? v boolean?)
      (eq? v number?)
      (eq? v string?)
      (eq? v char?)
      (eq? v symbol?)
      (and (pair? v)
           (bind-time? (car v))
           (bind-time? (cdr v)))))

(define/match (bind-time<=? bt1 bt2)
  [(_ (? top?)) #t]
  [((? bot?) _) #t]
  [((? boolean?) bt2) #:when (eq? bt2 boolean?) #t]
  [((? number?) bt2) #:when (eq? bt2 number?) #t]
  [((? string?) bt2) #:when (eq? bt2 string?) #t]
  [((? char?) bt2) #:when (eq? bt2 char?) #t]
  [((? symbol?) bt2) #:when (eq? bt2 symbol?) #t]
  [(bt1 bt2) #:when (eqv? bt1 bt2) #t]
  [((? pair?) (? pair?))
   #:when (and (bind-time<=? (car bt1) (car bt2))
               (bind-time<=? (cdr bt1) (cdr bt2)))
   #t]
  [(_ _) #f])

(define (bind-time-comparable? bt1 bt2)
  (or (bind-time<=? bt1 bt2) (bind-time<=? bt2 bt1)))

(define (bind-time-seq bt1 bt2)
  (error "undefined"))

(define (bind-time-lub t1 t2)
  (cond [(bot? t1) t2]
        [(bot? t2) t1]
        [(eqv? t1 t2) t1]
        [(and (pair? t1) (pair? t2))
         (cons (bind-time-lub (car t1) (car t2))
               (bind-time-lub (cdr t1) (cdr t2)))]
        [else top]))
