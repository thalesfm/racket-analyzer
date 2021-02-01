#lang racket

; (provide top bot top? bot? sup)
(provide (all-defined-out))

;; TODO: Define a structure type for `top' and `bot' w/ pretty printing
;; TODO: Define a way to mark bind-times as potentially non-terminating

(define top 'top)

(define bot 'bot)

(define (top? v)
  (eq? v top))

(define (bot? v)
  (eq? v bot))

(define (bind-time-singleton? v)
  (or (boolean? v)
      (number? v)
      (string? v)
      (char? v)
      (symbol? v)
      (and (pair? v)
    	   (bind-time-singleton? (car v))
    	   (bind-time-singleton? (cdr v)))))

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

(define/contract bind-time<=?
  (-> bind-time? bind-time? boolean?)
  (match-lambda**
	[(_ (? top?)) #t]
	[((? bot?) _) #t]
	[((? boolean?) bt) #:when (eq? bt boolean?) #t]
	[((? number?) bt) #:when (eq? bt number?) #t]
	[((? string?) bt) #:when (eq? bt string?) #t]
	[((? char?) bt) #:when (eq? bt char?) #t]
	[((? symbol?) bt) #:when (eq? bt symbol?) #t]
	[(bt1 bt2) #:when (eqv? bt1 bt2) #t]
	[((? pair? bt1) (? pair? bt2))
	 #:when (and (bind-time<=? (car bt1) (car bt2))
				 (bind-time<=? (cdr bt1) (cdr bt2)))
	 #t]
	[(_ _) #f]))

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
