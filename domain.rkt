#lang racket

(provide T T?
         ℕ ℕ?
         ⊥ ⊥?
         in-domain?
         (struct-out closure)
         closure-label
         <=?
         lub)

(require "environment.rkt"
         "free-vars.rkt")

(define T (unquoted-printing-string "T"))
(define ℕ (unquoted-printing-string "ℕ"))

(define (T? v) (eq? v T))
(define (ℕ? v) (eq? v ℕ))

(struct ⊥ (message)
  #:constructor-name make-⊥
  #:transparent
  #:omit-define-syntaxes)

(define/match (⊥ arg . rest-args)
  [((? string? message-str) '())
   (make-⊥ message-str)]
  [((? symbol? who-sym) (list* (? string? format-str) rest-args))
   (make-⊥ (format "~a: ~a" who-sym (apply format format-str rest-args)))])

;; TODO: Add false
;; TODO: Generic procedures/arrows

(struct closure (source-syntax environment)
  #:constructor-name make-closure)

(define closure-label closure-source-syntax)

(define (in-domain? v)
  (or (T? v) (⊥? v) (ℕ? v) (natural? v) (false? v) (procedure? v) (closure? v)))

(define (<=? d1 d2)
  (define ht (make-hashalw))
  (let loop ([d1 d1] [d2 d2])
    (or (hash-ref ht (cons d1 d2) #f)
        (begin
          (hash-set! ht (cons d1 d2) #t)
          (<=?/recur d1 d2 loop)))))

(define (<=?/recur d1 d2 recur-proc)
  (let loop ([d1 d1] [d2 d2])
    (cond
     [(eqv? d1 d2) #t]
     [(T? d2) #t]
     [(⊥? d1) #t]
     [(and (natural? d1) (ℕ? d2)) #t]
     [(and (closure? d1) (closure? d2))
      (and (eq? (closure-label d1) (closure-label d2))
           (closure-environment<=?/recur d1 d2 recur-proc))]
     [else #f])))

(define (closure-environment<=?/recur c1 c2 recur-proc)
  (define ρ1 (closure-environment c1))
  (define ρ2 (closure-environment c2))
  (for/and ([id (in-list (free-vars (closure-source-syntax c1)))])
    (let/ec break
      (define v1 (ρ-ref ρ1 id))
      (define v2 (ρ-ref ρ2 id (lambda () (break #f))))
      (<=?/recur (force v1) (force v2) recur-proc))))

(define (lub d d′)
  (define ht
    (make-custom-hash
      (lambda (p p′)
        (or (and (eqv? (car p) (car p′))
                 (eqv? (cdr p) (cdr p′)))
            (and (eqv? (car p) (cdr p′))
                 (eqv? (cdr p) (car p′)))))))
  (let loop ([d d] [d′ d′])
    (or (dict-ref ht (cons d  d′) #f)
        (let ()
          (define d″ (delay (lub/recur d d′ loop)))
          (dict-set! ht (cons d d′) d″)
          (force d″)))))

(define (lub/recur d d′ recur-proc)
  (cond
   [(eqv? d d′) d]
   [(or  (T? d) (T? d′)) T]
   [(and (⊥? d) (⊥? d′))
    (⊥ "something went wrong")]
   [(⊥? d ) d′]
   [(⊥? d′) d ]
   [(and (<=? d ℕ) (<=? d′ ℕ)) ℕ]
   [(and (closure? d) (closure? d′) (eq? (closure-label d) (closure-label d′)))
    (make-closure (closure-source-syntax d) (closure-environment-lub/recur d d′ recur-proc))]
   [else T]))

(define (closure-environment-lub/recur c c′ recur-proc)
    (define ρ  (closure-environment c ))
    (define ρ′ (closure-environment c′))
    (for/fold ([ρ″ (make-ρ)])
              ([id (in-list (free-vars (closure-source-syntax c)))])
      (define d  (force (ρ-ref ρ  id)))
      (define d′ (force (ρ-ref ρ′ id (⊥ "unbound identifier"))))
      (define d″ (recur-proc d d′))
      (ρ-set ρ″ id d″)))