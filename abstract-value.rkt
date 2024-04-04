#lang racket

(provide T T?
         ⊥ ⊥?
         (struct-out closure)
         closure-label
         <=?
         lub)

(require (prefix-in base: "abstract-value-base.rkt")
         "environment.rkt"
         "free-vars.rkt")

(define (T . _) T)
(define (⊥ . _) ⊥)

(define (T? v) (eq? v T))
(define (⊥? v) (eq? v ⊥))

(define (lift d)
  (cond
   [(eq? d base:T) T]
   [(eq? d base:⊥) ⊥]
   [else d]))

(define (proj d [default #f])
  (cond
   [(eq? d T) base:T]
   [(eq? d ⊥) base:⊥]
   [(base:abstract-value? d) d]
   [else default]))

(struct closure (source-syntax environment)
  #:constructor-name make-closure)

(define closure-label closure-source-syntax)

(define (<=? d d′)
  (define ht (make-hashalw))
  (let loop ([d d] [d′ d′])
    (or (hash-ref ht (cons d d′) #f)
        (begin
          (hash-set! ht (cons d d′) #t)
          (<=?/recur d d′ loop)))))

(define (<=?/recur d d′ recur-proc)
  (cond
   [(eqv? d  d′) #t]
   [(eq?  d  ⊥ ) #t]
   [(eq?  d′ T ) #t]
   [(and (base:abstract-value? (proj d  T))
         (base:abstract-value? (proj d′ T)))
    (base:<=? (proj d) (proj d′))]
   [else
    (and (closure? d )
         (closure? d′))
         (eq? (closure-label d) (closure-label d′))
         (closure-environment<=?/recur d d′ recur-proc)]))

(define (closure-environment<=?/recur c1 c2 recur-proc)
  (define ρ1 (closure-environment c1))
  (define ρ2 (closure-environment c2))
  (for/and ([id (in-list (free-vars (closure-source-syntax c1)))])
    (let/ec break
      (define v1 (environment-ref ρ1 id))
      (define v2 (environment-ref ρ2 id (lambda () (break #f))))
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
   [(eqv? d  d′) d ]
   [(eq?  d  ⊥ ) d′]
   [(eq?  d′ ⊥ ) d ]
   [(eq?  d  T ) T ]
   [(eq?  d′ T ) T ]
   [(and (base:abstract-value? (proj d  T))
         (base:abstract-value? (proj d′ T)))
    (lift (base:lub (proj d) (proj d′)))]
   [(and (closure? d )
         (closure? d′)
         (eq? (closure-label d) (closure-label d′)))
    (closure-lub/recur d d′ recur-proc)]
   [else T]))

(define (closure-lub/recur c c′ recur-proc)
  (define ρ  (closure-environment c ))
  (define ρ′ (closure-environment c′))
  (define ρ″
    (for/fold ([ρ″ (make-environment)])
              ([id (in-list (free-vars (closure-source-syntax c)))])
      (define d  (force (environment-ref ρ  id)))
      (define d′ (force (environment-ref ρ′ id ⊥)))
      (define d″ (recur-proc d d′))
      (environment-set ρ″ id d″)))
  (make-closure (closure-source-syntax c) ρ″))