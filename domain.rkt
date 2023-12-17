#lang racket

(provide T
         T?
         ⊥ (rename-out [⊥ B])
         ⊥? (rename-out [⊥? B?])
         ℕ (rename-out [ℕ N])
         ℕ? (rename-out [ℕ? N?])
         in-domain?
         (struct-out closure)
         <=?
         lub)

(require syntax/free-vars
         "environment.rkt"
         "store.rkt")

(define T 'T)
; HACK: The definition below works the same when called like `(⊥ message)` or when
; used directly like `⊥`. Temporary stub while error messages are not implemented.
(define (⊥ _) ⊥)
(define ℕ 'ℕ)

(define (T? v) (equal? v T))
(define (⊥? v) (equal? v ⊥))
(define (ℕ? v) (equal? v ℕ))

;; TODO: Add false
;; TODO: Generic procedures/arrows

(struct closure (source-syntax environment)
  #:constructor-name make-closure)

(define closure-label closure-source-syntax)

(define (in-domain? v)
  (or (T? v) (⊥? v) (ℕ? v) (natural? v) (false? v) (procedure? v) (closure? v)))

(define (<=? d1 d2)
  (cond
   [(equal? d1 d2) #t]
   [(T? d2) #t]
   [(⊥? d1) #t]
   [(and (natural? d1) (ℕ? d2)) #t]
   [(and (closure? d1) (closure? d2)) (closure<=? d1 d2)]
   [else #f]))

(define (closure<=? c1 c2)
  (or (eq? c1 c2)
      (and (eq? (closure-label c1)
                (closure-label c2))
           (closure-environment<=? c1 c2))))

(define (closure-environment<=? c1 c2)
  (define env1 (closure-environment c1))
  (define env2 (closure-environment c2))
  (for/and ([id (in-list (free-vars (closure-source-syntax c1)))])
    (<=? (ρ-ref env1 id ⊥) (ρ-ref env2 id ⊥))))

(define (lub d1 d2)
  (cond
   [(equal? d1 d2) d1]
   [(or (T? d1) (T? d2)) T]
   [(⊥? d1) d2]
   [(⊥? d2) d1]
   [(and (<=? d1 ℕ) (<=? d2 ℕ)) ℕ]
   [(and (closure? d1) (closure? d2)) (closure-lub d1 d2)]
   [else T]))

;; FIXME: Will enter an infinite loop when both arguments are recursive procedures which are not `eq?`
;; but have the same label (I think)
(define (closure-lub c1 c2)
  (cond
   [(eq? (closure-label c1) (closure-label c2))
    (make-closure (closure-source-syntax c1)
                  (closure-environment-lub c1 c2))]
   [else T]))

(define (closure-environment-lub c1 c2)
    (define ρ1 (closure-environment c1))
    (define ρ2 (closure-environment c2))
    (for/fold ([ρ- (make-ρ)])
              ([id (in-list (free-vars (closure-source-syntax c1)))])
      (define d1 (σ-ref (ρ-ref ρ1 id) (lambda () ⊥)))
      (define d2 (σ-ref (ρ-ref ρ2 id) (lambda () ⊥)))
      (define location (gensym (syntax-e id)))
      (σ-set! location (lub d1 d2))
      (ρ-set ρ- id location)))