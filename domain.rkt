#lang racket

(provide (all-defined-out))

(provide T T?
         ⊥ ⊥?
         ℕ ℕ?
         in-domain?
         make-introspectable-procedure
         <=?
         lub)

(require racket/lazy-require
         syntax/free-vars
         "environment.rkt")

(define T 'T)
(define ⊥ '⊥)
(define ℕ 'ℕ)

(define (T? v) (equal? v T))
(define (⊥? v) (equal? v ⊥))
(define (ℕ? v) (equal? v ℕ))

;; TODO: Generic procedures/arrows

(struct introspectable-procedure (source closure proc)
  #:extra-constructor-name make-introspectable-procedure
  #:property prop:procedure (struct-field-index proc))

(define (procedure-source proc)
  (cond
   [(introspectable-procedure? proc) (introspectable-procedure-source proc)]
   [else #f]))

(define (procedure-closure proc)
  (cond
   [(introspectable-procedure? proc) (introspectable-procedure-closure proc)]
   [else #f]))

(define (in-domain? v)
  (or (T? v) (⊥? v) (ℕ? v) (natural? v) (procedure? v)))

(define (<=? d1 d2)
  (cond
   [(equal? d1 d2) #t]
   [(T? d2) #t]
   [(⊥? d1) #t]
   [(and (natural? d1) (ℕ? d2)) #t]
   ;; TODO: (-> d1 r1) . <=? . (-> d2 r2)
   ;; TODO: procedure-closure . <=? . (-> d r)
   [(and (procedure? d1) (procedure? d2)) (procedure<=? d1 d2)]
   [else #f]))

(define (procedure<=? proc1 proc2)
  (or (eq? proc1 proc2)
      (and (introspectable-procedure? proc1)
           (introspectable-procedure? proc2)
           (eq? (procedure-source proc1)
                (procedure-source proc2))
           (procedure-closure-contents<=? proc1 proc2))))

;; TODO: Double check this works for recursive procedures
(define (procedure-closure-contents<=? proc1 proc2)
  (define ρ1 (procedure-closure proc1))
  (define ρ2 (procedure-closure proc2))
  (for/and ([id (in-list (free-vars (procedure-source proc1)))])
    (<=? (dict-ref ρ1 id ⊥) (dict-ref ρ2 id ⊥))))

(define (lub d1 d2)
  (cond
   [(equal? d1 d2) d1]
   [(or (T? d1) (T? d2)) T]
   [(⊥? d1) d2]
   [(⊥? d2) d1]
   [(and (<=? d1 ℕ) (<=? d2 ℕ)) ℕ]
   [(and (procedure? d1) (procedure? d2)) (procedure-lub d1 d2)]
   [else T]))

;; TODO: Double check this works for recursive procedures
(define (procedure-lub proc1 proc2)
  (cond
   [(eq? proc1 proc2) proc1]
   [(and (introspectable-procedure? proc1)
         (introspectable-procedure? proc2)
         (eq? (procedure-source proc1)
              (procedure-source proc2)))
    (make-introspectable-procedure (procedure-source proc1)
                                   (procedure-closure-contents-lub proc1 proc2)
                                   (lambda _ (error "not implemented")))]
   [else T]))

(define (procedure-closure-contents-lub proc1 proc2)
    (define ρ1 (procedure-closure proc1))
    (define ρ2 (procedure-closure proc2))
    (for/fold ([ρ- empty-environment])
              ([id (in-list (free-vars (procedure-source proc1)))])
      (define d1 (dict-ref ρ1 id ⊥))
      (define d2 (dict-ref ρ2 id ⊥))
      (dict-set ρ- id (lub d1 d2))))