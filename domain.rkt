#lang racket

(provide make-domain
         T T?
         ⊥ ⊥?
         <=? lub)

(require racket/generic)

; (let ([in-domain? (domain-contract domain)]
;       [T? (domain-T? domain)]
;       [⊥? (domain-⊥? domain)])
;   (T domain) -> T?
;   (⊥ domain) -> ⊥?
;   (from-datum domain v) -> in-domain?
;   (<=? domain v0 v1) -> boolean?
;   (lub domain v0 v1) -> in-domain?

; (define-syntax-rule (compose-domains d0 d ...)
;   #'(make-abstract-domain
;       (lambda (v)
;         (list (from-datum d0 v) (from-datum d v) ...))
;       (list/c (domain-contract d0) (domain-contract d) ...)
;       (lambda (x y)
;         (for/and ([d (in-list (list d0 d ...))]
;                   [x (in-list x)]
;                   [y (in-list y)])
;           (<=? d x y)))))

; (struct partial-order (member? <=))
; (struct join-semilattice (member? <= lub T))
; (struct pointed-complete-partial-order (member? <= lub T ⊥))
; (struct complete-lattice (member? <= lub glb T ⊥))

(struct domain (contract <=? lub T)
        #:constructor-name make-domain)

; (domain-T domain)
; ⊥
; (
; (domain-<=? domain v0 v)
; (domain-lub domain v0 v)

; (define (lift-domain d)
;   (or/c (domain-contract d) ⊥))

(define private-⊥ (string->uninterned-symbol "⊥"))

(define (T domain)
  (domain-T domain))

(define (T? domain v)
  (equal? (T domain) v))

(define (⊥ domain)
  private-⊥)

(define (⊥? domain v)
  (equal? (⊥ domain) v))

(define (<=? domain v1 v2)
  (cond
   [(eq? v1 v2) #t]
   [(⊥? domain v1) #t]
   [(T? domain v2) #t]
   [else ((domain-<=? domain) v1 v2)]))

(define (lub domain v1 v2)
  (cond
   [(eq? v1 v2) v1]
   [(or (T? domain v1) (T? domain v2)) (T domain)]
   [(⊥? domain v1) v2]
   [(⊥? domain v2) v1]
   [else ((domain-lub domain) v1 v2)]))
