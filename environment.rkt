#lang racket

(provide
 environment?
 (contract-out
  [make-empty-environment (-> environment?)]
  [make-base-environment (-> environment?)]
  [bind (-> environment? identifier? any/c environment?)]
  [bind* (-> environment? stx-list? list? environment?)]
  [bind! (-> environment? identifier? any/c void?)]
  [fresh (-> environment? identifier? environment?)]
  [fresh* (-> environment? stx-list? environment?)]
  [lookup (-> environment? identifier? any/c)]
))

(require syntax/stx
         "lift.rkt"
         "types.rkt")

;; TODO: Make module implementation independent on the domain definition
;; TODO: Move `make-base-environment` somewhere more appropriate

(define (massoc v lst [is-equal? equal?])
  (findf (λ (p) (is-equal? (mcar p) v)) lst))

(struct environment (assoc-list)
        #:mutable
        #:constructor-name make-environment)

(define (make-entry id v)
  (mcons (syntax->datum id) v))

(define (make-empty-environment)
  (make-environment null))

(define (make-base-environment)
  (let* ([Γ (make-empty-environment)]
         [Γ (bind Γ #'+ (lift +))]
         [Γ (bind Γ #'+ (lift +))]
         [Γ (bind Γ #'- (lift -))]
         [Γ (bind Γ #'* (lift *))]
         [Γ (bind Γ #'/ (lift /))]
         [Γ (bind Γ #'= (lift =))]
         [Γ (bind Γ #'read (λ () Any))]
         [Γ (bind Γ #'error (λ () Nothing))])
    Γ))

(define (bind env id v)
  (define lst (environment-assoc-list env))
  (define entry (make-entry id v))
  (make-environment (cons entry lst)))

(define (bind! env id v)
  (define lst (environment-assoc-list env))
  (define entry (massoc (syntax->datum id) lst))
  (if entry
      (set-mcdr! entry v)
      (set-environment-assoc-list! (cons (make-entry id v) lst))))

(define (bind* env id-list v-list)
  (for/fold ([env env])
            ([id (in-syntax id-list)]
             [v (in-list v-list)])
    (bind env id v)))

(define (fresh env id)
  (bind env id Nothing))

(define (fresh* env id-list)
  (for/fold ([env env])
            ([id (in-syntax id-list)])
    (fresh env id)))

(define (lookup env id)
  (define lst (environment-assoc-list env))
  (define entry (massoc (syntax->datum id) lst))
  (if entry (mcdr entry) Nothing))
