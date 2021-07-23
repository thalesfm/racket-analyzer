#lang racket

(provide Top Top?
         Empty Empty? ;; Prolly shouldn't export these
         Bot Bot?
         Number Number?
         Real Real?
         Integer Integer?
         Boolean Boolean?
         String String?
         datum->type
         type->datum
         maybe-bot?
         static?
         project
         type<=?
         type-comparable?
         lub
         seq)

(require racket/syntax)

(struct type-info (ctor ctor-args maybe-bot?))

(define (maybe-bot? τ)
  (type-info-maybe-bot? τ))

(define (project τ)
  (struct-copy type-info τ [maybe-bot? #f]))

(define (seq . τs)
  (struct-copy type-info (last τs) [maybe-bot? (ormap maybe-bot? τs)]))

;; Top

(define Top (type-info 'Top '() #f))

(define (Top? v)
  (and (type-info? v)
       (eq? (type-info-ctor v) 'Top)))

;; Empty

(define Empty (type-info 'Empty '() #f))

(define (Empty? v)
  (and (type-info? v)
       (eq? (type-info-ctor v) 'Empty)))

;; Bot

(define Bot (type-info 'Empty '() #t))

(define (Bot? v)
  (and (Empty? v) (maybe-bot? v)))

;; Number

(define Number (type-info 'Number '() #f))

(define (Number? v)
  (and (type-info? v)
       (eq? (type-info-ctor v) 'Number)))

;; Real

(define Real (type-info 'Real '() #f))

(define (Real? v) 
  (and (type-info? v)
       (eq? (type-info-ctor v) 'Real)))

;; Integer

(define Integer (type-info 'Integer '() #f))
(define (Integer? v)
  (and (type-info? v)
       (eq? (type-info-ctor v) 'Integer)))

;; Boolean

(define Boolean (type-info 'Boolean '() #f))
(define (Boolean? v)
  (and (type-info? v)
       (eq? (type-info-ctor v) 'Boolean)))

;; String

(define String (type-info 'String '() #f))

(define (String? v)
  (and (type-info? v)
       (eq? (type-info-ctor v) 'String)))

;; Literal

(define (Literal v)
  (type-info 'Literal (list v) #f))

(define (Literal? v)
  (and (type-info? v)
       (eq? (type-info-ctor v) 'Literal)))

(define/contract (Literal-v τ)
  (-> Literal? any/c)
  (car (type-info-ctor-args τ)))

;; Pair

(define (Pair τ_a τ_d)
  (type-info 'Pair
             (list (project τ_a)
                   (project τ_d)
                   (and (static? τ_a) (static? τ_d)))
             (or (maybe-bot? τ_a) (maybe-bot? τ_d))))

(define (Pair? v)
  (and (type-info? v) (eq? (type-info-ctor v) 'Pair)))

(define/contract (Pair-car τ)
  (-> Pair? type-info?)
  (car (type-info-ctor-args τ)))

(define/contract (Pair-cdr τ)
  (-> Pair? type-info?)
  (cadr (type-info-ctor-args τ)))

(define/contract (Pair-static? τ)
  (-> Pair? boolean?)
  (caddr (type-info-ctor-args τ)))

;;

(define (static? τ)
  (cond
    [(Literal? τ) #t]
    [(Pair? τ) (Pair-static? τ)]
    [else #f]))
  
(define (valid-literal? v)
  (or (integer? v)
      (real? v)
      (boolean? v)
      (string? v)))
  
(define (datum->type v)
  (cond
    [(pair? v) (Pair (datum->type (car v)) (datum->type (cdr v)))]
    [(valid-literal? v) (Literal v)]
    [else (error)]))
  
(define (type->datum τ)
  (cond
    [(not (static? τ)) (error)]
    [(Literal? τ) (Literal-v τ)]
    [(Pair? τ) (cons (type->datum (Pair-car τ))
                     (type->datum (Pair-cdr τ)))]
    [else (error "unreachable")]))
  
(define (supertype τ)
  (define super-τ
    (cond
      [(Top? τ) #f]
      [(Empty? τ) #f]
      [(Number? τ) Top]
      [(Real? τ) Number]
      [(Integer? τ) Real]
      [(Boolean? τ) Top]
      [(String? τ) Top]
      [(Pair? τ) #f]
      [(not (static? τ)) #f]
      [(exact-integer? (type->datum τ)) Integer]
      [(real? (type->datum τ)) Real]
      [(boolean? (type->datum τ)) Boolean]
      [(string? (type->datum τ)) String]
      [else #f]))
  (if super-τ (seq τ super-τ) #f))

(define (type=? τ1 τ2)
  (cond
    [(not (eq? (maybe-bot? τ1) (maybe-bot? τ2))) #f]
    [(and (Literal? τ1) (Literal? τ2))
     (eqv? (Literal-v τ1) (Literal-v τ2))]
    [(and (Pair? τ1) (Pair? τ2))
     (and (type=? (Pair-car τ1) (Pair-car τ2))
          (type=? (Pair-cdr τ1) (Pair-cdr τ2)))]
    [else (eq? (type-info-ctor τ1) (type-info-ctor τ2))]))
  
(define (type<=? τ1 τ2)
  (cond
    [(and (maybe-bot? τ1) (not (maybe-bot? τ2))) #f]
    [(Top? τ2) #t]
    [(Empty? τ1) #t]
    [(and (Pair? τ1) (Pair? τ2))
     (and (type<=? (Pair-car τ1) (Pair-car τ2))
          (type<=? (Pair-cdr τ1) (Pair-cdr τ2)))]
    [(type=? τ1 τ2) #t]
    [(supertype τ1) => (λ (super-τ1) (type<=? super-τ1 τ2))]
    [else #f]))
  
(define (type-comparable? τ1 τ2)
  (or (type<=? τ1 τ2) (type<=? τ2 τ1)))

(define (lub τ1 τ2)
  (define τ
    (cond
      [(or (Top? τ1) (Top? τ2)) Top]
      [(Empty? τ1) τ2]
      [(Empty? τ2) τ1]
      [(and (Pair? τ1) (Pair? τ2))
       (Pair (lub (Pair-car τ1) (Pair-car τ2))
             (lub (Pair-cdr τ1) (Pair-cdr τ2)))]
      [(type=? τ1 τ2) τ1]
      [(and (type<=? τ1 τ2) (supertype τ1)) =>
       (λ (super-τ1) (lub super-τ1 τ2))]
      [(and (type<=? τ2 τ1) (supertype τ2)) =>
       (λ (super-τ2) (lub τ1 super-τ2))]
      [else (error)]))
  (seq τ1 τ2 τ))
