#lang racket

(provide abstract-eval)

(require syntax/parse
         "abstract-value.rkt"
         "environment.rkt"
         "primop.rkt"
         "syntax-util.rkt")

;; (module->namespace 'racket/base)

(define (abstract-eval top-level-form [namespace (current-namespace)])
  (abstract-eval-kernel-syntax
   (expand
    (namespace-syntax-introduce (datum->syntax #f top-level-form) namespace))))

(define (abstract-eval-kernel-syntax expr [ρ (make-environment)])
  (let eval ([expr expr] [ρ ρ])
    (syntax-parse expr
      #:literal-sets (kernel-literals)
      ;; FIXME: Re-entrant on bad `letrec`
      [x:var (force (lookup ρ #'x))]
      [k:const (attribute k.value)]
      [o:primop (get-primop (attribute o.value))]

      [(#%plain-app expr0 expr ...)
       (define proc (eval #'expr0 ρ))
       (define args
         (for/list ([expr (in-syntax #'(expr ...))])
           (eval expr ρ)))
       (cond
         [(procedure? proc) (abstract-apply proc args)]
         [else ⊥])]

      [(#%plain-lambda (x ...) expr)
       (define (proc . args)
         (cond
          [(ormap ⊥? args) ⊥]
          [else (eval #'expr (extend* ρ #'(x ...) args))]))
       (procedure-rename proc (gensym))]

      [(if expr0 expr1 expr2)
       (define d (eval #'expr0 ρ))
       (cond
        [(eq? d #t) (eval #'expr1 ρ)]
        [(eq? d #f) (eval #'expr2 ρ)]
        [(eq? d  T) (lub (eval #'expr1 ρ) (eval #'expr2 ρ))]
        [(eq? d  ⊥) ⊥])]

      [(let-values ([(x) expr] ...) expr0)
       (eval #'(#%plain-app (#%plain-lambda (x ...) expr0) expr ...) ρ)]

      ;; TODO: letrec for regular values (not procedures)

      [(letrec-values ([(x) (~and expr (#%plain-lambda . _))] ...) expr0)
       (define vals
         (for/list ([x (in-syntax #'(x ...))]
                    [expr (in-syntax #'(expr ...))])
           (make-recursive
             (lambda (val)
               (eval expr (extend ρ x val))))))
       (eval #'expr0 (extend* ρ #'(x ...) vals))])))

;; (struct labelled-procedure (label proc)
;;  #:property prop:procedure (struct-field-index proc))
;; (define procedure-label labelled-procedure-label)

(define current-stack (make-parameter (hasheq)))

;; Todo: check if it's abstract procedure or not
(define (abstract-apply proc args)
  (let* ([args′ (hash-ref (current-stack) (object-name proc) (make-list (length args) ⊥))]
         [args″ (map lub args args′)])
    (parameterize ([current-stack (hash-set (current-stack) (object-name proc) args″)])
      (apply proc args″))))

(define (make-recursive phi)
  (letrec ([psi (procedure-rename
                  (lambda (arg)
                    (iterate-fixpoint
                      (lambda (v) ((phi (subst psi arg v)) arg))))
                  (gensym))])
    psi))

(define (iterate-fixpoint proc [v0 ⊥])
  (let ([v1 (proc v0)])
    (if (<=? v1 v0) v0 (iterate-fixpoint proc v1))))

(define (subst proc arg v)
  (define (proc′ arg′)
    (if (eqv? arg arg′) v (proc arg′)))
  (procedure-rename proc′ (object-name proc)))