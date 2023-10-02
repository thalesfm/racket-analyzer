#lang racket

(provide free-vars)

(require syntax/parse)

(define (union-map proc lst)
  (apply set-union (map proc lst)))

;; TODO: Cache results as syntax properties
(define (free-vars stx)
  (syntax-parse stx
    #:datum-literals (quote if let letrec)
    [var:id (set (syntax-e #'var))]
    [(quote ~! _) (set)]
    [((~datum lambda) ~! (arg-id:id ...) body:expr)
     (set-subtract (free-vars #'body)
                   (list->set (syntax->list #'(arg-id ...))))]
    [(if ~! test:expr then:expr else:expr)
     (set-union (free-vars #'test)
                (free-vars #'then)
                (free-vars #'else))]
    [(let ~! ([var:id val-expr:expr] ...) body)
     (set-union
       (union-map free-vars (syntax->list #'(val-expr ...)))
       (set-subtract (free-vars #'body)
                     (list->set (syntax->list #'(var ...)))))]
    [(letrec ~! ([var:id val-expr:expr] ...) body)
     ;; FIXME: `var` should not be considered a free variable in
     ;; subsequent value expressions
     (set-union
       (union-map free-vars (syntax->list #'(val-expr ...)))
       (set-subtract (free-vars #'body)
                     (list->set (syntax->list #'(var ...)))))]
    [(proc-expr:expr arg-expr:expr ...)
     (union-map free-vars (syntax->list #'(proc-expr arg-expr ...)))]
    [datum #:when (not (list? (syntax-e #'datum))) (set)]))
