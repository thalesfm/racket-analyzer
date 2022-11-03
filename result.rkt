#lang racket

(provide (struct-out result))
;; (provide seq)

(require "errors.rkt")

(struct result (values errors) #:transparent)

(define (seq r1 r2)
  (result (result-values r2)
          (errors-append (result-errors r1) (result-errors r2))))