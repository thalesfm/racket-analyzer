#lang racket

(provide (struct-out closure) closure<=? closure-lub)

(require "environment.rkt"
         "domain.rkt")

(struct closure (lambda environment) #:transparent)

; TODO: Double check there isn't a risk that this goes into an
; infinite loop when both arguments are recursive procedures
(define (closure<=? domain clo1 clo2)
  (and (eq? (closure-lambda clo1)
            (closure-lambda clo2))
       (environment<=? domain
                       (closure-environment clo1)
                       (closure-environment clo2))))

; TODO: Double check there isn't a risk that this goes into an
; infinite loop when both arguments are recursive procedures
(define (closure-lub domain clo1 clo2)
  (cond
   [(eq? (closure-lambda clo1) (closure-lambda clo2))
    (closure (closure-lambda clo1)
             (environment-lub domain
                              (closure-environment clo1)
                              (closure-environment clo2)))]
   [else (T domain)]))
