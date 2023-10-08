#lang racket

(provide (struct-out closure) closure<=? closure-lub)

(require "environment.rkt"
         "ordering.rkt")

(struct closure (lambda environment)
        #:methods gen:dcpo
        [(define (gen-<=? dcpo other)
           (if (closure? other) (closure<=? dcpo other) #f))
         (define (gen-lub dcpo other)
           (if (closure? other) (closure-lub dcpo other) T))])

; TODO: Double check there isn't a risk that this goes into an
; infinite loop when both arguments are recursive procedures
(define (closure<=? clo1 clo2)
  (and (eq? (closure-lambda clo1)
            (closure-lambda clo2))
       (environment<=? (closure-environment clo1)
                       (closure-environment clo2))))

; TODO: Double check there isn't a risk that this goes into an
; infinite loop when both arguments are recursive procedures
(define (closure-lub clo1 clo2)
  (cond
   [(eq? (closure-lambda clo1) (closure-lambda clo2))
    (closure (closure-lambda clo1)
             (environment-lub (closure-environment clo1)
                              (closure-environment clo2)))]
   [else T]))
