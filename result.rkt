#lang racket

(provide (struct-out result))
;; (provide seq)

(struct result (value errors) #:transparent)

#;(define (seq . ts)
  (abstr (abstr-repr (last ts))
   (ormap abstr>=bot? ts)))