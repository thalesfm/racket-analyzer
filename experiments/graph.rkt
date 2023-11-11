#lang racket
; (let ((a '(10 . #0=(20 30 . #0#))))
;   (displayln a))

(let* ([ph (make-placeholder #f)]
       [b (list* 20 30 ph)]
       [a (cons 10 b)])
  (placeholder-set! ph b)
  (make-reader-graph a))
