#lang racket

(require racket/control)

(define saved-k #f)

(define (save-it/cc!)
  (call/cc
    (lambda (k)
      (set! saved-k k)
      0)))

(define (save-it/comp!)
  (call/comp
    (lambda (k)
      (set! saved-k k)
      0)))

(void
  (prompt
    (+ 1 (+ 1 (save-it/cc!)))))
(prompt (saved-k 0))        ; -> 2
(prompt (+ 10 (saved-k 0))) ; -> 2
(newline)

(void
  (+ 1
    (prompt
      (+ 1 (save-it/cc!)))))
(prompt (saved-k 0))        ; -> 1
(prompt (+ 10 (saved-k 0))) ; -> 1
(newline)

(void
  (prompt
    (+ 1 (+ 1 (save-it/comp!)))))
(saved-k 0)        ; -> 2
(+ 10 (saved-k 0)) ; -> 12
(newline)

(void
  (+ 1
    (call-with-continuation-prompt
      (lambda ()
        (+ 1 (save-it/comp!))))))
(saved-k 0)        ; -> 1
(+ 10 (saved-k 0)) ; -> 11
