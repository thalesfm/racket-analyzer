#lang racket

;; TODO: Avoid unnecessary computation when `proc` is not recursive
(define ((fix proc) . args)
  (printf "~a~n" (cons 'proc args))
  (call-with-values
    (lambda () (apply proc args))
    (lambda result
      (if (equal? args result)
          (apply values result)
          (apply (fix proc) result)))))


; f(x) = E[f(x)]
;    y = E[y]
;    y = fix(E[])

; g(x) = if x = 0 then x else g(x - 1)
; g(3) = fix(???)
