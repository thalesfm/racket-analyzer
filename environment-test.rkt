#lang racket

(require rackunit
         "environment.rkt"
         "types.rkt")

(let* ([env1 (make-empty-environment)]
       [env1 (bind env1 #'a 10)]
       [env2 (make-empty-environment)]
       [env2 (bind env2 #'b 11)]
       [env  (environment-lub env1 env2)])
    (check-eq? (lookup env #'a) 10)
    (check-eq? (lookup env #'b) 11))
