#lang racket/base

(provide free-vars)

(require (prefix-in base: syntax/free-vars)
         "memoize.rkt")

(define free-vars (memoize base:free-vars))