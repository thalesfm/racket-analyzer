#lang racket/base

(provide memoize)

(require racket/dict
         racket/list
         racket/hash-code)

(define (memoize proc)
  (define memo
    (make-weak-custom-hash
     (lambda (args args′)
       (and (= (length args) (length args′))
            (andmap eq? args args′)))
     (lambda (args)
       (hash-code-combine* (map eq-hash-code args)))))
  (make-keyword-procedure
   (lambda (kws kw-args . pos-args)
     (define args (append* pos-args (map list kws kw-args)))
     (ephemeron-value
       (or (dict-ref memo args #f)
           (let ([v (keyword-apply proc kws kw-args pos-args)])
             (define eph (make-ephemeron args v))
             (dict-set! memo args eph)
             eph))))
   (lambda args
     (ephemeron-value
       (or (dict-ref memo args #f)
           (let ([v (apply proc args)])
             (define eph (make-ephemeron args v))
             (dict-set! memo args eph)
             eph))))))