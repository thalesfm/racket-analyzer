#lang racket

(define-syntax (compare-stx stx)
  (define test-stx #'(let ([x 1]) x (let ([x 2]) x)))
  (define expanded-stx (local-expand test-stx '() '()))
    (syntax-case expanded-stx (let-values)
      [(let-values _ x1 (let-values _ x2))
	   (with-syntax ([free=? (free-identifier=? #'x1 #'x2)]
	                 [bound=? (bound-identifier=? #'x1 #'x2)])
	     #'(list free=? bound=?))]))

(printf "~v\n" (compare-stx))
