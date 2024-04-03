#lang racket

(provide make-union-find ; make-equivalence
         union-find-ref ; equivalence-class
         union-find-union! ; equivalence-set!
         union-find-same-set? ; equivalence-ref

(define (make-union-find)
  (make-ephemeron-hasheq))

(define (equivalence-relation-add! rel v1 v2)
  (error "not implemented"))

(define (in-path node)
  (if (root? node)
      (in-value node)
      (stream-cons #:eager node
                   (in-path (link-next node)))))

(define (find-root node)
  (define root
    (for/last ([node (in-path node)])
      node))
  (for ([node (in-path node)]
        #:when (link? node))
    (set-link-next! node root))
  root)

;; FIXME: Could cause problems if `uf` already contains `v`
(define (union-find-add! uf v)
  (hash-set! uf v (root 0 v)))

(define (make-failure-result-for v)
  (lambda ()
    (raise-arguments-error 'union-find-ref "no set found for value" "v" v)))

(define (union-find-ref uf v [failure-result (make-failure-result-for v)])
  (define node (hash-ref uf v #f))
  (if node
      (root-value (find-root node))
      (if (procedure? failure-result) (failure-result) failure-result)))

;; Returns true if `v1` and `v2` belong to the same set, otherwise returns false.
;; In particular, this procedure returns false if `uf` doesn't contain either `v1`, `v2`, or both.
(define (union-find-same-set? uf v1 v2)
  (eq? (union-find-ref uf v1) (union-find-ref uf v2)))

(define (union-find-union! uf v1 v2 #:combine [combine (lambda (a b) a)])
  (define r1 (find-root! uf v1))
  (define r2 (find-root! uf v2))

#| Generic dict interface for union-find:

;; Returns the value for the equivalence class of `key`.
(define (union-find-ref uf key [failure-result ...]) ...)

;; Maps the equivalence class of `key` to `v`, overwriting any existing mapping
(define (union-find-set! uf key v) ...)

(define (union-find-count uf) ...)
(define (union-find-iterate-*) ...)
|#