#lang racket

(provide var const primop)

(require syntax/parse)

;; Locally bound identifier
(define-syntax-class var
  (pattern (~and id:id (~fail #:unless (eq? (identifier-binding #'id) 'lexical)))))

(define-syntax-class const
  #:literals (quote)
  (pattern (quote datum) #:attr value (syntax-e #'datum)))

;; TODO: Actually check if id has a top-level-binding
;; Identifier with top-level binding
(define-syntax-class primop
  (pattern id:id))
