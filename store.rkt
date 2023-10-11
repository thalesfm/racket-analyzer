#lang racket

(provide store-ref store-set! store-update!)

; Could be replaced by an ephemeron hash table
; (available from version 8.0 onwards of package `base`)
(define *store* (make-weak-hasheq))

(define (store-ref key [failure-result #f])
  (define eph (hash-ref *store* key #f))
  (if eph (ephemeron-value eph) failure-result))

(define (store-set! key v)
  (define eph (make-ephemeron key v))
  (hash-set! *store* key eph))

(define (store-update! key updater [failure-result #f])
  (define v (updater (store-ref key failure-result)))
  (define eph (make-ephemeron key v))
  (hash-set! *store* key eph))
