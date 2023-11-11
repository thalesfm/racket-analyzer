#lang racket

(provide empty-environment
         (rename-out [bound-id-table-ref environment-ref]
                     [bound-id-table-set environment-set]
                     [bound-id-table-keys environment-keys]))

(require syntax/id-table)
 
(define empty-environment (make-immutable-bound-id-table))