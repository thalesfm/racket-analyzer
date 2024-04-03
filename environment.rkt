#lang racket/base

(provide
 (rename-out
  [make-immutable-bound-id-table make-environment]
  [bound-id-table-ref environment-ref]
  [bound-id-table-set environment-set]))

(require syntax/id-table)