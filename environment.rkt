#lang racket/base

(provide
 (rename-out
  [make-immutable-bound-id-table make-ρ]
  [bound-id-table-ref ρ-ref]
  [bound-id-table-set ρ-set]))

(require syntax/id-table)