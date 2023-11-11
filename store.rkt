#lang racket

(provide (rename-out [make-ephemeron-hasheq make-store]
                     [hash-ref store-ref]
                     [hash-set! store-set!]
                     [hash-update! store-update!]))