#lang racket

(provide seq
         let/seq
         let*/seq
         for/seq)

(require syntax/parse/define
         (for-syntax racket/syntax
                     syntax/for-body
                     syntax/parse)
         "types.rkt")

(define-syntax-parser seq
  [(_ expr:expr ... final-expr:expr)
    #'(if (or (Bot? expr) ...) Bot final-expr)])

(define-syntax-parser let/seq
  [(_ () body ...+) #'(let () body ...)]
  [(_ ([id:id val-expr:expr] . rest) body ...+)
    (with-syntax ([temp-id (generate-temporary #'id)])
      #'(let ([temp-id val-expr])
          (seq temp-id (let/seq rest (let ([id temp-id]) body ...)))))])

(define-syntax-parser let*/seq
  [(_ () body ...+) #'(let () body ...)]
  [(_ ([id:id val-expr:expr] . rest) body ...+)
    #'(let ([id val-expr])
        (seq id (let*/seq rest body ...)))])

(define-syntax-parser for/seq
  [(_ (for-clause ...) body-or-break ... body)
    #:with ((pre-body ...) (post-body ...))
          (split-for-body this-syntax #'(body-or-break ... body))
    #'(for/fold/derived this-syntax
                        ([accum (void)])
                        (for-clause ...
                         #:break (Bot? accum))
        pre-body ...
        (seq accum (let () post-body ...)))])
