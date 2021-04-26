#lang racket

(require (for-syntax racket/syntax))

(begin-for-syntax
  (define intdef-ctx (syntax-local-make-definition-context))
  (with-syntax* ([id (generate-temporary)]
                 [expr #'(+ id 3)])
    (syntax-local-bind-syntaxes (list #'id) #'(lambda (stx) #'10) intdef-ctx)
    (define expanded-expr (local-expand #'expr (list #'id) '() intdef-ctx))
    (displayln (syntax->datum expanded-expr))))
