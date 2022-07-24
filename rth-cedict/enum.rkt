#lang racket/base

(require (for-syntax racket/base))

(provide enum)

#|
Usage: (enum A B _ C)
assigns 0, 1 ... to variables.
Skips over _
|#

(define-syntax (enum stx)
  ;;(print (syntax-source stx))
  (let ([vars (cdr (syntax->list stx))])
    ;; Predicate for _
    (define (regular-variable? var-stx) (not (eq? (syntax->datum var-stx) '_)) )

    ;; check the supplied args
    ;; they must be identifiers
    (for-each (lambda (elt)
                (unless (identifier? elt)
                  (raise-syntax-error #f "Identifier expected" stx elt)))
              vars)

    ;; ids must be distinct (except for the magic _ )
    (let* ([non-magic-vars (filter regular-variable? vars)]
           [dup (check-duplicate-identifier non-magic-vars)])
      (when dup
        (raise-syntax-error #f "Duplicate identifier" stx dup)))

    ;; generate code
    (datum->syntax stx `(begin
                          ,@(for/list ([v vars]
                                       [i (in-naturals)]
                                       #:when (regular-variable? v))
                              `(define ,v ,i))))  ))
