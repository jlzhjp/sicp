#lang racket/base

(provide encode)

(require "ex.2.67.rkt")

(define (encode message tree)
  (define (encode-symbol symbol tree)
    (cond [(leaf? tree) '()]
          [(memq symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree)))]
          [(memq symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree)))]
          [else (error "bad symbol -- ENCODE-SYMBOL" symbol)]))

  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(module+ test
  (require support/testing)

  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree (make-leaf 'D 1)
                                     (make-leaf 'C 1)))))

  (check-equal? (encode '(A D A B B C A) sample-tree)
                '(0 1 1 0 0 1 0 1 0 1 1 1 0)))
