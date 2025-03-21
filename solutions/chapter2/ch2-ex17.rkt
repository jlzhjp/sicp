#lang racket/base

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(module+ test
  (require rackunit)

  (check-equal? (last-pair (list 23 72 149 34)) '(34)))