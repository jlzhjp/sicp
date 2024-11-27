#lang racket/base

(require support)

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list* items)
  (map square items))

(module+ test
  (require rackunit)

  (check-equal? (square-list '(1 2 3 4)) '(1 4 9 16))
  (check-equal? (square-list* '(1 2 3 4)) '(1 4 9 16)))