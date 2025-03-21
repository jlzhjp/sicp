#lang racket/base

(define (reverse lst)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs)
              (cons (car xs) ys))))
  (iter lst '()))

(module+ test
  (require rackunit)

  (check-equal? (reverse (list 1 4 9 16 25))
                '(25 16 9 4 1)))