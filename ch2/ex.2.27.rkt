#lang racket/base

(define (deep-reverse lst)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (let* ([x (car xs)]
               [x-rev (if (pair? x)
                          (deep-reverse x)
                          x)])
          (iter (cdr xs)
                (cons x-rev ys)))))
  (iter lst '()))

(module+ test
  (require rackunit)

  (check-equal? (deep-reverse '((1 2) (3 4))) '((4 3) (2 1))))