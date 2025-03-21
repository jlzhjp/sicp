#lang racket/base

(define (fringe x)
  (cond [(null? x) '()]
        [(not (pair? x)) (list x)]
        [else (append (fringe (car x))
                      (fringe (cdr x)))]))

(module+ test
  (require rackunit)

  (define x (list (list 1 2) (list 3 4)))
  (check-equal? (fringe x) '(1 2 3 4))
  (check-equal? (fringe (list x x)) '(1 2 3 4 1 2 3 4)))