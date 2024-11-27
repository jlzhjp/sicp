#lang racket/base

(require (only-in support accumulate))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (cond [(null? x) 0]
                               [(not (pair? x)) 1]
                               [else (count-leaves x)]))
                       t)))

(module+ test
  (require rackunit)

  (define x (cons (list 1 2) (list 1 2)))
  (check-= (count-leaves x) 4 0)
  (check-= (count-leaves (list x x)) 8 0))