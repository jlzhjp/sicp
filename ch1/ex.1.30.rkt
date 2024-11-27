#lang racket/base

(require support)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(module+ test
  (require rackunit)

  (check-= (sum identity 1 inc 10) 55 0)
  (check-= (sum square 1 inc 10) 385 0))