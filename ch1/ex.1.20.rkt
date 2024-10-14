#lang racket/base

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(module+ test
  (require rackunit)

  (check-= (gcd 12 8) 4 0)
  (check-= (gcd 9 8) 1 0))