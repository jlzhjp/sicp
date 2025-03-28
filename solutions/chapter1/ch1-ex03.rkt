#lang racket/base

(define (sum-of-squares x y) (+ (* x x) (* y y)))

(define (sum-of-squares-of-max-two a b c)
  (if (>= a b)
      (if (>= b c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (if (>= a c)
          (sum-of-squares a b)
          (sum-of-squares b c))))

(module+ test
  (require akari-sicp/lib/testing)
  (expect [(sum-of-squares-of-max-two 1 2 3) => 13]
          [(sum-of-squares-of-max-two 2 3 1) => 13]
          [(sum-of-squares-of-max-two 3 2 1) => 13]
          [(sum-of-squares-of-max-two 2 1 3) => 13]))