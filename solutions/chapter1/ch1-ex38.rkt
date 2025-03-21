#lang racket/base

(require (only-in "ch1-ex37.rkt" cont-frac*))

(define (approximate-e k)
  (define (n _) 1)
  (define (d i)
    (if (= (remainder (+ i 1) 3) 0)
        (* 2.0
           (/ (+ i 1)
              3.0))
        1.0))
  (+ (cont-frac* n d k) 2))

(module+ test
  (require rackunit)

  (check-= (approximate-e 100) (exp 1) 1e-10))