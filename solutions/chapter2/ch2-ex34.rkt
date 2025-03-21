#lang racket/base

(require (only-in akari-sicp/lib/common accumulate))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(module+ test
  (require rackunit)

  (check-= (horner-eval 2 (list 1 3 0 5 0 1)) 79 0))