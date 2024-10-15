#lang racket/base

(require racket/math)
(require (only-in "ex.1.37.rkt" cont-frac*))

(define (tan-cf k x)
  (define (n i) (if (= i 1) x (- (expt x 2))))
  (define (d i) (- (* 2 i) 1))
  (cont-frac* n d k))

(module+ test
  (require rackunit)

  (check-= (tan-cf 100 (/ pi 3)) (sqrt 3) 1e-9)
  (check-= (tan-cf 100 (/ pi 4)) 1.0 1e-9)
  (check-= (tan-cf 100 (/ pi 6)) (/ 1 (sqrt 3)) 1e-9))
