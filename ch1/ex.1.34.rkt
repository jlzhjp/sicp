#lang racket/base

(define tolerance 0.00001)

(provide fixed-point)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(module+ test
  (require rackunit)

  (check-= golden-ratio (/ (+ 1 (sqrt 5)) 2) tolerance))