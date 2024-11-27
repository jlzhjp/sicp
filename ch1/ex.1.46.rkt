#lang racket/base

(require (only-in support square average))

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  try)

(define tolerance 1e-5)

(define (close-enough? v1 v2 tol)
  (< (abs (- v1 v2)) tol))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (close-enough? (square guess) x tolerance))
    (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (close-enough? guess (f guess) tolerance))
    f)
   first-guess))


(module+ test
  (require rackunit)

  (check-= (sqrt 4) 2 tolerance)
  (check-= (sqrt 16) 4 tolerance)

  ; taken from ex.1.35.rkt
  (define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
  (check-= golden-ratio (/ (+ 1 (sqrt 5)) 2) tolerance))