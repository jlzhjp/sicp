#lang racket/base

(require (only-in "ex.1.35.rkt" fixed-point))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

(module+ test
  (require rackunit)

  (check-= (newtons-method (cubic -3 1 1) 1.0) 1.0 1e-5)
  (check-= (newtons-method (cubic -1 -2 0) 1.0) -1.0 1e-5))