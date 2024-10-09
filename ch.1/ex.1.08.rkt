#lang racket/base

(require rackunit)

(define (cube-root x)
  (define (square x) (* x x))

  (define (good-enough? last-guess guess)
    (< (/ (abs (- guess last-guess)) last-guess) 0.001))

  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (cube-root-iter last-guess x)
    (let ([guess (improve last-guess x)])
      (if (good-enough? last-guess guess)
          guess
          (cube-root-iter guess x))))

  (cube-root-iter 1.0 x))

(check-= (cube-root 8) 2 0.01)
(check-= (cube-root 27) 3 0.01)
(check-= (cube-root 729) 9 0.01)
