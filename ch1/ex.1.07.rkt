#lang racket/base

(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (sqrt 1e-20) ; 0.033125
; (sqrt 1e21) ; No Output

(define (good-enough?* last-guess guess)
  (< (/ (abs (- guess last-guess)) last-guess) 0.001))

(define (sqrt-iter* last-guess x)
  (let ([guess (improve last-guess x)])
    (if (good-enough?* last-guess guess)
        guess
        (sqrt-iter* guess x))))

(define (sqrt* x)
  (sqrt-iter* 1.0 x))

(module+ test
  (require rackunit)

  (check-= (sqrt* 1e-20) 1e-10 0.001)

  #|
  (check-= (sqrt* 1e21) 31622776601.684 0.001)
  --------------------
  FAILURE
  name:       check-=
  location:   ch.1/ex.1.7.rkt:39:0
  actual:     31622778383.672726
  expected:   31622776601.684
  tolerance:  0.001
  --------------------
  |#)

