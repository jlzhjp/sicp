#lang racket/base

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

(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
   (describe "cube-root"
     (it "should return the cube root of a number"
       (expect [(cube-root 1) => 1]
               [(cube-root 0) => 0]
               [(cube-root -1) => -1]
               [(cube-root 2) ~> 1.2599210498948732]
               [(cube-root 3) ~> 1.4422495703074083]
               [(cube-root 4) ~> 1.5874010519681994]
               [(cube-root 5) ~> 1.7099759474226504]
               [(cube-root 6) ~> 1.8171205928321397]
               [(cube-root 7) ~> 1.9129311827745818])))))