#lang racket/base

(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? last-guess guess)
  (< (/ (abs (- guess last-guess)) last-guess) 0.00000001))

(define (sqrt-iter last-guess x)
  (let ([guess (improve last-guess x)])
    (if (good-enough? last-guess guess)
        guess
        (sqrt-iter guess x))))

(define (sqrt x)
  (when (< x 0)
    (error 'sqrt "negative number"))
  (if (= x 0)
      0
      (sqrt-iter 1.0 x)))

(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
   (describe "test sqrt"
     (it "should handle perfect squares"
       (expect
        [(sqrt 4) ~> 2]
        [(sqrt 9) ~> 3]
        [(sqrt 16) ~> 4]
        [(sqrt 100) ~> 10]))
     (it "should handle irrational square roots"
       (expect
        [(sqrt 2) ~> 1.4142135623730951]
        [(sqrt 3) ~> 1.7320508075688772]
        [(sqrt 5) ~> 2.23606797749979]))
     (it "should handle extreme large numbers"
       (expect
        [(sqrt 1e20) ~> 1e10]
        [(sqrt 1e50) ~> 1e25]
        [(sqrt 1e100) ~> 1e50]
        [(sqrt 1e200) ~> 1e100]))
     (it "should handle extreme small numbers"
       (expect
        [(sqrt 1e-20) ~> 1e-10]
        [(sqrt 1e-50) ~> 1e-25]
        [(sqrt 1e-100) ~> 1e-50]
        [(sqrt 1e-200) ~> 1e-100]))
     (it "should handle special cases"
       (expect
        [(sqrt 0) => 0]
        [(sqrt 1) ~> 1]))
     (it "should throw an error for negative numbers"
       (expect
        [(sqrt -1) =!> exn:fail?]
        [(sqrt -100) =!> exn:fail?]))
     (it "should handle numbers very close to perfect squares"
       (expect
        [(sqrt 4.000001) ~> 2.0000002]
        [(sqrt 3.999999) ~> 1.9999998])))))