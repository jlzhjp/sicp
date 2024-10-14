#lang racket/base

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(module+ test
  (require rackunit)

  (check-= (mul 8 9) 72 0)
  (check-= (mul 9 9) 81 0))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond [(= b 1) a]
        [(even? b) (double (fast-mul a (halve b)))]
        [else (+ a (fast-mul a (- b 1)))]))

(module+ test
  (check-= (fast-mul 8 9) 72 0)
  (check-= (fast-mul 9 9) 81 0))