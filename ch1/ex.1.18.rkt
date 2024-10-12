#lang racket/base

(require rackunit)

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (define (fast-mul-iter a b c)
    (cond [(= b 0) c]
          [(even? b) (fast-mul-iter (double a) (halve b) c)]
          [else (fast-mul-iter a (- b 1) (+ a c))]))
  (fast-mul-iter a b 0))

(check-= (fast-mul 8 8) 64 0)
(check-= (fast-mul 8 9) 72 0)