#lang racket/base

(define (square x) (* x x))

(define (fib n)
  (define (a* a b p q) (+ (* b q) (* a q) (* a p)))
  (define (b* a b p q) (+ (* b p) (* a q)))
  (define (p* p q) (+ (square p) (square q)))
  (define (q* p q) (+ (square q) (* 2 p q)))

  (define (fib-iter a b p q count)
    (cond [(= count 0) b]
          [(even? count) (fib-iter a b (p* p q) (q* p q) (/ count 2))]
          [else (fib-iter (a* a b p q) (b* a b p q) p q (- count 1))]))
  (fib-iter 1 0 0 1 n))

(module+ test
  (require rackunit)

  (check-= (fib 1) 1 0)
  (check-= (fib 2) 1 0)
  (check-= (fib 3) 2 0)
  (check-= (fib 4) 3 0)
  (check-= (fib 5) 5 0)
  (check-= (fib 6) 8 0)
  (check-= (fib 7) 13 0)
  (check-= (fib 8) 21 0)
  (check-= (fib 9) 34 0)
  (check-= (fib 10) 55 0))