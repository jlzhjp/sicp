#lang racket/base

(define (square x) (* x x))

(define (fast-expt b n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        (else (* b (fast-expt b (- n 1))))))

(module+ test
  (require rackunit)
  (check-= (fast-expt 2 9) 512 0)
  (check-= (fast-expt 2 10) 1024 0))

(define (fast-expt* b n)
  (define (fast-expt-iter a b n)
    (cond [(= n 0) a]
          [(even? n) (fast-expt-iter a (square b) (/ n 2))]
          [else (fast-expt-iter (* a b) b (- n 1))]))
  (fast-expt-iter 1 b n))

(module+ test
  (check-= (fast-expt* 2 9) 512 0)
  (check-= (fast-expt* 2 10) 1024 0))