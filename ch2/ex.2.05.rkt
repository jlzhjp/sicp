#lang racket/base

(define (divides? y x) (= (remainder x y) 0))

(define (cons x y) (* (expt 2 x) (expt 3 y)))
(define (car x) (if (divides? 3 x) (car (/ x 3)) (log x 2)))
(define (cdr x) (if (divides? 2 x) (cdr (/ x 2)) (log x 3)))

(module+ test
  (require rackunit)

  (define z (cons 3 4))
  (check-= (car z) 3 0)
  (check-= (cdr z) 4 0))