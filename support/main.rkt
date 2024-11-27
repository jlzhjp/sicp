#lang racket/base

(provide square
         identity
         inc
         dec
         average
         accumulate
         flatmap
         enumerate-interval
         atom?)

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (identity x) x)

(define (average . xs) (/ (apply + xs) (length xs)))

(define accumulate foldr)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (inc low) high))))

(define (flatmap proc seq) (foldr append '() (map proc seq)))

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(module+ test
  (require rackunit)

  (check-= (average 1 2 3) 2 0))