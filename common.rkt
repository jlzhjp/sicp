#lang racket/base

(provide square
         identity
         inc
         dec
         average)

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (identity x) x)

(define (average . xs) (/ (apply + xs) (length xs)))

(module+ test
  (require rackunit)

  (check-= (average 1 2 3) 2 0))