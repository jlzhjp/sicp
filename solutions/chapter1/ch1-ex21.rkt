#lang racket/base

(provide smallest-divisor)

(require akari-sicp/lib/common)

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

(define (smallest-divisor n) (find-divisor n 2))

(module+ test
  (require rackunit)
  (check-= (smallest-divisor 199) 199 0)
  (check-= (smallest-divisor 1999) 1999 0)
  (check-= (smallest-divisor 19999) 7 0))
