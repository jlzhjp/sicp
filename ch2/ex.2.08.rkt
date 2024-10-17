#lang racket/base

(provide sub-interval)

(require "ex.2.07.rkt")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(module+ test
  (require rackunit)

  (define x (make-interval 2 4))
  (define y (make-interval -1 1))
  (check-equal? (sub-interval x y) (make-interval 1 5)))