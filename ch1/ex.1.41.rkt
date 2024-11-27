#lang racket/base

(require support)

(define (double f) (lambda (x) (f (f x))))

(module+ test
  (require rackunit)

  (check-= (((double double) inc) 5) 9 0)
  (check-= (((double (double double)) inc) 5) 21 0))