#lang racket/base

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p _) p)))
(define (cdr z) (z (lambda (_ q) q)))

(module+ test
  (require rackunit)

  (define z (cons 1 2))
  (check-= (car z) 1 0)
  (check-= (cdr z) 2 0))