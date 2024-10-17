#lang racket/base

(provide make-interval
         upper-bound
         lower-bound)

(define (make-interval a b) (list 'interval a b))
(define (lower-bound interval) (cadr interval))
(define (upper-bound interval) (caddr interval))

(module+ test
  (require rackunit)

  (define interval (make-interval 3 8))
  (check-= (lower-bound interval) 3 0)
  (check-= (upper-bound interval) 8 0))