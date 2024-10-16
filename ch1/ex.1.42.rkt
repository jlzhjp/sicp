#lang racket/base

(provide compose)

(require "../common.rkt")

(define (compose f g) (lambda (x) (f (g x))))

(module+ test
  (require rackunit)

  (check-= ((compose square inc) 6) 49 0))