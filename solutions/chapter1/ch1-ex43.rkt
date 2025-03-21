#lang racket/base

(provide repeated)

(require akari-sicp/lib/common)

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(module+ test
  (require rackunit)

  (check-= ((repeated square 2) 5) 625 0))
