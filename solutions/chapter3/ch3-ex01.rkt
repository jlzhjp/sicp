#lang racket/base

(define (make-accumulator initial)
  (let ([value initial])
    (lambda (x)
      (set! value (+ value x))
      value)))

(module+ test
  (require akari-sicp/lib/testing)

  (define A (make-accumulator 5))

  (check-= (A 10) 15 0)
  (check-= (A 10) 25 0))
