#lang racket/base

(require support/stream
         "ex.3.50.rkt")

(define (show x)
  (displayln x)
  x)

(module+ main
  (define x
    (stream-map show (stream-enumerate-interval 0 10)))

  (void (stream-ref x 5))
  (void (stream-ref x 7)))