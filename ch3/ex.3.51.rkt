#lang racket/base

(require support/stream
         "ex.3.50.rkt")

(define (show x)
  (displayln x)
  x)

(module+ test
  (require support/testing)

  (define x '())

  (check-output
   (lines "0")
   (set! x (stream-map show (stream-enumerate-interval 0 10))))

  (check-output
   (lines "1" "2" "3" "4" "5")
   (check-= (stream-ref x 5) 5 0))

  (check-output
   (lines "6" "7")
   (check-= (stream-ref x 7) 7 0)))
