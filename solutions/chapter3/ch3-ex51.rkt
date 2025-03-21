#lang racket/base

(require akari-sicp/lib/stream
         "ch3-ex50.rkt")

(define (show x)
  (displayln x)
  x)

(module+ test
  (require akari-sicp/lib/testing)

  (define x '())

  (check-normalized-output
   (lambda ()
     (set! x (stream-map show (stream-enumerate-interval 0 10))))

   '("0"))

  (check-normalized-output
   (lambda ()
     (check-= (stream-ref x 5) 5 0))

   '("1" "2" "3" "4" "5"))

  (check-normalized-output
   (lambda ()
     (check-= (stream-ref x 7) 7 0))

   '("6" "7")))
