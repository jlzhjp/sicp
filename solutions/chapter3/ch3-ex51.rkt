#lang racket/base

(require akari-sicp/lib/stream
         "ch3-ex50.rkt")

(define (show x)
  (displayln x)
  x)

(module+ test
  (require akari-sicp/lib/testing)

  (define x '())

  (run-tests
   (describe "exercise 3.51"
     (it "should only evaluation the first element when initialized"
       (expect [(set! x (stream-map show (stream-enumerate-interval 0 10))) =$> '("0")]))
     (it "should evaluate up to the 5th element when calling (stream-ref x 5)"
       (expect
        [(expect [(stream-ref x 5) => 5]) =$> '("1" "2" "3" "4" "5")]))
     (it "should evaluate up to the 7th element when calling (stream-ref x 7)"
       (expect
        [(expect [(stream-ref x 7) => 7]) =$> '("6" "7")])))))
