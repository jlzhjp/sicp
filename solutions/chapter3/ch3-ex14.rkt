#lang racket/base

(require compatibility/mlist)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ([temp (mcdr x)])
          (set-mcdr! x y)
          (loop temp x)))) ; x = temp, y = x
  (loop x '()))

(define v (mlist 'a 'b 'c))
(define w (mystery v))

(module+ test
  (require akari-sicp/lib/testing)

  (check-equal? v (mlist 'a))
  (check-equal? w (mlist 'c 'b 'a)))