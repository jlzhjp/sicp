#lang racket/base

(require (rename-in "ex.2.09.rkt"
                    [mul-interval old-mul-interval])
         (only-in "ex.2.12.rkt"
                  make-center-percent
                  center-interval
                  percent-interval))

(define (mul-interval x y)
  (make-center-percent
   (* (center-interval x) (center-interval y))
   (+ (percent-interval x) (percent-interval y))))

(module+ test
  (require rackunit)

  (define interval1 (make-center-percent 10 0.01))
  (define interval2 (make-center-percent 20 0.005))
  (check-within (mul-interval interval1 interval2)
                (old-mul-interval interval1 interval2)
                0.011))