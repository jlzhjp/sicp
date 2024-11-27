#lang racket/base

(provide make-center-width
         make-center-percent
         center-interval
         percent-interval)

(require (only-in support average)
         (only-in "ex.2.07.rkt"
                  make-interval
                  lower-bound
                  upper-bound)
         (only-in "ex.2.09.rkt" width-interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center-interval i)
  (average (lower-bound i) (upper-bound i)))

(define (make-center-percent center percent)
  (make-center-width center (* center percent)))

(define (percent-interval interval)
  (/ (width-interval interval)
     (center-interval interval)))

(module+ test
  (require rackunit)
  (define interval (make-center-percent 100 0.1))

  (check-= (lower-bound interval) 90 1e-5)
  (check-= (upper-bound interval) 110 1e-5)
  (check-= (center-interval interval) 100 1e-5)
  (check-= (percent-interval interval) 0.1 1e-5))