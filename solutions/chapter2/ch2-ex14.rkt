#lang racket/base

(require racket/format)
(require (only-in "ch2-ex07.rkt"
                  make-interval)
         (only-in "ch2-ex09.rkt"
                  add-interval
                  mul-interval
                  div-interval)
         (only-in "ch2-ex12.rkt"
                  make-center-percent
                  center-interval
                  percent-interval))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ([one (make-interval 1 1)])
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (interval->percentage-form x)
  (format "~a+-~a%"
          (~r (center-interval x) #:precision 2)
          (~r (* 100 (percent-interval x)) #:precision 2)))

(module+ test)

(module+ main
  (define r1 (make-center-percent 10 0.1))
  (define r2 (make-center-percent 20 0.1))
  (displayln (par1 r1 r2))
  (displayln (par2 r1 r2))
  #|
  (interval 4.909090909090909 8.962962962962962)
  (interval 6.0 7.333333333333334)
  |#
  (displayln (interval->percentage-form (div-interval r1 r1)))
  (displayln (interval->percentage-form (div-interval r2 r2)))
  #|
  1.02+-19.8% ; should be 1+-0
  1.02+-19.8% ; should be 1+-0
  |#)