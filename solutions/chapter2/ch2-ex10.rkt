#lang racket/base

(require "ch2-ex07.rkt")
(require (only-in "ch2-ex09.rkt" mul-interval))

(define (div-interval x y)
  (if (<= (lower-bound y) 0 (upper-bound y))
      (error "can not divide by a interval that spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(module+ test
  (require rackunit)

  (check-exn exn:fail? (lambda ()
                         (div-interval (make-interval 1 2)
                                       (make-interval -1 1)))))