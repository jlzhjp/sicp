#lang racket/base

(require "ch2-ex07.rkt")
(require "ch2-ex08.rkt")

(provide add-interval
         mul-interval
         div-interval
         width-interval)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))


(module+ test
  (require rackunit)

  (define x (make-interval 1 5))
  (define y (make-interval 3 7))

  (check-= (width-interval (add-interval x y))
           (+ (width-interval x) (width-interval y))
           0)

  (check-= (width-interval (sub-interval x y))
           (+ (width-interval x) (width-interval y))
           0))

(module+ main
  (displayln (width-interval (mul-interval (make-interval 1 3)
                                           (make-interval 2 4))))
  (displayln (width-interval (mul-interval (make-interval -1 1)
                                           (make-interval 2 4)))))